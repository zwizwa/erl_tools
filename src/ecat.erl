%% Generalize the socat idea.  Implemented on top of epid.erl
-module(ecat).

-export([pty/1,tcp_listen/1,
         open_spec/1,cmd/1,
         
         start_link/1, handle/2,
         test/1]).

%% 1) PROCESS SPEC

%% A server process isn't necessary.  Just provide open_port shortcuts.
%% This flattening is not really necessary.
pty(Spec)        -> open_spec({pty,Spec}).
tcp_listen(Spec) -> open_spec({tcp_listen,Spec}).


open_spec(Spec) ->
    Cmd = cmd(Spec),
    log:info("cmd: ~s~n",[Cmd]),
    open_port({spawn, Cmd}, [use_stdio, binary, exit_status]).

%% One-ended socat mfa
socat(Fmt,Args) ->
    fmt("socat - " ++ Fmt, Args).
fmt(Fmt, Args) ->
    tools:format(Fmt, Args).

%% Allow for a specification language to name some typical setyps.
%% This doesn't need to be all socat, but it is necessary that
%% programs exit when stdin closes, which is not always the case with
%% everything!
cmd(Spec) ->
    case Spec of
        {pty, Link}          -> socat("PTY,link=~s,echo=0,raw", [Link]);
        {tcp_listen, Port}   -> socat("TCP-LISTEN:~p,reuseaddr", [Port]);
        {read_file, File}    -> fmt("cat '~s'",[File]);
        {write_file, File}   -> fmt("cat >'~s'",[File]);
        {dd_if, File}        -> fmt("dd 'if=~s' 2>/dev/null",[File]);
        {dd_of, File}        -> fmt("dd 'of=~s' 2>/dev/null",[File]);
        {pipe, [S]}          -> cmd(S);
        {pipe, [S|Ss]}       -> fmt("~s | ~s", [cmd(S), cmd({pipe, Ss})]);
        {cmd, Cmd}           -> Cmd
    end.


%% 2) EPID PROXY

%% Please note that there is no backpressure mechanism for ports.  If
%% source is faster than sink, and the data size is large, this might
%% cause problems as the data will be buffered in Erlang mailboxes.


start_link(Init) ->
    {ok,
     serv:start(
       {handler,
        fun() -> Init end,
        fun ?MODULE:handle/2})}.

%% Open port if we don't have it yet.
need_port(Spec, State) ->
    case maps:find({port, Spec}, State) of
        {ok, Port} ->
            {Port, State};
        error ->
            log:info("adding port ~p~n",[Spec]),
            Port = open_spec(Spec),
            {Port,
             maps:merge(
               State,
               %% Index is redundant, to allow fast lookup for data
               %% forwarding in both directions.
               #{{port, Spec} => Port,
                 {spec, Port} => Spec})}
    end.

%% When port or subscriber or router is removed, remove the
%% corresponding subscriber(s) or port.

remove_port(Port, State, _Reason) ->
    catch port_close(Port),
    Spec = maps:get({spec, Port}, State),
    log:info("removing port ~p because of ~p~n",[Spec,_Reason]),
    State1 = epid:unsubscribe_all(Spec, State),
    maps:remove({spec, Port},
    maps:remove({port, Spec}, State1)).

remove_subscriber(Spec, Epid, State) ->
    State1 = epid:unsubscribe(Spec, Epid, State),
    remove_stale_port(Spec, State1).

remove_stale_port(Spec, State) ->
    case epid:subscribers(Spec, State) of
        [] ->
            Port = maps:get({port, Spec}, State),
            remove_port(Port, State, stale);
        _ ->
            State
    end.

remove_stale_ports(State) ->
    lists:foldr(
      fun remove_stale_port/2,
      State,
      specs(State)).

specs(State) ->
    [Spec || {{port, Spec}, _Port} <- maps:to_list(State)].

            

handle(TopMsg, State) ->
    %% log:info("~p~n",[TopMsg]),
    case TopMsg of
        {_, dump} ->
            obj:handle(TopMsg, State);
        %% Reuse the multi-subscriber infrastructure from epid.erl as
        %% much as possible.  In addition we also need to keep track
        %% of Erlang port instances.
        {'DOWN',_,_,_,_} ->
            %% FIXME: This needs to garbage-collect also.
            State1 = epid:down(TopMsg, State),
            remove_stale_ports(State1);
        %% epid message router
        {epid_send, Spec, Msg} ->
            case Msg of

                %% Disconnect from infinite stream.  We support only
                %% one connection, so make sure it's the right one.
                {epid_unsubscribe, Epid} ->
                    remove_subscriber(Spec, Epid, State);

                {epid_unsubscribe2, Epid} ->
                    epid:send(Epid, {epid_unsubscribe, {epid,self(),Spec}}),
                    remove_subscriber(Spec, Epid, State);

                %% Any other case needs to have a live port.
                _ ->
                    {Port, State1} = need_port(Spec, State),
                    case Msg of
                        {epid_subscribe, Epid} ->
                            epid:subscribe(Spec, Epid, State1);

                        {epid_subscribe2, Epid} ->
                            %% Bi-directional connections require some
                            %% care: Make sure that the first message
                            %% we send to the other end is a subscribe
                            %% message.
                            epid:send(Epid, {epid_subscribe, {epid,self(),Spec}}),
                            epid:subscribe(Spec, Epid, State1);

                        %% Channel protocol
                        {data, Data} ->
                            port_command(Port, Data),
                            State1;
                        eof ->
                            remove_port(Port, State1, eof)
                    end
            end;
        %% port message router
        {Port, Msg} when is_port(Port) ->
            case maps:find({spec,Port}, State) of
                error ->
                    %% FIXME: This should not happen.
                    log:info("WARNING: ignoring: ~p~n",[TopMsg]),
                    State;
                {ok, Spec} ->
                    case Msg of
                        {data, Data} ->
                            epid:dispatch(Spec, {data,Data}, State),
                            State;
                        {exit_status, N}=Status ->
                            case N of 0->ok; _-> log:info("WARNING: ~p~n",[TopMsg]) end,
                            epid:dispatch(Spec, eof, State),
                            remove_port(Port, State, Status)
                    end
            end
    end.

            

                 
%%test(epid2) -> 
%%    exo:push(vybrid_img, kingston_sd);
     
test({epid1,N}) -> 
    epid:connect2(
      {epid, {ecat, 'exo@10.1.3.29'}, {read_file, "/tmp/test"}},
      {epid, {ecat, exo:to_node(N)}, {write_file, "/tmp/test"}}).




    


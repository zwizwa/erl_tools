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
        {read_file, File}    -> fmt("cat '~s'", [File]);
        {write_file, File}   -> fmt("cat >'~s'", [File]);
        {dd_if, File}        -> fmt("dd 'if=~s' 2>/dev/null", [File]);
        {dd_of, File}        -> fmt("dd 'of=~s' 2>/dev/null", [File]);
        {pipe, [S]}          -> cmd(S);
        {pipe, [S|Ss]}       -> fmt("~s | ~s", [cmd(S), cmd({pipe, Ss})]);
        {ssh,S1,UAH,S2}      -> fmt("~s | ssh '~s' ~s", [cmd(S1), UAH, run:shell_quote(cmd(S2))]);
        {cmd, Cmd}           -> Cmd
    end.




%% 2) EPID PROXY

%% Notes: This is intended for real-time event streams.  While using
%% epid:connect/2 for file transfers is possible, it is not ideal for
%% two reasons:
%%
%% - No flow control.  All data will likely be buffered in the mailbox
%%   of the target's router.
%%
%% - No privacy: the target node is accessible by anyone that knows
%%   its name, and techically anyone can insert events into the
%%   stream.
%%
%% Instead, use epid:transfer/2



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
        %% epid message router
        {epid_send, Spec, Msg} ->
            case Msg of

                %% Transfers are only implemented between two ecat
                %% ports.  We let the other end handle it since we
                %% need to provide a UserAtHost value anyway.
                {call, Pid, Ref, {epid_transfer, {epid,SrcPid,SrcSpec}}} ->
                    {ok, [{IP,_BC,_NM}|_]} = inet:getif(),
                    DstIP = type:encode({ip,IP}),
                    SrcPid ! {transfer_start, Pid, SrcSpec, Spec, DstIP, Ref},
                    State;

                %% Disconnect from infinite stream.  We support only
                %% one connection, so make sure it's the right one.
                {epid_unsubscribe, DstEpid} ->
                    remove_subscriber(Spec, DstEpid, State);

                {epid_unsubscribe_bidir, DstEpid} ->
                    _ = epid:send(DstEpid, {epid_unsubscribe, {epid,self(),Spec}}),
                    remove_subscriber(Spec, DstEpid, State);

                %% Any other case needs to have a live port.
                _ ->
                    {Port, State1} = need_port(Spec, State),
                    case Msg of
                        {epid_subscribe, DstEpid} ->
                            epid:subscribe(Spec, DstEpid, State1);

                        {epid_subscribe_bidir, DstEpid} ->
                            %% Guarantee that epid_subscribe arrives
                            %% before the first data message.
                            _ = epid:send(DstEpid, {epid_subscribe, {epid,self(),Spec}}),
                            epid:subscribe(Spec, DstEpid, State1);

                        %% Raw binary data
                        {data, Data} ->
                            port_command(Port, Data),
                            State1;
                        %% Support end of transfer.
                        eof ->
                            remove_port(Port, State1, eof)
                    end
            end;
        %% port message router
        {Port, Msg} when is_port(Port) ->
            case maps:find({spec,Port}, State) of
                %% Streaming ports
                {ok, Spec} ->
                    case Msg of
                        {data, Data} ->
                            epid:dispatch(Spec, {data,Data}, State),
                            State;
                        {exit_status, N}=Status ->
                            case N of 0->ok; _-> log:info("WARNING: ~p~n",[TopMsg]) end,
                            epid:dispatch(Spec, eof, State),
                            remove_port(Port, State, Status)
                    end;
                error ->
                    %% Transfer ports
                    case maps:find({transfer,Port}, State) of
                        {ok, {Pid, Ref, _Spec}} ->
                            %% The only thing that the port should do
                            %% is exit successfully.  Anything else is
                            %% an error.
                            Reply =
                                case Msg of
                                    {exit_status, 0} -> ok;
                                    Error -> {error, Error}
                                end,
                            catch port_close(Port),
                            %% log:info("transfer end: ~p~n",[Reply]),
                            Pid ! {Ref, Reply},
                            maps:remove({transfer,Port}, State);
                        error ->
                            %% FIXME: This should not happen.
                            log:info("WARNING: ignoring: ~p~n",[TopMsg]),
                            State
                    end
            end;

        %% internal machinery

        %% obj.erl
        {_, dump} ->
            obj:handle(TopMsg, State);

        %% Reuse the multi-subscriber infrastructure from epid.erl as
        %% much as possible.  In addition we also need to keep track
        %% of Erlang port instances.
        {'DOWN',_,_,_,_} ->
            %% FIXME: This needs to garbage-collect also.
            State1 = epid:down(TopMsg, State),
            remove_stale_ports(State1);

        %% Called by other end after receiving transfer message.
        {transfer_start, Pid, SrcSpec, DstSpec, DstIP, Ref} ->
            Spec = {ssh, SrcSpec, DstIP, DstSpec},
            %% FIXME:  Solve the quoting problem.
            %% log:info("transfer_start: ~s~n",[cmd(Spec)]),
            %% Port = open_spec({cmd, "sleep .8"}),
            Port = open_spec(Spec),
            maps:put({transfer, Port}, {Pid, Ref, Spec}, State)

    end.





     
%% test(exo1) -> 
%%     exo:push(vybrid_img, kingston_sd);            
test(exo1) ->
    exo:transfer(vybrid_img, kingston_sd);

test({transfer,N1,N2}) ->  %% ecat:test({transfer,29,20}).
    epid:transfer(
      {epid, {ecat, exo:to_node(N1)}, {read_file, "/tmp/test"}},
      {epid, {ecat, exo:to_node(N2)}, {write_file, "/tmp/test"}});

test({epid,N1,N2}) -> 
    epid:connect_bidir(
      {epid, {ecat, exo:to_node(N1)}, {read_file, "/tmp/test"}},
      {epid, {ecat, exo:to_node(N2)}, {write_file, "/tmp/test"}}).




    


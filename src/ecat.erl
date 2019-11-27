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

handle(TopMsg, State) ->
    case TopMsg of
        {_, dump} ->
            obj:handle(TopMsg, State);
        {cleanup, Spec} ->
            log:info("~p~n", [TopMsg]),
            Port = maps:get(Spec, State),
            maps:remove(Spec, maps:remove(Port, State));
        %% epid message router
        {epid_send, Spec, Msg} ->
            case Msg of
                %% Setup
                {epid_subscribe, Epid} ->
                    error = maps:find(Spec, State),  %% Do this better
                    Port = open_spec(Spec),
                    maps:merge(
                      State,
                      #{ Spec => Port,
                         Port => {Spec, Epid} });

                %% Disconnect from infinite stream.  We support only
                %% one connection, so make sure it's the right one.
                {epid_unsubscribe, Epid} ->
                    Port = maps:get(Spec, State),
                    {_, Epid} = maps:get(Port, State),
                    epid:send({epid,self(),Spec},eof),
                    State;

                %% Channel protocol
                {data, Data} ->
                    Port = maps:get(Spec, State),
                    port_command(Port, Data),
                    State;
                eof ->
                    Port = maps:get(Spec, State),
                    port_close(Port),
                    handle({cleanup, Spec}, State)
            end;
        %% port message router
        {Port, Msg} when is_port(Port) ->
            case maps:find(Port, State) of
                error ->
                    log:info("WARNING: ignoring: ~p~n",[TopMsg]),
                    State;
                {ok, {Spec, Epid}} ->
                    case Msg of
                        {data, Data} ->
                            epid:send(Epid, {data, Data}),
                            State;
                        {exit_status, N} ->
                            case N of 0->ok; _-> log:info("WARNING: ~p~n",[TopMsg]) end,
                            epid:send(Epid, eof),
                            handle({cleanup, Spec}, State)
                    end
            end
    end.

            

                 
test(epid2) -> 
    exo:push(vybrid_img, kingston_sd);
     
test(epid1) -> 
    epid:connect2(
      {epid, {ecat, 'exo@10.1.3.29'}, {read_file, "/tmp/test"}},
      {epid, {ecat, 'exo@10.1.3.19'}, {write_file, "/tmp/test"}}).




    


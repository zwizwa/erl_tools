-module(serv_tcp).
-export([init/2, init/4, init/5, handle/2, accept_loop/3]).
-include("debug.hrl").
%% TCP server with client registry.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to serv_tcp.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


%% `Connect': a serv:start prototype that handles a single client
%% connection, taking the socket as argument of the init module.

%% `Handle',`State': function that handles additional messages sent to
%% the server registry process, with its corresponding initial state.


init(Ports, Connect, Handle, State, Opts) ->
    %% This runs in the same process as the handler.
    process_flag(trap_exit, true),
    Registry = self(),  
    %% Create listening sockets and acceptors.
    LSocks = [case gen_tcp:listen(Port, Opts) of
                  {error, Reason} ->
                      tools:info("listen error: port ~p: ~p~n", [Port,Reason]),
                      exit(Reason);
                  {ok, LSock} ->
                      spawn_link(fun() -> accept_loop(LSock, Port, Registry) end),
                      LSock
              end || Port <- Ports],
    {fun(Key) ->
             maps:get(Key,
                      #{connect => Connect,
                        handle  => Handle,
                        lsocks  => LSocks})
     end,
     [], %% Connection Pids
     State}.
init(P,C,D,S) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    init(P,C,D,S,Opts).
init(P,C) ->
    Handle = fun(_, State) -> State end,
    init(P,C,Handle,[]).
    

%% FIXME: make accept loop reloadable as well.
accept_loop(LSock, Port, Registry) ->
    case gen_tcp:accept(LSock, ?RELOAD_TIMEOUT) of
        {error, timeout} ->
            serv_tcp:accept_loop(LSock, Port, Registry);
        {error, Reason} ->
            exit(Reason);
        {ok, Sock} -> 
            unlink(Sock),
            Registry ! {connect, Sock, Port},
            serv_tcp:accept_loop(LSock, Port, Registry)
    end.


handle(Msg, {Env,_,_}=S) ->
    {Pids, State} = handle_(Msg, S),
    {Env, Pids, State}.

handle_({connect, Sock, Port}, {Env, Pids, State}) ->
    Pid =
        case Env(connect) of
            %% Support handlers, which can be reloaded without
            %% breaking the connection (FIXME: once switched to
            %% passive sockets).
            {handler, Init, Handle} ->
                serv:start({handler,
                            fun() -> link(Sock), Init(Sock, Port) end,
                            Handle});
            %% Generic.  Will be killed on double reload.
            {body, Body} ->
                serv:start({body,
                            fun() -> link(Sock), Body(Sock, Port) end})
        end,
    {[Pid | Pids], State};
            

handle_({'EXIT', FromPid, Reason}, {Env, Pids, State}) ->
    case lists:member(FromPid, Pids) of
        true -> 
            tools:info("child exit(~p,~p)~n", [FromPid, Reason]),
            NextState = (Env(handle))({'EXIT', FromPid, Reason}, State),
            {Pids -- [FromPid], NextState};
        false -> 
            case Reason of
                normal ->
                    ignore;
                _ ->
                    tools:info("linked exit(~p,~p), shutting down~n",
                               [FromPid, Reason]),
                    self() ! shutdown
            end,
            {Pids, State}
    end;

handle_({apply, Fun}, {_Env, Pids, State}) ->
    Fun(Pids),
    {Pids, State};

handle_(shutdown, {Env, _Pids, _State}) ->
    [ gen_tcp:close(LSock) || LSock <- Env(lsocks) ],
    process_flag(trap_exit, false),
    exit(shutdown);

handle_(Msg, {Env, Pids, State}) ->
    {Pids, (Env(handle))(Msg, State)}.


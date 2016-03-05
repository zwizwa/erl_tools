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
    LSocks =
        [case gen_tcp:listen(Port, Opts) of
             {error, Reason} ->
                 tools:info("listen error: port ~p: ~p~n", [Port,Reason]),
                 exit(Reason);
             {ok, LSock} ->
                 self() ! {spawn_acceptor, LSock, Port},
                 LSock
         end || Port <- Ports],
    {#{registry => Registry,
       connect  => Connect,
       handle   => Handle,
       lsocks   => LSocks},
     [], %% Connection Pids
     State}.
init(P,C,D,S) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    init(P,C,D,S,Opts).
init(P,C) ->
    Handle = fun(_, State) -> State end,
    init(P,C,Handle,[]).
    

%% FIXME: make accept loop reloadable as well.
accept_loop(LSock, Port,
            #{connect  := Connect,
              registry := Registry
             }=Env) ->
    case gen_tcp:accept(LSock, ?RELOAD_TIMEOUT) of
        {error, timeout} -> serv_tcp:accept_loop(LSock, Port, Env);
        {error, Reason}  -> exit(Reason);
        {ok, Sock} ->
            %% Notify registry, then enter server loop.
            Registry ! {register_connect, self()},
            Registry ! {spawn_acceptor, LSock, Port},
            case Connect of
                {handler, Init, Handle} ->
                    serv:enter({handler, fun() -> Init(Sock, Port) end, Handle});
                {body, Body} ->
                    serv:enter({body, fun() -> Body(Sock, Port) end})
            end
    end.


handle(Msg, {Env,_,_}=S) ->
    {Pids, State} = handle_(Msg, S),
    {Env, Pids, State}.

handle_({spawn_acceptor, LSock, Port}, {Env, Pids, State}) ->
    serv:start({body, fun() -> accept_loop(LSock, Port, Env) end}),
    {Pids, State};

handle_({register_connect, Pid}, {_Env, Pids, State}) ->
    {[Pid | Pids], State};

handle_({'EXIT', FromPid, Reason}, {#{handle := Handle}=_Env, Pids, State}) ->
    case lists:member(FromPid, Pids) of
        true -> 
            tools:info("child exit(~p,~p)~n", [FromPid, Reason]),
            NextState = Handle({'EXIT', FromPid, Reason}, State),
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

handle_(shutdown, {#{lsocks := LSocks}=_Env, _Pids, _State}) ->
    [ gen_tcp:close(LSock) || LSock <- LSocks ],
    process_flag(trap_exit, false),
    exit(shutdown);

handle_(Msg, {#{handle := Handle}=_Env, Pids, State}) ->
    {Pids, Handle(Msg, State)}.


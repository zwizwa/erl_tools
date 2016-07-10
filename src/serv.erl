-module(serv).
-export([
         %% Broadcaster
         bc_start/0, 
         %% Hub with predicates
         hub_start/0, hub_handle/2, hub_add/3, hub_add/2, hub_send/2,
         %% Printer process
         info_start/0,
         %% Pid registry with GC
         pids_new/0, pids_add/2, pids_del/2, pids_foreach/2, pids_send/2,
         %% Simple server control
         up/2, down/1, daemon/3,
         %% Several spawning methods
         start/1,
         enter/1,

         %% Process registry to keep track of all serv-started processes.
         reg_start/0,

         %% Private, needed for reloading.
         bc_handle/2,
         reg_handle/2,
         receive_loop/2,
         periodic_loop/3

        ]).
-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).


%% Simple server tools - avoiding OTP boilerplate.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to serv.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0

%% Main problem faced when creating small servers:
%% - Cost of writing in OTP style outweigh the benefits
%% - Do need reliable code reload during development + simple restart on crash.

%% To handle code reload:
%% - Add an explicit reload message to each server loop
%% - Register each server process to a central monitor
%% - Allow reloading of the monitor


%% Set of Pids, with garbage collection on message send.
pids_new() ->
    sets:new().
pids_add(Pid, Pids) when is_pid(Pid) ->
    sets:add_element(Pid, Pids).
pids_del(Pid, Pids) when is_pid(Pid) ->
    sets:del_element(Pid, Pids).
pids_foreach(Fun, Pids) ->
    sets:fold(
      fun(Pid, Ps) ->
              case is_process_alive(Pid) of
                  true  -> Fun(Pid), Ps;
                  false -> sets:del_element(Pid, Ps)
              end
      end,
      Pids, Pids).
pids_send(Msg, Pids) ->
    pids_foreach(fun(Pid) -> Pid ! Msg end, Pids).

%% Broadcaster. FIXME: use gen_event
bc_start() -> spawn_handler(fun pids_new/0, fun serv:bc_handle/2).
bc_handle({subscribe,   Pid}, Pids) -> pids_add(Pid,Pids);
bc_handle({unsubscribe, Pid}, Pids) -> pids_del(Pid,Pids);
bc_handle({foreach,     Msg}, Pids) -> pids_foreach(Msg,Pids);
bc_handle({broadcast,   Msg}, Pids) -> pids_send(Msg,Pids).



%% Hub with predicates.
hub_init() ->
    [].
hub_cleanup(Hub) ->
    lists:filter(
      fun({_,Pid}) -> is_process_alive(Pid) end, Hub).
hub_handle({add, Pred, Pid}, Hub) ->
    [{Pred, Pid} | Hub];
hub_handle({send, Msg}, Hub) ->
    ?IF(lists:foldl(
          fun({Pred, Pid}, Alive) ->
                  ?IF(Pred(Msg), Pid ! Msg, ignore),
                  is_process_alive(Pid) and Alive
          end, true, Hub),
        Hub,
        %% Process doesn't allocate in happy path.  Update structure
        %% only when one or more processes have died.
        hub_cleanup(Hub)).
hub_start() ->
    start({handler, fun hub_init/0, fun serv:hub_handle/2}).
hub_add(HubPid, Pred, Pid) ->
    HubPid ! {add, Pred, Pid}.
hub_add(HubPid, Pid) ->
    HubPid ! {add, fun(_)->true end, Pid}.
hub_send(HubPid, Msg) ->
    HubPid ! {send, Msg}.


%% Print everything
info_start() ->
    start({handler,
           fun() -> [] end,
           fun(Msg,_) -> tools:info("~p~n", [Msg]), [] end}).


%% Process registry for all serv.erl started tasks. This is different
%% from a monitor as it doesn't own processes.  Monitor will start
%% this and register it as registry.

%% FIXME: untill all loops are broken (which requires switching to
%% active sockets), the reload functionality isn't fully functional.
%% Currently all servers use the debug timeout to cause reloads.
reg_start() ->
    spawn_link(fun() -> receive_loop(pids_new(), fun serv:reg_handle/2)end).
reg_handle(Msg, Pids) ->
    NewPids =
        case Msg of
            {apply, Sink} ->
                Sink(sets:to_list(Pids)), Pids;
            {subscribe, Pid} ->
                pids_add(Pid, Pids);
            reload ->
                pids_send(reload, Pids), Pids;
            Unknown ->
                exit(reg_unknown_message,Unknown)
        end,
    NewPids.


%% FIXME: this isn't used for anything.  Remove.
serv_reg(Pid) ->
    case whereis(serv_reg) of
        undefined -> 
            tools:info("no registry~n"),
            undefined;
        Registry -> 
            Registry ! {subscribe, Pid}
    end.

receive_loop(State, Handle) ->
    receive 
        reload  -> serv:receive_loop(State, Handle);
        Message -> serv:receive_loop(Handle(Message, State), Handle)
    end.



%% Idempotent start/stop                      
daemon(up,    N,S) -> up(N,S);
daemon(down,  N,_) -> down(N).
down(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> exit(Pid,down)
    end.
up(Name, Spawner) ->
    case whereis(Name) of
        undefined ->
            %%tools:info("spawn: ~p~n",[Name]),
            Pid = start(Spawner),
            tools:info("register: ~p ~p~n",[Pid,Name]),
            register(Name, Pid),
            Pid;
        Pid ->
            Pid
    end.
%% Generic starter, also for non-serv processes.
start({handler, Init, Handle}) ->
    start({spawner, fun() -> spawn_handler(Init, Handle) end});
start({periodic, Ms, Thunk}) ->
    start({spawner, fun() -> spawn_periodic(Ms, Thunk) end});
start({periodic, Ms, Init, Body}) ->
    start({spawner, fun() -> spawn_periodic(Ms, Init, Body) end});
start({body, Body}) ->
    start({spawner, fun() -> spawn_link(Body) end});
start({spawner, Spawn}) ->
    Spawn().


%% Take over current process.
enter({handler, Init, Handle}) ->
    enter_handler(Init(), Handle);
enter({body, Body}) ->
    Body().


%% Spawn a linked, serv_reg-registered process.
spawn_handler(Init, Handle) ->
    spawn_link(fun() -> enter_handler(Init(), Handle) end).
%% Enter its main loop.
enter_handler(State, Handle) ->
    serv_reg(self()),
    receive_loop(State,Handle).



spawn_periodic(Ms, Thunk) ->
    spawn_periodic(Ms,
                   fun() -> [] end,
                   fun(_) -> Thunk(), [] end).
spawn_periodic(Ms, Init, Body) ->
    Pid = spawn_link(fun() -> periodic_loop(Ms, Body, Init()) end),
    serv_reg(Pid),
    Pid.
periodic_loop(Ms, Body, State) ->
    NextState = Body(State),
    timer:sleep(Ms),
    serv:periodic_loop(Ms, Body, NextState).

    

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
         %% Rate limiting
         throttle/2,
         %% queue flush + synchronous call based batch processing
         batch_processor/1, flush/0, receive_batch/0,

         %% Private, needed for reloading.
         bc_handle/2,
         receive_loop/2,
         periodic_loop/3

        ]).

-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to serv.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0



%% Simple server tools - avoid OTP boilerplate.

%% To persist processes across reload, either:
%% - do not reload serv (more than once): processes are blocked in receive_loop/2
%% - send a reload message to serv processes after reloading serv


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
bc_start() ->
    spawn_handler(
      fun() -> 
              process_flag(trap_exit, true),
              pids_new()
      end,
      fun serv:bc_handle/2).

bc_handle({subscribe, Pid}, Pids) when is_pid(Pid) -> 
    link(Pid),
    pids_add(Pid,Pids);
bc_handle({subscribe, Atom}, Pids) when is_atom(Atom) -> 
    bc_handle({subscribe, whereis(Atom)}, Pids);

bc_handle({unsubscribe, Pid}, Pids) -> pids_del(Pid,Pids);
bc_handle({foreach,     Msg}, Pids) -> pids_foreach(Msg,Pids);
bc_handle({broadcast,   Msg}, Pids) -> pids_send(Msg,Pids);

bc_handle({'EXIT', Pid, _}, Pids) ->
    pids_del(Pid,Pids);

bc_handle(Msg, Pids) ->
    tools:info("WARNING: bc_handle: ~p~n",[{Msg,Pids}]),
    Pids.


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



%% Main loop shared by all processes.
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
    receive_loop(State,Handle).



spawn_periodic(Ms, Thunk) ->
    spawn_periodic(Ms,
                   fun() -> [] end,
                   fun(_) -> Thunk(), [] end).
spawn_periodic(Ms, Init, Body) ->
    spawn_link(fun() -> periodic_loop(Ms, Body, Init()) end).
periodic_loop(Ms, Body, State) ->
    NextState = Body(State),
    timer:sleep(Ms),
    serv:periodic_loop(Ms, Body, NextState).

    


%% Rate limiter.  Passes last message to Thunk.
throttle(Delay, Thunk) ->
    spawn_link(fun() -> throttle_idle(Delay, Thunk) end).
throttle_idle(Delay, Thunk) ->
    receive
        First ->
            Pid = self(),
            Ref = make_ref(),
            spawn_link(
              fun() ->
                      timer:sleep(Delay),
                      Pid ! Ref
              end),
            throttle_wait(Delay, Thunk, Ref, First)
    end.
throttle_wait(Delay, Thunk, Ref, Last) ->
    receive
        Ref ->
            Thunk(Last),
            throttle_idle(Delay, Thunk);
        New ->
            throttle_wait(Delay, Thunk, Ref, New)
    end.



%% Throttling mechanism based on a message flush and a batch
%% processing function.  To stop, the Process method can call exit
%% based on some stop message.
batch_processor(Process) ->
    spawn_link(fun() -> batch_loop(Process) end).
batch_loop(Process) ->
    Process(receive_batch()),
    batch_loop(Process).

%% Wait and flush
receive_batch() -> 
    flush_loop([receive Msg -> Msg end]).
flush() ->
    flush_loop([]).
flush_loop(Batch) ->
    receive Msg -> flush_loop([Msg|Batch])
    after 0 -> lists:reverse(Batch)
    end.
                               

             
                 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("serv.erl.expect").
expect_test() -> expect:run_form(?FILE ++ ".expect", fun serv_expect/0).
-endif.


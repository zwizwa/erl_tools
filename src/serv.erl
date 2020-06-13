%% (c) 2018 Tom Schouten -- see LICENSE file

-module(serv).
-export([
         %% Broadcaster
         bc_start/0,  bc_up/1,  bc_test/1,
         %% Printer process
         info_start/0,
         %% Pid registry with GC
         pids_new/0, pids_add/2, pids_del/2, pids_foreach/2, pids_send/2,
         %% Simple server control
         up/2, down/1, daemon/3, obj/2,
         %% Several spawning methods
         start/1,
         enter/1,
         %% For OTP supervisors
         supervisor_init/2,
         start_child/1,

         %% queue flush + synchronous call based batch processing
         batch_processor/2, flush/0, receive_batch/0,

         retain/2,

         %% Private, needed for reloading.
         bc_init/0, bc_handle/2,
         receive_loop/2,
         periodic_loop/3

        ]).

-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).
-export_types([start_spec/0]).


%% Simple server tools - avoid OTP boilerplate.

%% OTP is great.  Supervisors are great.  However, gen_server is
%% heavyweight, requiring one module per service type.  Often, that is
%% really overkill, and an anonymous init+handler is sufficient.
%% Combine this with obj.erl for object-style services on top of
%% serv.erl handlers.


%% To persist processes across reload, either:
%% - do not reload serv (more than once): processes are blocked in receive_loop/2
%% - send a reload message to serv processes after reloading serv



%% Set of Pids, with garbage collection on message send.
pids_new() ->
    #{}.
pids_add(Pid, Pids) when is_pid(Pid) ->
    pids_add(Pid, [], Pids).
pids_add(Pid, Filter, Pids) ->
    maps:put(Pid, Filter, Pids).
pids_del(Pid, Pids) when is_pid(Pid) ->
    maps:remove(Pid, Pids).
pids_subscribed(Pid, Pids) when is_pid(Pid) ->
    case maps:find(Pid, Pids) of
        {ok, _} -> true;
        _ -> false
    end.
pids_pids(Pids) ->
    maps:keys(Pids).

%% Non-filtered
pids_foreach(Fun, Pids) ->
    _ = maps:fold(
          fun(Pid, _Filter, Ps) ->
                  Fun(Pid), Ps end,
          Pids,
          Pids),
    ok.
pids_foreach_with_filter(Fun, Pids) ->
    _ = maps:fold(
          fun(Pid, Filter, Ps) ->
                  Fun(Pid, Filter), Ps end,
          Pids,
          Pids),
    ok.


%% This can be filtered.

%% FIXME: Filter spec is very ad hoc here:

%% - Empty list means no filter, because a filter that filters out
%%   everything isn't of much use.
%%
%% - Otherwise, a list is a list of tags for tagged pairs
%%
pids_send(Msg, Pids) ->
    pids_foreach_with_filter(
      fun(Pid, FilterSpec) ->
              case {Msg, FilterSpec} of
                  {_,[]} ->
                      Pid ! Msg;
                  {{Tag,_},Tags} when is_list(Tags) ->
                      case lists:member(Tag, Tags) of
                          true -> Pid ! Msg;
                          false -> ok
                      end;
                  {_, Pred} when is_function(Pred) ->
                      case Pred(Msg) of
                          true -> Pid ! Msg;
                          false -> ok
                      end;
                  _ ->
                      Pid ! Msg
              end
      end,
      Pids).


%% Broadcaster. FIXME: use gen_event?
bc_init() ->
    process_flag(trap_exit, true),
    #{pids => pids_new()}.
    
bc_spawner() ->
    {handler,
     fun serv:bc_init/0,
     fun serv:bc_handle/2}.
bc_start()  -> start(bc_spawner()).
bc_up(Name) -> up(Name, bc_spawner()).

bc_test(BC) ->
    BC ! {subscribe,
          start(
            {handler,
             fun() -> #{} end,
             fun(Msg,State) ->
                     log:info("~p~n", [Msg]),
                     State 
             end})}.

bc_handle({subscribe, Pid}, State) when is_pid(Pid) ->
    %% Unfiltered
    bc_handle({subscribe, {Pid,[]}}, State);
bc_handle({subscribe, {Pid, Filter}}, #{pids := Pids}=State) when is_pid(Pid) -> 
    case pids_subscribed(Pid, Pids) of
        true ->
            %% Don't re-monitor
            State;
        false ->
            _Ref = erlang:monitor(process, Pid),
            maps:put(pids, pids_add(Pid, Filter, Pids), State)
    end;
bc_handle({subscribe, Atom}, State) when is_atom(Atom) -> 
    case whereis(Atom) of
        undefined ->
            tools:info("WARNING: whereis(~p) == undefined~n", [Atom]),
            State;
        Pid ->
            bc_handle({subscribe, Pid}, State)
    end;
bc_handle({'EXIT',Pid,_Reason}=_Msg, #{pids := Pids}=State) ->
    maps:put(pids, pids_del(Pid,Pids), State);
bc_handle({'DOWN',_Ref,process,Pid,_Reason}=_Msg, #{pids := Pids}=State) ->
    %% tools:info("~p~n",[_Msg]),
    maps:put(pids, pids_del(Pid,Pids), State);
bc_handle({unsubscribe, Pid}, #{pids := Pids}=State) ->
    maps:put(pids, pids_del(Pid, Pids), State);
bc_handle({foreach,     Fun}, #{pids := Pids}=State) ->
    pids_foreach(Fun, Pids), State;
bc_handle({foreach_with_filter, Fun}, #{pids := Pids}=State) ->
    pids_foreach_with_filter(Fun, Pids), State;
bc_handle({broadcast,   Msg}, #{pids := Pids}=State) ->
    pids_send(Msg,Pids), State;
bc_handle({Pid,pids}, #{pids := Pids}=State) ->
    obj:reply(Pid, pids_pids(Pids)),
    State;
bc_handle(Msg, State) ->
    try
        obj:handle(Msg, State)
    catch
        C:E ->
            tools:info("WARNING: bc_handle: ~p~n",[{Msg,State,{C,E}}]),
            State
    end.

        




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

%% Useful for debug objects
obj(Name,Init) ->
    up(Name, {handler, fun() -> Init end, fun obj:handle/2}).
                                

%% Generic starter, also for non-serv processes.
-type start_spec() ::
        {handler, fun(() -> _), fun((_,_)->_)} |
        {periodic, integer(), fun(() -> _)} |
        {periodic, integer(), fun(() -> _), fun((_) -> _)} |
        {body, fun(() -> _)} |
        {spawner, fun(() -> pid())}.

-spec start(start_spec()) -> pid().

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


%% For use in supervisor child_spec().
start_child(Spec) ->
    try {ok, start(Spec)}
    catch C:E -> {error, {C,E}} end.

%% Simplified OTP superviser init function using some reasonable defaults.
%% - start supervisor as: supervisor:start_link(?MODULE, InitArgs),
%% - export an init function in your module that calls serv:sup_init/2
  
%% FIXME: Why is Modules needed?
supervisor_init(Modules, Specs) ->
    Restart = permanent,
    ShutDown = brutal_kill,
    Type = worker,
    ChildSpecs = 
        [{ID, 
          {serv, start_child, [StartSpec]},
          Restart,
          ShutDown,
          Type,
          Modules}
         || {ID, StartSpec} <- Specs],
    {ok, {{one_for_one, 5, 5}, ChildSpecs}}.





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

    

%% FIXME: Left here as an example of how not to do it.  Use
%% batch_processor/2 which has much cleaner semantics.

%% %% Rate limiter.  Passes last message to Thunk.
%% throttle(Delay, Thunk) ->
%%     spawn_link(fun() -> throttle_idle(Delay, Thunk) end).
%% throttle_idle(Delay, Thunk) ->
%%     receive
%%         First ->
%%             Pid = self(),
%%             Ref = make_ref(),
%%             spawn_link(
%%               fun() ->
%%                       timer:sleep(Delay),
%%                       Pid ! Ref
%%               end),
%%             throttle_wait(Delay, Thunk, Ref, First)
%%     end.
%% throttle_wait(Delay, Thunk, Ref, Last) ->
%%     receive
%%         Ref ->
%%             Thunk(Last),
%%             throttle_idle(Delay, Thunk);
%%         New ->
%%             throttle_wait(Delay, Thunk, Ref, New)
%%     end.



%% Throttling mechanism based on a message flush and a batch
%% processing function.  To stop, the Process method can call exit
%% based on some stop message.
batch_processor(Process, State) ->
    spawn_link(fun() -> batch_loop(Process, State) end).
batch_loop(Process, State) ->
    NewState = Process(receive_batch(), State),
    batch_loop(Process, NewState).

%% Wait and flush
receive_batch() -> 
    flush_loop([receive Msg -> Msg end]).
flush() ->
    flush_loop([]).
flush_loop(Batch) ->
    receive Msg -> flush_loop([Msg|Batch])
    after 0 -> lists:reverse(Batch)
    end.
                               


%% Retain data for a short while.  This can be used in SPA bootup to
%% pass data between initial page generation and web socket startup,
%% as those are normally only linked through the generated document.
%% Basically, pass the data by reference instead of serializing it to
%% a value.  A timeout is needed to discard the data after a while in
%% case data is not claimed.
retain(Data, Timeout) ->
    DataPid = spawn_link(
      fun() ->
         receive {Pid, get} -> Pid ! {self(), Data}
         after Timeout -> log:info("retain timeout~n") end
      end),
    retain_get(DataPid).
%% Separate function to avoid the Data to pollute the closure.
retain_get(DataPid) ->
    fun() ->
            %% Get retained data,
            DataPid ! {self(), get},
            receive {DataPid, Data} -> Data
            after 3000 -> throw(retain_get_timeout) end
    end.
             
                 

%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%% -include("../test/serv.expect").
%% expect_test() ->
%%     expect:run_form(
%%       filename:dirname(?FILE)++"/../test/serv.expect",
%%       fun serv_expect/0).
%% -endif.


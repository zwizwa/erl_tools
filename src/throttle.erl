-module(throttle).
-export([start_link/1, handle/2]).

%% The trigger message either starts a process if none is running.  If
%% a process is running the work item will be pushed to a stack.  Once
%% the worker is done and if more work items have been collected, a
%% _single_ new worker is started for those items.

%% There are 3 states:
%% idle   := not building and no requests
%% busy   := building and no more requests
%% again  := building and more requests pending

%% The worker gets passed all the requests that have come in since
%% last finish, most recent one first.

start_link(Spec = #{ start_worker := _}) ->
    {ok,serv:start(
          {handler,
           fun() -> maps:merge(Spec, #{ state => idle}) end,
           fun ?MODULE:handle/2})}.

handle({trigger, Arg}=_Msg, 
       State = #{ 
         state := S0,
         start_worker := StartWorker
        }) ->
    %% Refpid is {Ref, Pid} where Ref is monitor reference and Pid is
    %% the worker's process id.
    S1 = case S0 of
             idle                  -> {busy,  StartWorker([Arg])};
             {busy,  RefPid}       -> {again, RefPid, [Arg]};
             {again, RefPid, Args} -> {again, RefPid, [Arg|Args]}
         end,
    maps:put(state, S1, State);

handle({'DOWN',_Ref,process,_Pid,_Reason}=_Msg,
       State = #{ state := S0, start_worker := StartWorker }) ->
    %% log:info("done: ~p~n",[_Reason]),
    S1 = case S0 of
             {busy,  _}       -> idle;
             {again, _, Args} -> {busy, StartWorker(Args)}
         end,
    maps:put(state, S1, State);

handle(Msg, State) ->
    obj:handle(Msg, State).





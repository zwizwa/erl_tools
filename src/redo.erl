-module(redo).
-export([redo/2, need/2, get/2, set/3,
         start_link/1, handle_outer/2, handle/2, test/1]).


%% This is an Erlang implementation of a variant of djb's redo.
%% Inspired by apenwarr's blog posts.

%% Properties:
%%
%% - "pull" structure: all network inputs are polled
%%
%% - Can be thought of as functional reactive programming structured
%%   as a cache invalidation problem
%%
%% - Dependencies are generated programmatically as part of update
%%
%% - Imperative structure: nodes can be updated in-place in an
%%   abstract external store; system only provides synchronization.
%%
%% - Abstract operators: update functions can be operators on that
%%   abstract external store.
%%
%% - Transaction interface: each "pull" conceptually corresponds to a
%%   single moment in time.


%% So what is the trick here?  What makes this different from generic
%% FRP?  Dependency structure is input-dependent.  I.e. the value of
%% the inputs of an update script can change and as a consequence
%% change the list of dependencies on the fly, i.e. value influences
%% computation structure.  This is in stark contrast to computation
%% structure being fixed up front.

%% In addition, it is structured as a wrapper around an abstract
%% imperative update.  I.e. we do not deal with values directly inside
%% the Erlang code, we just provide an environment in which an
%% abstract imperative obdate is legal.


%% Interface.  See test(ex1) below.
redo(Pid, Product) ->
    obj:call(Pid, {redo, Product}, 6124).

%% Several calls against the evaluator will be performed during
%% evaluation of the network.  The task of this monitor process is to
%% maintain transaction semantics.
start_link(Spec) ->
    {ok, serv:start(
           {handler,
            fun() -> #{ eval => start_eval(Spec) } end,
            fun ?MODULE:handle_outer/2})}.
handle_outer(Msg, State = #{eval := Eval}) ->
    case Msg of
        {_, dump} -> 
            obj:handle(Msg, State);
        {Pid, {redo, Product}} ->
            ok = obj:call(Eval, start),
            Rv = obj:call(Eval, {eval, Product}),
            obj:reply(Pid, Rv),
            State
    end.




%% Evaluator maintains dependency table, store (optional), and keeps
%% track of which nodes have been checked in a single network run.
start_eval(Spec = #{ updates := Updates }) when is_map(Updates) ->
    serv:start(
      {handler,
       fun() -> Spec end,
       fun ?MODULE:handle/2}).

%% RetVal  Meaning
%% true    object has been rebuilt, output propagation needed
%% false   no change, no propagation needed
handle({CallPid, {eval, Product}},
       State = #{updates := Updates}) ->
    case maps:find({phase, Product}, State) of
        {ok, {changed, Changed}} ->
            %% Already evaluated as change/nochange in this evaluation
            %% phase.  Reply directly.
            debug("~p: already determined changed: ~p~n", [Product, Changed]),
            obj:reply(CallPid, Changed),
            State;
        {ok, {waiting, WaitList}} ->
            %% Product is not yet ready (dep check or update).  Join
            %% the wait list.
            debug("~p: already waiting~n", [Product]),
            maps:put({phase, Product}, {waiting, [CallPid|WaitList]}, State);
        error ->
            %% Not waiting nor checked.  Get the dependencies that
            %% were calculated in the last run.  Inputs and "thunks"
            %% are defined as update functions with no dependencies.
            Update = maps:get(Product, Updates),
            StatePid = self(),
            case maps:get({deps, Product}, State, []) of
                [] ->
                    %% First run (no recorded deps), or an input that
                    %% is externally managed and needs to be polled.
                    debug("~p: ext dep -> update~n", [Product]),
                    spawn_update(StatePid, Product, Update);
                Deps ->
                    %% Subsequent runs have deps, so check them first.
                    debug("~p: check deps~n", [Product]),
                    spawn_depcheck(StatePid, Product, Update, Deps)
            end,
            maps:put({phase, Product}, {waiting, [CallPid]}, State)
    end;

           
%% Worker is done computing the product.  Signal all waiters and save
%% the new dependency list.
handle({updated, Product, {Changed, NewDeps}}, State) ->
    case maps:find({phase, Product}, State) of
        {ok, {waiting, Waiters}} ->
            lists:foreach(
              fun(Waiter) -> obj:reply(Waiter, true) end,
              Waiters),
            debug("~p: changed=~p, deps=~p~n", [Product, Changed, NewDeps]),
            maps:merge(
              State,
              #{ {phase, Product} => {changed, Changed},
                 {deps, Product} => NewDeps})
    end;

%% Worker determined that there is nothing to do.  Signal all waiters.
handle({cached, Product}, State) ->
    case maps:find({phase, Product}, State) of
        {ok, {waiting, Waiters}} ->
            lists:foreach(
              fun(Waiter) -> obj:reply(Waiter, false) end,
              Waiters),
            debug("~p: no_change~n", [Product]),
            maps:merge(
              State,
              #{ {phase, Product} => {changed, false}})
    end;

%% Implement a store inside of the evaluator.  Note that we can just
%% as well use a completely abstract (remote/external) store and
%% update methods that operate on that store.
handle({Pid,{get,Tag}}, State) ->
    obj:reply(Pid, maps:get({product, Tag}, State)),
    State;
handle({Pid,{set,Tag,Val}}, State) ->
    obj:reply(Pid, ok),
    maps:put({product, Tag}, Val, State);

%% Prepare for a new run, cleaning up old cruft.  This will make sure
%% we propagate all the way to the inputs when checking dependencies.
handle({Pid, start}, State) ->
    obj:reply(Pid, ok),
    maps:filter(
      fun({phase, _}, _) -> false; (_,_) -> true end,
      State);

%% Allow raw get/set from protected context.
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State).


%% Updates run in their own process, and push the new dependency
%% list to the server.
spawn_update(ServPid, Product, Update) ->
    _Pid =
        spawn_link(
          fun() -> ServPid ! {updated, Product, Update(ServPid)} end),
    ok.

%% Dependency checks run in their own process foremost because they
%% run against the server process initiating more evals, but also
%% because the update needs to run in its own process.
spawn_depcheck(StatePid, Product, Update, Deps) ->
    _Pid = 
        spawn_link(
          fun() ->
                  %% Check deps in parallel. This causes need/2 to
                  %% propagate through the dependency chain, running
                  %% update tasks as necessary.  Note that the update
                  %% call below will run need/2 again, but at that
                  %% time evaluation will have finished.
                  NeedUpdate = need(StatePid, Deps),
                  debug("~p: need update: ~p~n", [Product, NeedUpdate]),
                  StatePid ! 
                      case NeedUpdate of
                          false -> {cached,  Product};
                          true  -> {updated, Product, Update(StatePid)}
                      end
          end),
    ok.

%% Also called "ifchange".
need(StatePid, Deps) ->
    lists:any(
      fun(Updated)-> Updated end,
      maps:values(
        tools:pmap(
          fun(Dep) -> obj:call(StatePid, {eval, Dep}) end,
          Deps))).

%% Store interface.  Currently this will store items inside the
%% evaluator state machine, but it can easily be replaced by any
%% abstract store.
%%
%% Calling this is only allowed from update tasks, which have get
%% access _after_ calling need, and set access to the associated
%% output tag during the execution of the updat task.  One way to look
%% at it is that the purpose of the system is to schedule update
%% scripts in such a way to properly guard imperative updates like
%% these.
%%
set(StatePid, Tag, Val) -> obj:call(StatePid, {set,Tag,Val}).
get(StatePid, Tag)      -> obj:call(StatePid, {get,Tag}).

debug(F,As) ->
    log:info(F,As).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(ex1) ->
    Updates = #{
      ex1 =>
          %% Inputs and thunks are the same.
          fun(Pid) ->
                  set(Pid, ex1, #{}),
                  {true, []}
          end,
      ex2 => 
          %% Update functions use the imperative redo style: after
          %% need/2, the dependencies are guaranteed to be ready, and
          %% the output node can be set at any time.  It's
          %% straightforward to embed pure functions in this
          %% framework.
          fun(Pid) ->
                  Deps = [ex1],
                  need(Pid, Deps),
                  set(Pid, ex2, #{}),
                  {true, Deps}
          end

     },
    Spec = #{ updates => Updates },
    {ok, Pid} = start_link(Spec),
    #{eval := Eval} = obj:dump(Pid),
    
    Changed1 = redo(Pid, ex2), log:info("state1: ~p~n", [obj:dump(Eval)]),
    Changed2 = redo(Pid, ex2), log:info("state2: ~p~n", [obj:dump(Eval)]),
    unlink(Pid),
    exit(Pid, kill),
    {Changed1,Changed2};
    

test(Spec) ->
    throw({?MODULE,{test,Spec}}).




-module(redo).
-export([pull/2, get/2, with_eval/2,
         make_var/1, make_var/2, make_var/3,
         push/3, push/2,
         start_link/1, handle_outer/2, handle/2,
         file_changed/3,
         from_list_dir/2,
         read_file/2, write_file/3, is_regular/2,
         from_filename/1, to_filename/1, to_directory/1, to_includes/1,
         path_find/4,
         update_using/2, update_file/1,
         update_value/3, update_pure/3, update_const/2,
         update_case/3,
         need/2, changed/2,
         stamp/3, stamp_hash/2,
         run/2, run/3,
         gcc_deps/1, need_gcc_deps/2,
         import/1,
         need_val/2, find_val/2, put_val/3,
         no_update/1,
         default_script_log/1,
         test/1, u1/1, u2/1]).


%% This is an Erlang implementation of a variant of djb's redo.
%% Inspired by apenwarr's blog posts.  It has been generalized a
%% number of times as new insights appeared.

%% Properties:
%%
%% - It is a reactive "pull" structure: a dependency graph is
%%   constructed as part of the first evaluation.
%%
%% - This is Functional Reactive Programming operating on abstract
%%   references, which allows embedding of abstract, imperative
%%   operations on an abstract store.  The system only provides
%%   synchronization.
%%
%% - Transaction interface: each "pull" conceptually corresponds to a
%%   single moment in time.
%%
%%

%% Additional remarks:
%%
%% - There is a "push" interface available once the network has
%%   executed once.
%%
%% - Rule quantification is implemented through Erlang pattern
%%   matching on var names.  Very convenient in practice.
%%
%% - Concrete opaque rules are supported to provide an "applicative"
%%   interface as an alternative to the "arrow" interface.


%% So what is the trick here?  What makes the redo approach different
%% from generic FRP?  Redo is aptly named: it produces dependency
%% information as part of the first "do".
%%
%% This dependency structure is then updated on every product update.
%% This allows things like one dependency being a description of a
%% list of dependencies.  Or more succinctly: values can influence
%% dependency network struture structure.
%%
%% In addition, it is structured as a wrapper around an abstract
%% imperative update.  I.e. we do not deal with values directly inside
%% the Erlang code, we just provide an environment in which an
%% abstract imperative obdate is legal.


%% FIXME: Allow for a "window" timestamp to be included at startup.
%% E.g. don't take into account anything older than that.
%% Alternatively, save the timestamps somewhere, or derive them from
%% products.
%%
%% FIXME: Partial rebuilds don't work properly, e.g. if you have A->B
%% and you redo A, then B doesn't build when you redo B after.  It
%% seems that stamps would need to be stored associated to the rule,
%% not in general.


debug(_F,_As) ->
    %% log:info(_F,_As),
    ok.

%% -------- CORE

%% Main entry point.  See test(ex1) below.
pull(Redo, Products) ->
    with_eval(
      Redo,
      fun(Eval) ->
              %% FIXME: Put this elsewhere?
              Log =
                  case obj:call(Eval, script_log) of
                      {ok, L} -> L;
                      _ -> fun(_) -> ok end
                  end,
              Log(clear),
              Rv =need(Eval, Products),
              %% Printing products is too verbose.
              %% Log({line,tools:format("pull: ~p~n~p",[Rv,Products])}),
              Log({line,tools:format("pull: ~p",[Rv])}),
              Rv
      end).

%% Trigger update of dependent products from external change notification
push(Redo, Changed, NotChanged) ->
    obj:call(Redo, {push, Changed, NotChanged}, infinity).
%% Providing NotChanged is just an optimization.  Not filling those in
%% will let the network determine change statuse.
push(Redo, Changed) ->
    push(Redo, Changed, []).

%% Run evaluations against a network.
with_eval(Redo, Fun) ->
    obj:call(Redo, {with_eval, Fun}).

%% Similar, but install a new opaque var using a function.
make_var(Redo) ->
    make_var(Redo, undefined).
make_var(Redo, Fun) ->
    make_var(Redo, Fun, '_NEW_').
make_var(Redo, Fun, VarTag) ->
    obj:call(Redo, {make_var, Fun, VarTag}).

%% For products that are Erlang values, update and return the value.
get(Redo, Product) ->
    with_eval(Redo, fun(Eval) -> need_val(Eval, Product) end).


%% Several calls against the evaluator will be performed during
%% evaluation of the network.  The task of this monitor process is to
%% maintain transaction semantics.
start_link(Spec0) when is_map(Spec0)  ->
    {ok, serv:start(
           {handler,
            fun() ->
                    Spec = load_config(Spec0),
                    #{ eval => start_eval(Spec) } 
            end,
            fun ?MODULE:handle_outer/2})}.


handle_outer(Msg, State = #{eval := Eval}) ->
    %% log:info("handle_outer: ~p~n",[Msg]),
    case Msg of
        {Pid, state} ->
            obj:reply(Pid, obj:dump(Eval)),
            State;
        {_, dump} -> 
            obj:handle(Msg, State);
        {Pid, reload} ->
            obj:reply(Pid, obj:call(Eval, reload)),
            State;
        {Pid, {make_var, Fun, MaybeVar}} ->
            obj:reply(Pid, obj:call(Eval, {make_var, Fun, MaybeVar})),
            State;
        {Pid, {with_eval, Fun}} ->
            obj:call(Eval, reload),
            ok = obj:call(Eval, start_pull),
            obj:reply(Pid, try Fun(Eval) catch C:E -> {error,{C,E}} end),
            State;
        {Pid, {push, Changed, NotChanged}} when 
              is_list(Changed) and 
              is_list(NotChanged)->
            %% Push is added as an optimization avoid to traversing
            %% the whole tree.  This can be beneficial in the case
            %% that change polling is expensive.
            %%
            %% Pusher can supply information about changed/nochanged.
            %% Any missing information in NotChanged is polled from
            %% stamp info.  Note that any changed items not in Changed
            %% will not cause recomputation unless they are
            %% accidentally part of some other evaluation path
            %% affected by the transitive closure.
            %%
            obj:call(Eval, reload),
            ok = obj:call(Eval, {start_push, Changed, NotChanged}),
            TC = obj:call(Eval, {transitive_closure, Changed}),
            debug("TC: ~p~n", [TC]),
            obj:reply(Pid, try need(Eval, TC) catch C:E -> {error,{C,E}} end),
            State
    end.

with_redo(Spec, Fun) ->
    {ok, Pid} = start_link(Spec),
    Rv = Fun(Pid),
    unlink(Pid),
    exit(Pid, kill),
    Rv.




%% Evaluator maintains dependency table, store (optional), and keeps
%% track of which vars have been checked in a single network run.
start_eval(Spec) when is_map(Spec) ->
    serv:start(
      {handler,
       fun() -> log:set_info_name({redo,eval}), Spec end,
       fun ?MODULE:handle/2}).

%% RetVal  Meaning
%% true    object has been rebuilt, output propagation needed
%% false   no change, no propagation needed
handle({CallPid, {eval, Product}}, State) ->
    case maps:find({phase, Product}, State) of
        {ok, {changed, Changed}} ->
            %% Already evaluated as change/nochange in this evaluation
            %% phase.  Reply directly.
            debug("~p: already determined changed: ~p~n", [Product, Changed]),
            obj:reply(CallPid, Changed),
            State;
        {ok, {waiting, WaitList}} ->
            %% Product is not yet ready (dep check or update).  Join
            %% the wait list that is notified on 'done'.
            debug("~p: already waiting~n", [Product]),
            maps:put({phase, Product}, {waiting, [CallPid|WaitList]}, State);
        error ->
            %% Not waiting nor checked.  Get the dependencies that
            %% were calculated in the last run and start a producer
            %% task.  Note: if there are no deps or if the deps list
            %% is empty, we will allways poll the update function to
            %% decide what to do.
            try
                Deps = maps:get({deps, Product}, State, []),
                Eval = self(),
                UpdateFun =
                    %% Two kinds: dynamically created opaque vars or
                    %% user-provided named vars.
                    case Product of
                        {var,_} ->
                            maps:get(
                              Product, 
                              State,
                              fun(_) ->
                                      log:info("stale reference ~p~n",[Product]),
                                      true
                              end);
                        _ ->
                            ToUpdateFun = 
                                maps:get(
                                  update, State,
                                  fun ?MODULE:no_update/1),
                            ToUpdateFun(Product)
                    end,
                Worker =
                    spawn_link(
                      fun() -> 
                              log:set_info_name({redo,Product}),
                              worker(Eval, Product, UpdateFun, Deps) end
                     ),
                maps:merge(
                  State,
                  #{{phase, Product} => {waiting, [CallPid]},
                    {need, Worker} => #{}})
            catch
                C:E ->
                    %% Don't bring down redo when the update function
                    %% crashes.  Print a warning instead.  Solve this
                    %% properly later.
                    log:info("WARNING: UpdateFun failed:~n~p:~n~p~n~p~n", 
                             [Product, {C,E}, erlang:get_stacktrace()]),
                    maps:put({phase, Product}, {changed, error}, State),
                    obj:reply(CallPid, error),
                    State
            end
    end;

%% Every need/2 call registers what was needed by user.
handle({Pid, {worker_needs, Deps}}, State) ->
    obj:reply(Pid, ok),
    case maps:find({need, Pid}, State) of
        {ok, OldDeps} ->
            maps:put(
              {need, Pid},
              lists:foldr(
                fun(D, Ds) -> maps:put(D, true, Ds) end,
                OldDeps, Deps),
              State);
        error ->
            %% This happens e.g. for with_eval call in monitor.
            debug("worker_needs: not a worker ~p~n", [Pid]),
            State
    end;
handle({Pid, worker_deps}, State) ->
    case maps:find({need,Pid}, State) of
        {ok, DepMap} ->
            obj:reply(
              Pid,
              lists:sort(maps:keys(DepMap))),
            maps:remove({need, Pid}, State)
    end;

%% Insert update computed by worker and notify waiters.
handle({worker_done, Product, PhaseUpdate}=Msg, State0) ->
    debug("worker_done: ~p~n", [{Product,PhaseUpdate}]),

    %% Update reverse table if deps changed.
    State = 
        update_reverse_deps(
          Product,
          maps:find({deps, Product}, PhaseUpdate),
          State0),
  
    %% Wake up everyone that is waiting for this product.
    {changed, Changed} = maps:get({phase, Product}, PhaseUpdate),
    case maps:find({phase, Product}, State) of
        {ok, {waiting, Waiters}} ->
            lists:foreach(
              fun(Waiter) -> obj:reply(Waiter, Changed) end,
              Waiters),
            maps:merge(State, PhaseUpdate);
        OtherPhase ->
            throw({worker_done_phase_error, Msg, OtherPhase})
    end;

%% Time stamp, hashes, ...
handle({Pid,{set_stamp,Tag,New}}, State) ->
    MaybeOld = maps:find({stamp, Tag}, State),
    obj:reply(Pid, MaybeOld),
    maps:put({stamp, Tag}, New, State);

%% Implement a store inside of the evaluator.  Note that we can just
%% as well use a completely abstract (remote/external) store and
%% update methods that operate on that store.
handle({Pid,{find_val,Tag}}, State) ->
    obj:reply(Pid, maps:find({val, Tag}, State)),
    State;
handle({Pid,{put_val,Tag,Val}}, State) ->
    obj:reply(Pid, ok),
    maps:put({val, Tag}, Val, State);

%% We provide file access relative to a "working directory" shared by
%% all update scripts.  Keep this abstract so it is easy to change
%% later.
handle({Pid,{find_file,Path}}, State = #{dir := Dir}) ->
    RvPath =
        case Path of
            [$/|_]=AbsPath -> AbsPath;
            _ -> tools:format("~s/~s",[Dir,Path])
        end,
    obj:reply(Pid, {ok, RvPath}),
    State;

    

%% Prepare for a new pull run, cleaning up old cruft.  This will make
%% sure we propagate all the way to the inputs when checking
%% dependencies.
handle({Pid, start_pull}, State) ->
    obj:reply(Pid, ok),
    maps:filter(
      fun({phase, _}, _) -> false;
         (log, _) -> false;
         (_,_) -> true end,
      State);

%% Same, but for a push run.  In this case we know what changed and
%% what didn't.  Note that this only makes sense when there are deps,
%% e.g. after a pull run.  Note that NotChanged could just contain all
%% inputs, as we overwrite Changed set later.

handle({Pid, {start_push, Changed, NotChanged}}, State0) ->
    obj:reply(Pid, ok),
    State1 =
        %% Start of a new phase: remove all old phase information.
        maps:filter(
          fun({phase, _}, _) -> false; (_,_) -> true end,
          State0),
    State2 =
        %% Use information coming from the outside regarding unchanged
        %% inputs.  This prunes the evaluation tree.
        lists:foldr(
          fun(NC, S) -> maps:put({phase,NC}, {changed, false}, S) end,
          State1, NotChanged),
    State3 =
        %% Same for known changes.  This avoids a poll.
        %% FIXME: poll is probably necessary to update stamps!
        lists:foldr(
          fun(C, S) -> maps:put({phase,C}, {changed, true}, S) end,
          State2, Changed),
    State3;


%% Compute the transitive closure of the changed set.
handle({Pid, {transitive_closure, Changed}}, State) ->
    obj:reply(Pid, transitive_closure(Changed, State)),
    State;


%% Create a new var
handle({Pid, {make_var,Fun,VarTag}}, State) ->
    case VarTag of
        '_NEW_' ->
            N = maps:get(next_var, State, 0),
            obj:reply(Pid, {var, N}),
            maps:merge(
              State,
              #{ {var, N} => Fun,
                 next_var => N+1 });
        _ ->
            obj:reply(Pid, {var, VarTag}),
            case is_pid(VarTag) of
                true -> erlang:monitor(process, VarTag);
                false -> ok
            end,
            maps:merge(
              State,
              #{ {var, VarTag} => Fun })
    end;    

%% FIXME: This cleans up cases where a {var,Pid} is an output var,
%% e.g. removes the var, the deps of that var, and the corresponding
%% rdeps.  It does not work very well if some other var started
%% depending on that var this would need more effort to not leak in
%% those cases.  We at least clean up the rdep so push will not wake
%% up any of those vars, and pull will then likely cause an error
%% once the var is gone.
handle({'DOWN', _Ref, process, Pid, Reason}, State) ->
    Var = {var, Pid},
    log:info("removing ~p: ~p ~n", [Var,Reason]),
    Deps = maps:get({deps, {var, Pid}}, State),
    %% remove 4 things:
    %% var, deps, var's rdeps, and remove var from dep's rdeps
    State1 = maps:remove(Var, State),
    State2 = maps:remove({deps, Var}, State1),
    State3 = maps:remove({rdeps, Var}, State2),
    lists:foldl(remove_rdeps(Var),State3,Deps);

%% Asynchronous log message.
handle({log_append,Item}, State) ->
    maps:put(log, [Item|maps:get(log, State, [])], State);


handle({Pid, script_log}, State) ->
    obj:reply(Pid, maps:find(script_log, State)),
    State;

%% Allow some read only access for internal use.
handle({_,{find,_}}=Msg, State) -> obj:handle(Msg, State);
handle({_,dump}=Msg, State)     -> obj:handle(Msg, State);

%% Reload do file
handle({Pid, reload}, State) -> 
    obj:reply(Pid, ok),
    load_config(State).

no_update(_Target) ->
    fun(_Eval) ->
            log:info("WARNING: no update/1 function defined~"),
            error
    end.

%% Update the reverse dependency structure.  This is done only when
%% the dependency list changes, which makes it fairly cheap to
%% maintain the inverse structure to implement push.  Note that this
%% is not a transitive closure, so push will need to traverse the
%% inverse graph until it iterminates.
update_reverse_deps(_, error, State) ->
    State;
update_reverse_deps(Product, {ok, Deps}, State0) ->
    OldDeps = maps:get({deps, Product}, State0, []),
    ToRemove = lists:subtract(OldDeps, Deps),
    ToAdd    = lists:subtract(Deps, OldDeps),
    State1   = lists:foldr(remove_rdeps(Product), State0, ToRemove),
    State2   = lists:foldr(add_rdeps(Product), State1, ToAdd),
    State2.

remove_rdeps(Product) ->
    fun(Dep, S) ->
            RDeps = maps:get({rdeps, Dep}, S, []),
            maps:put({rdeps, Dep}, lists:delete(Product, RDeps), S)
    end.

add_rdeps(Product) ->
    fun(Dep, S) ->
            RDeps = maps:get({rdeps, Dep}, S, []),
            maps:put({rdeps, Dep}, [Product|RDeps], S)
    end.
    

transitive_closure(Inputs, State) ->
    %% Recurse over leaf vars that do not have dependencies.
    Closure = 
        lists:foldl(
          fun(Var, Outputs) -> add_outputs(Var, Outputs, State) end,
          #{}, Inputs),
    %% Remove inputs that are also temrinals, e.g. no deps.
    maps:keys(
      lists:foldr(
        fun maps:remove/2,
        Closure, Inputs)).
add_outputs(Var, Outputs, State) ->
    case maps:get({rdeps,Var}, State, []) of
        [] ->
            %% Terminal var.
            maps:put(Var, true, Outputs);
        Vars ->
            %% Non-terminal var, keep looking.
            lists:foldl(
              fun(N, Os) -> add_outputs(N, Os, State) end,
              Outputs, Vars)
    end.


load_config(State) ->
    Spec =
        case maps:find(config, State) of
            {ok, {mfa, {M,F,A}}} -> apply(M,F,A);
            {ok,  {file, File}}  -> import(File);
            _ -> #{}
        end,
    maps:merge(State, Spec).


%% Worker process responsible of producing the target.
worker(Eval, Product, Update, OldDeps) ->
    %% need/2 will cause parallel eval of the dependencies.
    Affected =
        case OldDeps of
            [] -> true; %% First run or polling
            _  -> need(Eval, OldDeps)
        end,

    debug("~p: need update: ~p~n", [Product, Affected]),
    %% It's simplest to compute the state update here.
    PhaseUpdate =
        case Affected of
            true ->
                %% Dont crash the daemon in the common case that a
                %% build rule has a runtime or type error.  Propagate
                %% 'error' instead.
                {Changed, NewDeps} =
                    try
                        %% app/2 allows for "reloadable closures".
                        Ch = app(Update,[Eval]),
                        case Ch of
                            false -> ok;
                            true  -> log:info("changed~n");
                            error -> log:info("error~n");
                            _ -> throw({bad_update_rv, Update, Ch})
                        end,
                        {Ch, obj:call(Eval, worker_deps)}
                    catch C:E ->
                            ST = erlang:get_stacktrace(),
                            log:info("WARNING: update failed:~n~p~n~p~n", 
                                     [{C,E},ST]),
                            {error,[]}
                    end,
                %% Special-case the happy path.  Keeping track of dep
                %% graph changes allows caching the inverted dep graph
                %% for "push" operation and allows to prune a need/2
                %% call to stamp the new deps.

                %% FIXME: To better implement "push": change the
                %% (open) inverted dependencies accordingly when the
                %% dependencies change, but don't compute the
                %% transitive closure.  Do that on "push".

                case NewDeps == OldDeps of
                    true ->
                        #{{phase, Product} => {changed, Changed}};
                    false ->
                        debug("worker: deps changed: ~p~n", [{OldDeps,NewDeps}]),
                        %% _ = need(Eval, NewDeps), %% FIXME: This is no longer necessary
                        #{{phase, Product} => {changed, Changed},
                          {deps,  Product} => NewDeps}
                end;
            %% false, error
            _ -> 
                #{ {phase, Product} => {changed, Affected} }
                
                
        end,
    Eval ! {worker_done, Product, PhaseUpdate},
    ok.


             


%% Any error -> error, any change -> true.
any_changed(ChangeList) ->
    case lists:member(error, ChangeList) of
        true  -> error;
        false -> lists:member(true, ChangeList)
    end.

%% Also called "ifchange".
need(Eval, Deps) ->
    Prod2Changed = changed(Eval, Deps),
    Changed = any_changed(maps:values(Prod2Changed)),
    debug("need: ~p~n",[{Changed,Prod2Changed}]),
    Changed.

%% Same as need, but give more detailed information.  Can be used for
%% smart update functions.
%%
%% FIXME: There is already one worker process per product, so this
%% pmap isn't really necessary.  It could just as well be something
%% that sends out the call messages, then waits for all replies.  That
%% would cut the number of processes in half.
%%
changed(Eval, Deps) ->
    ok = obj:call(Eval, {worker_needs, Deps}),
    tools:pmap(
      fun(P) -> obj:call(Eval, {eval, P}) end,
      Deps).


%% Provide dataflow variables with values stored in the evaluator.
%%
%% Here 'find' behaves as ordinary find, e.g. it can fail, but it also
%% calls 'need', so all variables will have to be dataflow variables.
%% In practice this is always what you want.

find_val(Eval, Tag) ->
    obj:call(Eval, {find_val, Tag}).
get_val(Eval, Tag) ->
    case find_val(Eval, Tag) of
        {ok, Val} -> Val;
        error -> throw({get_val_not_found, Tag})
    end.
need_val(Eval, Tag) ->
    case need(Eval, [Tag]) of
        error -> throw({need_val_error, Tag});
        _ -> get_val(Eval, Tag)
    end.
put_val(Eval, Tag, Val) ->
    %% log:info("put_val ~p=~p~n", [Tag, Val]),
    obj:call(Eval, {put_val,Tag,Val}).




%% Import specs and build rules
import(Path) ->
    debug("importing from ~s~n", Path),
    reflection:run_module(Path, do, []).


%% -------- DYNAMIC NETWORK

%% Given an update function, insert it into the network and return an
%% named var.  This requires some extra infrastructure.




%% -------- LIB FOR SCRIPTS

%% Special kinds of update routines

update_using(Deps, Body) ->
    fun(Redo) -> redo:need(Redo, Deps), Body(Redo), true end.

update_file(Target) ->
    fun(Eval) ->
            File = redo:to_filename(Target),
            debug("c: ~p~n", [Target]),
            redo:file_changed(Eval, Target, File)
    end.

%% Update function that produces an Erlang value, but can use
%% arbitrary references as inputs.
update_value(Target, Deps, Body) ->
    fun(Eval) ->
            need(Eval, Deps),
            NewVal = Body(Eval),
            case find_val(Eval, Target) of
                {ok, NewVal} ->
                    false;
                _ ->
                    put_val(Eval, Target, NewVal),
                    true
            end
    end.

%% Similar, inputs are also variables.  This brings pure Erlang
%% functions into the mix.
update_pure(Target, Deps, PureBody) ->
    update_value(
      Target, Deps,
      fun(Eval) -> PureBody([get_val(Eval, Dep) || Dep <- Deps]) end).

update_const(Target, Val) ->
    update_pure(Target, [], fun([]) -> Val end).


%% Dispatch contained in a map, to allow for proper rebuild.
update_case(Target, Config, DefaultUpdate) ->
    fun(Eval) ->
            Cases = redo:need_val(Eval, Config),
            log:info("Cases=~p~n",[Cases]),
            Update = maps:get(Target, Cases, DefaultUpdate),
            (Update(Target))(Eval)
    end.

                                    

%% How to support anonymous targets?  Suppose we do not have a list of
%% outputs, but we have an opaque function that produces a set of
%% targets.  I would want to add functions on the fly.

%% FIXME


%% Allow for "reloadable closures".
app(F,Args) when is_function(F) ->
    apply(F,Args);
app({F,CArgs},Args) when is_function(F) ->
    apply(F,CArgs++Args).
            
            

            
    


%% Note that the redo core does not know about:
%% - the file system
%% - shell commands

%% Traditional "build system" functionality is built using these
%% interface routines.



%% Files and scripts

%% Convert between Erlang data type that is easy to pattern match"
%% {Type,BaseName,ReversePath}, and a "dotted list" file name
%% encoding.  Type can be a tuple to allow for composite types.
from_filename(IOList) ->
    %% debug("from_filename: ~s~n", [IOList]),
    [FileName|RPath] = lists:reverse(re:split(IOList,"/")),
    [BaseName|BinDotNames] = re:split(FileName,"\\."),
    %% debug("BinDotNames ~p~n",[BinDotNames]),
    DotNames =  [type:decode({pterm,Bin}) || Bin <- BinDotNames],
    {case DotNames of
         [DotName] -> DotName;
         _ -> list_to_tuple(lists:reverse(DotNames))
     end,
     BaseName,RPath}.

%% E.g. {c,<<"dht11">>=BaseName,[]=Path}
%% Symbol tags can be multiple.
to_directory(Dirs) ->
    tools:format("~s",[[[Dir,"/"] || Dir <- lists:reverse(Dirs)]]).
to_filename({Type,BaseName,Dirs}) ->
    Tags = case {Type,is_tuple(Type)} of
               {'',_} -> [];
               {_,true} -> lists:reverse(tuple_to_list(Type));
               {_,false} -> [Type]
           end,
    Path = to_directory(Dirs),
    Ext  = [[".",type:encode({pterm,Tag})] || Tag <- Tags],
    tools:format("~s",[[Path,BaseName,Ext]]).

to_includes(Paths) ->
    [[" -I",to_directory(P)] || P <- Paths].

%% FIXME: This probably doesn't work with spaces in names.  Actually
%% think about this and clean it up.
gcc_deps(Bin0) ->
    Bin1 = re:replace(Bin0, "^.*: ", ""),
    Bin2 = re:replace(Bin1, "\\n$", ""),
    Bin3 = re:replace(Bin2, " ?\\\\\n", "", [global]),
    List = lists:filter(
             fun(<<>>) -> false; (_) -> true end,
             re:split(Bin3, " +")),
    %% log:info("gcc_deps: ~p~n",[List]),
    lists:map(fun from_filename/1, List).

%% A need wrapper for the above.
need_gcc_deps(Eval, {_Type,_BaseName,_Path}=D) ->
    DepsFile = to_filename(D),
    {ok, DepsBin} = read_file(Eval, DepsFile),
    Deps = gcc_deps(DepsBin),
    need(Eval,Deps).



with_abs_path(Eval, RelPath, Fun) ->
    {ok, AbsPath} = obj:call(Eval, {find_file, RelPath}),
    Fun(AbsPath).

read_file(Eval, RelPath) ->
    with_abs_path(Eval, RelPath, fun file:read_file/1).

read_file_info(Eval, RelPath) ->
    with_abs_path(Eval, RelPath, fun file:read_file_info/1).

write_file(Eval, RelPath, Bin) ->
    with_abs_path(Eval, RelPath, fun(A) -> file:write_file(A, Bin) end).

is_regular(Eval, RelPath) ->
    with_abs_path(Eval, RelPath, fun filelib:is_regular/1).

%% Convert a path list into a list of files in that directory,
%% expressed in target token form.
from_list_dir(Eval, PathList) ->
    RelDir = to_directory(PathList),
    with_abs_path(
      Eval, RelDir, 
      fun(AbsDir) ->
              {ok, Files} = file:list_dir(AbsDir),
              lists:append(
                  lists:map(
                    fun(FileName) ->
                            try
                                {Ext,Bn,[]} = from_filename(FileName),
                                [{Ext,Bn,PathList}]
                            catch
                                _C:_E ->
                                    %% Just ignore extensions that are not pterms.
                                    %% log:info("WARNING: redo:from_list_dir:~999p:~n~999p~n", [PathList, {_C, _E}]),
                                    []
                            end
                    end,
                    Files))
      end).

    

%% I would like to keep the "changed" predicate as abstract as
%% possible.  I currently see two ways to do this: filesystem
%% timestamp or file hash, and file hash seems more general.  See also
%% 'updated' case in handle/2 which is needed to prime the table.
file_changed(Eval, Product, RelPath) ->
    %% Needs to go outside of next call to avoid re-entry.
    case read_file_info(Eval, RelPath) of
        {ok,{file_info,_,_,_,
             _Atime,
             Mtime,
             _CTime,
             _,_,_,_,_,_,_}} ->
            MaybeOld = obj:call(Eval, {set_stamp, Product, Mtime}),
            Changed = {ok, Mtime} /= MaybeOld,
            %% debug("file_changed ~p~n", [{Product, RelPath, Changed, New, MaybeOld}]),
            Changed;
        Error ->
            log:info("~p~n",[{redo_file_changed, Product, RelPath, Error}]),
            error
    end.


%% FIXME: The .do version allows for configuration to add .c and .h
%% files from external repositories.  It uses environment variables to
%% do so.  We do it with explicit configuration items.

path_find(_Eval, _Ext, _BaseName, []) ->
    error;
path_find(Eval, Ext, BaseName, [Path|Paths]) ->
    File = to_filename({Ext,BaseName,Path}),
    %% log:info("find_c ~p~n",[File]),
    case redo:is_regular(Eval, File) of
        true  ->
            {ok, Path};
        false ->
            path_find(Eval, Ext, BaseName, Paths)
    end.


%% Hashes are slow and also do not capture the "void event" which is
%% quite useful in case the output of the network is something
%% happening, and not just a value change effect.  FIXME: Integrate
%% "stamping" somehow.
%%
%% file_changed_stamp(Eval, Product, RelPath, Stamp) ->
%%     %% Needs to go outside of next call to avoid re-entry.
%%     case read_file(Eval, RelPath) of
%%         {ok, Bin} ->
%%             stamp(Eval, Product, Stamp(Bin))
%%         Error ->
%%             throw({redo_file_changed, Product, RelPath, Error})
%%     end.
%%
%% file_changed(Eval, Product, RelPath) ->
%%     file_changed(Eval, Product, RelPath,
%%                  fun(Bin) ->
%%                          tools:hex(crypto:hash(md5, Bin))
%%                  end).

stamp(Eval, Product, NewStamp) ->
    MaybeOldStamp = obj:call(Eval, {set_stamp, Product, NewStamp}),
    Changed = {ok, NewStamp} /= MaybeOldStamp,
    %% debug("file_changed ~p~n", [{Product, RelPath, Changed, New, MaybeOld}]),
    Changed.

stamp_hash(Eval, Product) ->
    {ok, Bin} = read_file(Eval, to_filename(Product)),
    stamp(Eval, Product, crypto:hash(md5, Bin)).

%% Generic build command dispatch.

%% This is for running external commands.  It builds a trace log
%% useful for debugging and generating executable build traces.
%% FIXME: Also add throttling here, e.g. to limit parallellism.

run(Eval, Thing) ->
    Log =
        case obj:call(Eval, script_log) of
            {ok, L} -> L;
            _ -> fun ?MODULE:default_script_log/1
        end,
    run(Eval, Thing, Log).

default_script_log({line,Line}) -> log:info("~s~n", [Line]);
default_script_log(_) -> ok.

run(Eval, {mfa, MFA={M,F,A}}, Log) ->
    Eval ! {log_append, MFA},
    run(Eval, apply(M,F,A), Log);

%% Raw bash command
run(Eval, {bash, Cmds}, Log) ->
    {ok, Dir} = obj:call(Eval, {find_file, ""}),
    run:bash(Dir, Cmds, Log);

%% Via rpc.  Note that this does the rpc call here, so we can (later)
%% still do logging on this host, but run the command remotely via
%% whatever means.

%% FIXME: Make sure Log stays local.

run(Eval, {remote_bash, Var, Cmds}, Log) ->
    case rpc:call(Var, ?MODULE, run, [Eval, {bash, Cmds}, Log]) of
        Rv -> Rv
    end.
             



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME: These probably bitrotted.  Module is "dogfooded" and used in
%% a build system that exercises it thoroughly.  Delete these.

u1(Eval) ->
    %% Inputs and thunks are the same.
    put_val(Eval, ex1, #{}),
    true.
u2(Eval) ->    
    %% Update functions use the imperative redo style: after
    %% need/2, the dependencies are guaranteed to be ready, and
    %% the output var can be set at any time.  It's
    %% straightforward to embed pure functions in this
    %% framework.
    Deps = [ex1],
    need_val(Eval, Deps),
    put_val(Eval, ex2, #{}),
    true.

test(ex1) ->
    Updates = #{
      ex1 => fun ?MODULE:u1/1,
      ex2 => fun ?MODULE:u2/1
     },
    Spec = #{ update => fun(P) -> maps:get(P, Updates) end },
    {ok, Pid} = start_link(Spec),
    #{eval := Eval} = obj:dump(Pid),
    
    %% Standard "pull" mode.
    Changed1 = pull(Pid, [ex2]), debug("state1: ~p~n", [obj:dump(Eval)]),
    Changed2 = pull(Pid, [ex2]), debug("state2: ~p~n", [obj:dump(Eval)]),

    %% The afterthought "push" mode.
    Changed3 = push(Pid, [ex1], []),

    unlink(Pid),
    exit(Pid, kill),
    {Changed1,Changed2,Changed3};


test(nlspec) ->
    case import("/home/tom/exo/erl/apps/exo/src/nlspec_do.erl") of
        #{ outputs := Outputs } = Spec ->
            with_redo(
              Spec,
              fun(Pid) -> pull(Pid, Outputs) end)
    end;
test(uc_tools) ->
    test({uc_tools, fun(_Eval) -> ok end});
test(uc_tools_v) ->
    test({uc_tools, fun(Eval) -> log:info("~n~p~n",[obj:dump(Eval)]) end});

test({uc_tools, Report}) ->
    case import("/home/tom/exo/erl/apps/exo/src/uc_tools_gdb_do.erl") of
        #{ outputs := Outputs } = Spec ->
            with_redo(
              Spec,
              fun(Pid) ->
                      Eval = maps:get(eval, obj:dump(Pid)),

                      log:info("redo1 ~p~n", [Outputs]),
                      _ = pull(Pid, Outputs),
                      Report(Eval),

                      log:info("redo2 ~p~n", [Outputs]),
                      _ = pull(Pid, Outputs),
                      Report(Eval),

                      Push = [{c,<<"memory">>,[]}],
                      log:info("push3 ~p~n",[Push]),
                      _ = push(Pid, Push),
                      Report(Eval),

                      ok
              end)
    end;

test(Spec) ->
    throw({?MODULE,{test,Spec}}).




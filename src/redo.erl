-module(redo).
-export([redo/2, need/2,
         get/2, set/3, update/3,
         push/3,
         start_link/1, handle_outer/2, handle/2,
         file_changed/3,
         read_file/2,
         from_filename/1, to_filename/1,
         update_rule/3, update_file/1,
         run/2,
         gcc_deps/1,
         import/1,
         test/1, u1/1, u2/1]).


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
%%
%% - A bolted on "push" interface.


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


%% -------- CORE


%% Main entry point.  See test(ex1) below.
redo(Pid, Products) ->
    obj:call(Pid, {redo, Products}, 6124).

%% Trigger update of dependent products from change notification
push(Pid, Changed, NotChanged) ->
    obj:call(Pid, {push, Changed, NotChanged}, 6123).



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
        {Pid, deps} ->
            Rv = obj:call(Eval, deps),
            obj:reply(Pid, Rv),
            State;
        {Pid, {redo, Products}} ->
            ok = obj:call(Eval, start_pull),
            obj:reply(Pid, {ok, peval(Eval, Products)}),
            State;
        %% Added as a convenience.  Note that a network will need to
        %% have executed at least once to have a dependency graph
        %% available.  Changed/NotChanged are passed in here in case
        %% the change state is known from the context.  Otherwise
        %% network will poll the associated update function.
        {Pid, {push, Changed, NotChanged}} ->
            case obj:call(Eval, {ideps, Changed}) of
                error ->
                    obj:reply(Pid, error),
                    State;
                {ok, NeedUpdate} ->
                    ok = obj:call(Eval, {start_push, Changed, NotChanged}),
                    obj:reply(Pid, {ok, peval(Eval, NeedUpdate)}),
                    State
            end
    end.

with_redo(Spec, Fun) ->
    {ok, Pid} = start_link(Spec),
    Rv = Fun(Pid),
    unlink(Pid),
    exit(Pid, kill),
    Rv.




%% Evaluator maintains dependency table, store (optional), and keeps
%% track of which nodes have been checked in a single network run.
start_eval(Spec = #{ update := _ }) ->
    serv:start(
      {handler,
       fun() -> Spec end,
       fun ?MODULE:handle/2}).

%% RetVal  Meaning
%% true    object has been rebuilt, output propagation needed
%% false   no change, no propagation needed
handle({CallPid, {eval, Product}},
       State = #{update := ProductToUpdate}) ->
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
            Update = ProductToUpdate(Product),
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
              fun(Waiter) -> obj:reply(Waiter, Changed) end,
              Waiters),
            %% debug("~p: changed=~p, deps=~n~p~n", [Product, Changed, NewDeps]),
            debug("~p: changed=~p~n", [Product, Changed]),
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
handle({Pid,{update,Tag,Fun}}, State) ->
    MaybeOld = maps:find({product, Tag}, State),
    New = Fun(MaybeOld),
    obj:reply(Pid, {MaybeOld, New}),
    maps:put({product, Tag}, New, State);

%% We provide file access relative to a "working directory" shared by
%% all update scripts.  Keep this abstract so it is easy to change
%% later.
handle({Pid,{find_file,RelPath}}, State = #{dir := Dir}) ->
    obj:reply(Pid, {ok, tools:format("~s/~s",[Dir,RelPath])}),
    State;

    

%% Prepare for a new pull run, cleaning up old cruft.  This will make
%% sure we propagate all the way to the inputs when checking
%% dependencies.
handle({Pid, start_pull}, State) ->
    obj:reply(Pid, ok),
    maps:filter(
      fun({phase, _}, _) -> false; (_,_) -> true end,
      State);

%% Same, but for a push run.  In this case we know what changed and
%% what didn't.  Note that this only makes sense when there are deps,
%% e.g. after a pull run.  Note that NotChanged could just contain all
%% inputs, as we overwrite Changed set later.
handle({Pid, {start_push, Changed, NotChanged}}, State0) ->
    obj:reply(Pid, ok),
    State1 =
        maps:filter(
          fun({phase, _}, _) -> false; (_,_) -> true end,
          State0),
    State2 =
        lists:foldr(
          fun(NC, S) -> maps:put({phase,NC}, {changed, false}, S) end,
          State1, NotChanged),
    State3 =
        lists:foldr(
          fun(C, S) -> maps:put({phase,C}, {changed, true}, S) end,
          State2, Changed),
    State3;

%% Get the dependencies in a form compatible with depgraph.erl
handle({Pid, {ideps, Products}}, State) ->
    Deps =
        maps:fold(
          fun({deps,Proc},Deps,Acc) -> [{Proc,'_',Deps} | Acc];
             (_,_,Acc) -> Acc
          end,
          [], State),
    NeedUpdate =
        case Deps of
            [] ->
                %% No deps.  Network didn't run yet.
                error;
            _ ->
                IDeps = depgraph:invert_deps(Deps),
                {ok, maps:keys(depgraph:need_update(IDeps, Products))}
        end,
    debug("~p: need update ~p~n", [Products, NeedUpdate]),
    obj:reply(Pid, NeedUpdate),
    State;

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

%% Evaluate products in parallel
peval(StatePid, Products) ->
    tools:pmap(
      fun(P) -> obj:call(StatePid, {eval, P}) end,
      Products).

%% Also called "ifchange".
need(StatePid, Deps) ->
    DepsUpdated = peval(StatePid, Deps),
    NeedUpdate =
        lists:any(
          fun(Updated)-> Updated end,
          maps:values(DepsUpdated)),
    debug("need: ~p~n",[{NeedUpdate,DepsUpdated}]),
    NeedUpdate.
    



%% Provide a store interface.
%%
%% Note that a store is not always necessary.  It is perfectly
%% possible to have redo operate on abstract names, e.g. files in a
%% filesystem, entries in a database, ...
%%
%% Calling this is only allowed from update tasks, which have get
%% access _after_ calling need, and set access to the associated
%% output tag during the execution of the updat task.  One way to look
%% at it is that the purpose of the system is to schedule update
%% scripts in such a way to properly guard imperative updates like
%% these.

update(StatePid, Tag, Fun) -> obj:call(StatePid, {update,Tag,Fun}).
set(StatePid, Tag, Val)    -> obj:call(StatePid, {set,Tag,Val}).
get(StatePid, Tag)         -> obj:call(StatePid, {get,Tag}).


debug(_F,_As) ->
    %% log:info(_F,_As),
    ok.


%% Import specs and build rules
import(Path) ->
    debug("importing from ~s~n", Path),
    reflection:run_module(Path, do, []).


%% -------- LIB FOR SCRIPTS

%% Special kinds of update routines

%% This is a nice abstraction on top of redo if the dependencies are
%% known before the rule is running.  It resembles 'make' rules.  We
%% split it up in two parts: rule lookup and script lookup.

%% FIXME: Decouple from shell assumption.  Maybe split off redo_shell
%% library?

update_rule(Rule, Build, Target) ->
    {Deps, ScriptSpec} = Rule(Target),
    fun(RedoPid) ->
            RunSpec = Build(ScriptSpec),
            redo:need(RedoPid, Deps),
            %% At this point, inputs are available and we can execute
            %% the shell code.  FIXME: This
            debug("RUN: ~s~n", [RunSpec]),
            case run(RedoPid, RunSpec) of
                {ok, {0, _Out}} ->
                    {true, Deps};
                Err ->
                    throw(Err)
            end
    end.

update_file(Target) ->
    File = redo:to_filename(Target),
    fun(RedoPid) ->
            debug("c: ~p~n", [Target]),
            {redo:file_changed(RedoPid, Target, File), []}
    end.


%% Note that the redo core does not know about:
%% - the file system
%% - shell commands

%% Traditional "build system" functionality is built using these
%% interface routines.



%% Files and scripts

%% Convert between Erlang data type that is easy to pattern match, and
%% a "dotted list" file name encoding.
from_filename(IOList) ->
    %% debug("from_filename: ~s~n", [IOList]),
    [FileName|RPath] = lists:reverse(re:split(IOList,"/")),
    [BaseName|BinDotNames] = re:split(FileName,"\\."),
    %% debug("BinDotNames ~p~n",[BinDotNames]),
    DotNames =  [type:decode({pterm,Bin}) || Bin <- BinDotNames],
    list_to_tuple(lists:reverse(DotNames) ++ [BaseName,RPath] ).

%% E.g. {c,<<"dht11">>=BaseName,[]=Path}
%% Symbol tags can be multiple.
to_filename(Tuple) ->
    [Dirs,BaseName|Tags] = lists:reverse(tuple_to_list(Tuple)),
    Path = [[Dir,"/"] || Dir <- Dirs],
    Ext  = [[".",type:encode({pterm,Tag})] || Tag <- Tags],
    tools:format("~s",[[Path,BaseName,Ext]]).

%% FIXME: This probably doesn't work with spaces in names.
gcc_deps(Bin0) ->
    Bin1 = re:replace(Bin0, "^.*: ", ""),
    Bin2 = re:replace(Bin1, "\\n$", ""),
    Bin3 = re:replace(Bin2, " \\\\\n", "", [global]),
    List = re:split(Bin3, " "),
    lists:map(fun from_filename/1, List).

%% I would like to keep the "changed" predicate as abstract as
%% possible.  I currently see two ways to do this: filesystem
%% timestamp or file hash.
read_file(StatePid, RelPath) ->
    {ok, AbsPath} = obj:call(StatePid, {find_file, RelPath}),
    file:read_file(AbsPath).

file_changed(StatePid, Product, RelPath) ->
    %% Needs to go outside of update/3 to avoid re-entry.
    {ok, Bin} = read_file(StatePid, RelPath),
    {MaybeOld, New} =
        update(
          StatePid, Product,
          fun(_) -> crypto:hash(md5, Bin) end),
    Changed = {ok, New} /= MaybeOld,
    %% debug("file_changed ~p~n", [{Product, RelPath, Changed, New, MaybeOld}]),
    Changed.

%% Generic build command dispatch.  This can
run(StatePid, {bash, Cmds}) ->
    {ok, Dir} = obj:call(StatePid, {find_file, ""}),
    log:info("run: ~s~n", [Cmds]),
    run:fold_script(
      tools:format("bash -c 'cd ~s ; ~s'", [Dir, Cmds]), 
      fun({data,{eol,Line}}, Lines) ->
              %% tools:info("~s~n",[Line]),
              {cont, [Line ++ "\n"|Lines]};
         ({exit_status, ExitCode},Lines) ->
              {done, {ExitCode, lists:flatten(lists:reverse(Lines))}}
      end,
      [],
      infinity,
      [{line, 1024}]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

u1(Pid) ->
    %% Inputs and thunks are the same.
    set(Pid, ex1, #{}),
    {true, []}.
u2(Pid) ->    
    %% Update functions use the imperative redo style: after
    %% need/2, the dependencies are guaranteed to be ready, and
    %% the output node can be set at any time.  It's
    %% straightforward to embed pure functions in this
    %% framework.
    Deps = [ex1],
    need(Pid, Deps),
    set(Pid, ex2, #{}),
    {true, Deps}.

test(ex1) ->
    Updates = #{
      ex1 => fun ?MODULE:u1/1,
      ex2 => fun ?MODULE:u2/1
     },
    Spec = #{ update => fun(P) -> maps:get(P, Updates) end },
    {ok, Pid} = start_link(Spec),
    #{eval := Eval} = obj:dump(Pid),
    
    %% Standard "pull" mode.
    Changed1 = redo(Pid, [ex2]), debug("state1: ~p~n", [obj:dump(Eval)]),
    Changed2 = redo(Pid, [ex2]), debug("state2: ~p~n", [obj:dump(Eval)]),

    %% The afterthought "push" mode.
    Changed3 = push(Pid, [ex1], []),

    unlink(Pid),
    exit(Pid, kill),
    {Changed1,Changed2,Changed3};

            


%% FIXME: c->o en o->a rules are there.  Make some tests regarding
%% actual redoing.  FIXME: Bug: second redo should not run the update.
test(uc_tools) ->
    case import("/home/tom/exo/erl/apps/exo/src/uc_tools_gdb_do.erl") of
        #{ outputs := Outputs } = Spec ->
            with_redo(
              Spec,
              fun(Pid) ->
                      log:info("redo1 ~p~n", [Outputs]),
                      _ = redo(Pid, Outputs),
                      log:info("redo2 ~p~n", [Outputs]),
                      _ = redo(Pid, Outputs),
                      ok
              end)
    end;

test(Spec) ->
    throw({?MODULE,{test,Spec}}).




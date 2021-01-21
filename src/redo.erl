-module(redo).
-export([pull/1, pull/2,
         pull_vals/1, pull_vals/2,
         with_eval/2,
         make_var/1, make_var/2, make_var/3,
         push/3, push/2,
         start_link/1, handle_outer/2, handle/2,
         file_changed/3,
         from_list_dir/2, from_list_dir_type/3, files_from_filename/2,
         read_file/2, write_file/3, is_regular/2,
         from_filename/1, to_filename/1, to_filename_binary/1, 
         to_directory/1, to_includes/1,
         compile_env/2, compile_path/2,
         path_find/4,
         root/1,
         update_using/2, update_file/1,
         update_value/3, update_pure/3, update_const/2,
         update_case/3,
         need/2, changed/2,
         stamp/3, stamp_hash/2, stamp_hash/3,
         run/2, run/3,
         gcc_deps/1, need_gcc_deps/2,
         import/1,
         need_vals/2, find_vals/2, get_vals/2, %% retrieval is always parallel
         put_val/3, put_val_changed/3,         %% put is sequential
         no_update/1,
         default_script_log/1,
         default_log_error/1,
         save_state/2, merge_state/2, filter_state/2,
         dump_state/2, dump_state/1,
         export_deps/2, export_makefile/3,
         test/1, u1/1, u2/1]).

-define(WARN,{warn, 20000}).

%% TL;DR
%%
%% It is customary to apologize for writing yet another build system.
%% I've tried many existing approaches, and I've come to the
%% conclusion that for my case it was necessary.
%%
%% 1. Most build systems operates on files.  This is not abstract
%%    enough for me.  I need this generalized to updating abstract
%%    caches in a distributed heterogeneous network managed through
%%    Erlang proxy processes.
%%
%% 2. File names are not a good abstraction for quantification,
%%    i.e. to express generic build rules.  Erlang's pattern matching
%%    is a much better fit for this.
%%
%% 3. The system is built as an overlay, where a subset of the rules
%%    that operate only on files can be exported as Makefiles or
%%    (TODO) any other build system.


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

%% Bound shorthands in case of registered redo singleton setup.
pull(Products) -> pull(exo:need(redo), Products).
pull_vals(Products) -> pull_vals(redo, Products).

%% Main entry point.  See test(ex1) below.
pull(Redo, Products0) when is_list(Products0) ->
    Products = lists:map(fun rename_product/1, Products0),
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

%% Interpret strings as filenames and map them to the canonical tuple
%% representation.
rename_product(L) when is_list(L) -> from_filename(L);
rename_product(P) -> P.
    


%% Trigger update of dependent products from external change notification
push(Redo, Changed, NotChanged) ->
    obj:call(Redo, {push, Changed, NotChanged}, infinity).
%% Providing NotChanged is just an optimization.  Not filling those in
%% will let the network determine change statuse.
push(Redo, Changed) ->
    push(Redo, Changed, []).

%% Run evaluations against a network.
with_eval(Redo, Fun) ->
    obj:call(Redo, {with_eval, Fun}, ?WARN).

%% Similar, but install a new opaque var using a function.
make_var(Redo) ->
    make_var(Redo, undefined).
make_var(Redo, Fun) ->
    make_var(Redo, Fun, '_NEW_').
make_var(Redo, Fun, VarTag) ->
    obj:call(Redo, {make_var, Fun, VarTag}).

%% For products that are Erlang values, update and return the value.
pull_vals(Redo, Products) ->
    with_eval(Redo, fun(Eval) -> need_vals(Eval, Products) end).


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

%% Redo internal state is path-indexed with a tag such as {log,_} or
%% {deps, _}.  This creates a projection for a limited set of tags.
filter_state(Redo, Tags) ->
    maps:filter(
      fun(K,_V) ->
              case K of
                  {Tag,_} -> lists:member(Tag, Tags);
                  _       -> false
              end
      end,
      obj:call(Redo, state)).


%% Export build rules.
%%
%% The build rules for the binaries are necessarily a subset of the
%% redo.erl build rules, because we can't export abstract build
%% operations.  Note that this is ok as it covers two completely
%% different use cases:
%%
%% 1. Fully integrated build based on redo.erl managing abstract
%%    state, deployment, ...
%%
%% 2. Stand-alone Makefile for code distribution, without any
%%    dependencies on the scaffolding system
%%
%% This is interpreted as a "projection" of the build rules onto a
%% limited set of operations.  Only rules that are based on redo:run/2
%% and thus provide a log of build commands will be exported.

%% redo:export_deps(redo, [{c8,testsuite}]).

export_deps(Redo,Targets) ->
    Rules = filter_state(Redo, [log, deps]),
    fold_commands(Rules,#{},Targets).

fold_commands(Rules,State,Targets) ->
    lists:foldl(
      fun(Target,S) ->
              %% Don't print anything in the non-pruned branch!
              case maps:find(Target, S) of
                  {ok,_} -> 
                      %% PRUNE: Rule is already compiled, so we can
                      %% cut off the traversal here.
                      S;
                  error ->
                      %% PROJECT: Only keep targets that have external
                      %% build scripts.
                      Log = maps:get({log,Target}, Rules, []),
                      TargetDeps0 = maps:get({deps, Target}, Rules, []),

                      %% Remove all absolute paths.  E.g. dependencies
                      %% discovered by GCC.
                      TargetDeps =
                          lists:filter(
                            fun({_,_,Dir}) ->
                                    case lists:reverse(Dir) of
                                        [<<>>|_] -> false;
                                        _ -> true
                                    end;
                               (_) -> true
                            end,
                            TargetDeps0),

                      S1 = case [C || {run, C} <- Log] of
                               [] ->
                                   S;
                               CmdSpecs ->
                                   %% FIXME: Also filter deps that are not files?
                                   Rule = #{ cmd_specs => CmdSpecs, deps => TargetDeps },
                                   maps:put(Target, Rule, S)
                           end,
                      %% RECURSE: Traverse the tree recursively.
                      fold_commands(Rules, S1, TargetDeps)
              end
      end,
      State,
      Targets).


%% Generate a Makefile.  A previous attempt extracted variable names,
%% which quickly became too complicated due to both having to work
%% with Makefile variable syntax and shell environment variable syntax
%% and associated quoting.  Instead, we rely only on path substitution
%% (rebasing), which allows upstream config e.g. through symlinks.
%%
%% To make export practical, some more things need to happen: remove
%% all dependencies on system files.  This can be done heuristically
%% by removing all absolute paths.

%% redo:export_makefile(redo, [{c8,testsuite}]).


export_makefile(Redo, Targets, Rebase) ->
    sink:gen_to_list(
      fun(Sink) -> export_makefile(Redo, Targets, Rebase, {sink, Sink}) end).

export_makefile(Redo, Targets, Rebase, {sink, Sink}) ->
    Fmt = fun(F,A) -> Sink({data,io_lib:format(F,A)}) end,

    ExportedDeps = maps:to_list(export_deps(Redo, Targets)),

    %% Deps :: #{ target() => #{ cmds => cmds(), deps => [target()] } }
    lists:foreach(
      fun({Target,
           #{ cmd_specs := CmdSpecs,
              deps := Deps }}) ->
              TargetFile = to_filename(rebase_file(Rebase, Target)),
              Fmt("\n~s: \\\n", [TargetFile]),
              lists:foreach(
                fun({Typ,BN,Dir0}) when is_binary(BN) and is_list(Dir0) ->
                        Dir = rebase_dir(Rebase, Dir0),
                        Dep = {Typ,BN,Dir},
                        Fmt("\t~s \\\n",[to_filename(Dep)]);
                   (_) ->
                        ok
                end,
                Deps),
              Fmt("\n",[]),
              lists:foreach(
                fun({bash_with_vars, Env0, Cmd}) ->
                        Env = compile_env_make(Env0, Rebase),
                        Cmd1 = ["@echo ", TargetFile,                             
                                " ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \\\n\t", %% Allow extra config
                                run:with_vars(Env, Cmd," ; \\\n\t")],
                        Fmt("\t~s\n", [makefile_quote(Cmd1)])
                end,
                CmdSpecs)
      end,
      ExportedDeps),

    Fmt("\n"
        ".PHONY: clean\n"
        "clean:\n"
        "\trm -f",
        []),
    lists:foreach(
      fun({Target, _}) ->
              Fmt(" \\\n\t\t~s", [to_filename(rebase_file(Rebase, Target))])
      end,
      ExportedDeps),
    Fmt("\n",[]),
    Sink(eof).


compile_env_make(Env, Rebase) ->
    Compile = #{
      root => "/i/exo/", %% FIXME! not: root(Eval),
      to_filename =>
          fun(File) ->
                  to_filename(rebase_file(Rebase, File))
          end,
      to_directory =>
          fun(Dir) ->
                  to_directory(rebase_dir(Rebase, Dir))
          end
     },
    compile_env(Compile, Env).

%% Go from most specific to least specific until there is a match,
%% otherwise keep old.
rebase_file(Rebase, {Type,BN,Dir}) ->
    {Type,BN,rebase_dir(Rebase, Dir)}.
rebase_dir(_Rebase, []) -> [];
rebase_dir(Rebase, Dir) ->
    case maps:find(Dir, Rebase) of
        {ok, NewDir} ->
            %% log:info("rebase: ~999p -> ~999p~n", [Dir, NewDir]),
            NewDir;
        error ->
            %% log:info("no rebase: ~p~n", [Dir]),
            [hd(Dir) | rebase_dir(Rebase, tl(Dir))]
    end.
    


makefile_quote(IOL) ->
    makefile_quote_(binary_to_list(iolist_to_binary(IOL))).
makefile_quote_([])     -> [];
makefile_quote_([$$|T]) -> [$$,$$|makefile_quote_(T)];
makefile_quote_([H|T])  -> [H|makefile_quote_(T)].
    

%% Dump state necessary for resume.
save_state(Redo, File) ->
    MinimalState = filter_state(Redo, [stamp,val,log,deps]),
    %% encode performs some checks, but the format is more readable.
    _ = type:encode({pterm,MinimalState}),
    Bin = io_lib:format("~p~n",[MinimalState]),
    file:write_file(File, Bin).

%% Merge current state with state from save file.

merge_state(Redo, Map) when is_map(Map) ->
    _ = obj:call(Redo, {merge_state, Map}),
    ok;

merge_state(Redo, File) ->
    try
        {ok, Bin} = file:read_file(File),
        Map = type:decode({pterm, Bin}),
        true = is_map(Map),
        merge_state(Redo, Map)
    catch _:_ ->
            error
    end.


%% Dump entire state of the redo object to a file, for debug.  This
%% also dumps non-pterms.
dump_state(Redo) ->
    obj:call(Redo, state).
dump_state(Redo, File) ->
    State = obj:call(Redo, state),
    IOL =  io_lib:format("~p~n",[State]),
    file:write_file(File, IOL).



handle_outer(Msg, State = #{eval := Eval}) ->
    %% log:info("handle_outer: ~p~n",[Msg]),
    case Msg of
        {Pid, state} ->
            obj:reply(Pid, obj:dump(Eval)),
            State;
        {Pid, {merge_state, Map}} when is_map(Map) ->
            obj:reply(Pid, obj:merge(Eval, Map)), 
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
            epid:dispatch(notify, start, State),
            obj:call(Eval, reload),
            ok = obj:call(Eval, start_pull),
            epid:dispatch(notify, stop, State),
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
            State;
        _ ->
            %% Delegate to a mixin.
            Mixins = [fun epid:mixin/3],
            {Handled, State1} = serv:delegate(Mixins, Msg, State),
            case Handled of
                true -> ok;
                false -> log:info("WARNING: not handled: ~p~n", [Msg])
            end,
            State1
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
    %% Error logging is parameterized.  Reason is that error messages
    %% from parallell builds can be very confusing, so it helps to
    %% isolate them.

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
                    ST = erlang:get_stacktrace(),
                    log_error(State, {error, Product, eval, {C, E, ST}}),
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
handle({Pid, collect_worker_deps}, State) ->
    case maps:find({need,Pid}, State) of
        {ok, DepMap} ->
            obj:reply(
              Pid,
              lists:sort(maps:keys(DepMap))),
            maps:remove({need, Pid}, State);
        error ->
            obj:reply(Pid, []),
            State
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


%% Error message coming from worker
handle({log_error, ErrorInfo}, State) ->
    log_error(State, ErrorInfo),
    State;

%% Similarly, other metadata can be attached.

%% Per-target logging during rule exuction phase.  We index it pid,
%% then re-index it under target name once run is complete.

handle({Pid, {log, Key, Val}}, State) ->
    obj:reply(Pid, ok),
    OldLog = maps:get({log, Pid}, State, []),
    NewLog = [{Key,Val}|OldLog],
    maps:put({log, Pid}, NewLog, State);
handle({Pid, {rename_log, Target}}, State) ->
    {Log1, State1} =
        case maps:find({log, Pid}, State) of
            error -> 
                {error, State};
            {ok, Log} ->
                {{ok, Log},
                 maps:put({log, Target}, Log,
                          maps:remove({log,Pid}, State))}
        end,
    obj:reply(Pid, {ok, Log1}),
    State1;


%% Time stamp, hashes, ...
handle({Pid,{set_stamp,Tag,New}}, State) ->
    MaybeOld = maps:find({stamp, Tag}, State),
    obj:reply(Pid, MaybeOld),
    maps:put({stamp, Tag}, New, State);

%% Implement a store inside of the evaluator.  Note that we can just
%% as well use a completely abstract (remote/external) store and
%% update methods that operate on that store.
handle({Pid,{find_vals,Tags}}, State) ->
    Vals =
        lists:map(
          fun(Tag) -> maps:find({val, Tag}, State) end,
          Tags),
    obj:reply(Pid, Vals),
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
         ({need, _}, _) -> false;
         %% Don't erase per-target logs.  These are used for rule export.
         %% ({log, _}, _) -> false;  
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
handle({_,{find,_}}=Msg, State)  -> obj:handle(Msg, State);
handle({_,dump}=Msg, State)      -> obj:handle(Msg, State);
handle({_,{merge,_}}=Msg, State) -> obj:handle(Msg, State);

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
            [] ->
                true; %% First run or polling
            _  ->
                A = need_or_error(Eval, OldDeps),
                %% Ensure worker_deps are cleared in case we run
                %% update below.
                _ = obj:call(Eval, collect_worker_deps),
                A
        end,

    debug("~p: need update: ~p~n", [Product, Affected]),
    %% It's simplest to compute the state update here.
    PhaseUpdate =
        case Affected of
            false ->
                %% Only skip if we are sure that no rebuild is
                %% necessary.
                #{ {phase, Product} => {changed, Affected} };
            _ ->
                %% When changed, or one of the old dependencies
                %% returned an error, we need to re-run the rule.
                %%
                %% Dont crash the daemon in the common case that a
                %% build rule has a runtime or type error.  Propagate
                %% 'error' instead.

                %% Logger wil print the product as log prefix.
                %% log:info("worker~n"),

                {Changed, NewDeps} =
                    try
                        %% app/2 allows for "reloadable closures".
                        Ch = app(Update,[Eval]),
                        Wd = obj:call(Eval, collect_worker_deps),
                        case Ch of
                            false ->
                                {false, Wd};
                            true  -> 
                                %% FIXME: Make this configurable?
                                %% Logging is too verbose to go to the
                                %% main log.
                                %% log:info("changed~n"),
                                {true, Wd};
                            error ->
                                log:info("error~n"),
                                %% Erase deps on failure.  We don't
                                %% know what went wrong, so make sure
                                %% it re-runs next time.
                                {error, []};  
                            _ ->
                                throw({bad_update_retval, Update, Ch})
                        end
                    catch C:E ->
                            ST = erlang:get_stacktrace(),
                            %% We're not running in the handler, so
                            %% this is sent onward to be handled
                            %% elsewhere.
                            Eval ! {log_error, {error, Product, {worker,self()}, {C, E, ST}}},
                            {error,[]}
                    end,

                %% Save the pid-indexed log under a symbolic name.
                {ok, _Log} = obj:call(Eval, {rename_log, Product}),

                %% FIXME gather {log,Pid} and translate it to {log,Target}
                

                %% Special-case the happy path.  Keeping track of dep
                %% graph changes allows caching the inverted dep graph
                %% for "push" operation and allows to prune a need/2
                %% call to stamp the new deps.

                %% FIXME: To better implement "push": change the
                %% (open) inverted dependencies accordingly when the
                %% dependencies change, but don't compute the
                %% transitive closure.  Do that on "push".

                %% case Product of 
                %%     {{o,f103},<<"synth">>,[<<"gdb">>,<<"uc_tools">>]} ->
                %%         log:info("NewDeps:~n~p~n",[NewDeps]);
                %%     _ ->
                %%         ok
                %% end,

                case NewDeps == OldDeps of
                    true ->
                        #{{phase, Product} => {changed, Changed}};
                    false ->
                        debug("worker: deps changed: ~p~n", [{OldDeps,NewDeps}]),
                        %% _ = need(Eval, NewDeps), %% FIXME: This is no longer necessary
                        #{{phase, Product} => {changed, Changed},
                          {deps,  Product} => NewDeps}
                end
                
        end,
    Eval ! {worker_done, Product, PhaseUpdate},
    ok.


             


%% Any error -> error, any change -> true.
any_changed(Prod2Changed) ->
    ChangeList = maps:values(Prod2Changed),
    case lists:member(error, ChangeList) of
        true  -> error;
        false -> lists:member(true, ChangeList)
    end.

%% Also called "ifchange".

%% Two behaviors are necessary.  need/2 is called from inside user
%% rules, and will have to abort the rule when one of the deps error
%% out.
need(Eval, Deps) ->
    {Changed, P2C} = need_or_error_p2c(Eval, Deps),
    case Changed of
        error -> throw({error_need, P2C});
        Other -> Other
    end.
%% This variant is for internal usely, executed before evaluating an
%% update rule.
need_or_error(Eval, Deps) ->
    {Changed, _P2C} = need_or_error_p2c(Eval, Deps),
    Changed.
%% Also returns Prod2Changed, for debugging.
need_or_error_p2c(Eval, Deps) ->
    Prod2Changed = changed(Eval, Deps),
    Changed = any_changed(Prod2Changed),
    debug("need: ~p~n",[{Changed,Prod2Changed}]),
    {Changed, Prod2Changed}.


            

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
    %% Typical dilemma: what is a good level of verbosity?  We can't
    %% put an actual timeout here.  But it is probably good to add
    %% some indication that a particular target is taking a long time
    %% to evaluate.
    Timeout = ?WARN,
    tools:pmap(
      fun(P) -> obj:call(Eval, {eval, P}, Timeout) end,
      Deps).


%% Provide dataflow variables with values stored in the evaluator.
%%
%% Here 'find' behaves as ordinary find, e.g. it can fail, but it also
%% calls 'need', so all variables will have to be dataflow variables.
%% In practice this is always what you want.

find_vals(Eval, Tags) ->
    obj:call(Eval, {find_vals, Tags}).
get_vals(Eval, Tags) ->
    lists:map(
      fun(MaybeVal) ->
              case MaybeVal of
                  error -> throw({get_val_not_found, Tags});
                  {ok, Val} -> Val
              end
      end,
      find_vals(Eval, Tags)).
need_vals(Eval, Tags) ->
    case need(Eval, Tags) of
        error -> throw({need_val_error, Tags});
        _ -> get_vals(Eval, Tags)
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
            put_val_changed(Eval, Target, NewVal)
    end.
put_val_changed(Eval, Target, NewVal) ->
    case find_vals(Eval, [Target]) of
        [{ok, NewVal}] ->
            false;
        _ ->
            put_val(Eval, Target, NewVal),
            true
    end.



%% Similar, inputs are also variables.  This brings pure Erlang
%% functions into the mix.
update_pure(Target, Deps, PureBody) ->
    update_value(
      Target, Deps,
      fun(Eval) -> PureBody(get_vals(Eval, Deps)) end).

update_const(Target, Val) ->
    update_pure(Target, [], fun([]) -> Val end).


%% Dispatch contained in a map, to allow for proper rebuild.
update_case(Target, Config, DefaultUpdate) ->
    fun(Eval) ->
            [Cases] = redo:need_vals(Eval, [Config]),
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


%% Do type checks to avoid weird issues with target name aliasing
%% between strings and byte strings.
is_list_of_binary([]) -> true;
is_list_of_binary([H|T]) -> 
    is_binary(H) and is_list_of_binary(T).

%% E.g. {c,<<"dht11">>=BaseName,[]=Path}
%% Symbol tags can be multiple.
to_directory(Dirs) ->
    case is_list_of_binary(Dirs) of
        true -> ok;
        false -> throw({redo_dirname, Dirs})
    end,
    tools:format("~s",[[[Dir,"/"] || Dir <- lists:reverse(Dirs)]]).
to_filename({Type,BaseName,Dirs}=Product) ->
    case is_binary(BaseName) of
        true -> ok;
        false -> throw({redo_basename,Product})
    end,
    Tags = case {Type,is_tuple(Type)} of
               {'',_} -> [];
               {_,true} -> lists:reverse(tuple_to_list(Type));
               {_,false} -> [Type]
           end,
    Path = to_directory(Dirs),
    Ext  = [[".",type:encode({pterm,Tag})] || Tag <- Tags],
    tools:format("~s",[[Path,BaseName,Ext]]).
to_filename_binary(F) ->
    iolist_to_binary(to_filename(F)).


to_includes(Paths) ->
    to_includes(fun to_directory/1, Paths).
to_includes(_Compile = #{ to_directory := ToDirectory}, Paths) ->
    [[" -I",ToDirectory(P)] || P <- Paths].

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
    %% log:info("need_gcc_deps:~n~p~n",[Deps]),
    need(Eval,Deps).


root(Eval) ->
    {ok, AbsPath} = obj:call(Eval, {find_file, ""}),
    AbsPath.

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
              files_from_filename(Files, PathList)
      end).
from_list_dir_type(Eval, Dir, Type) ->
    lists:filter(
      fun({Type1,_,_}) -> Type1 == Type;
         (_)           -> false
      end,
      from_list_dir(Eval, Dir)).

files_from_filename(Files, PathList) ->
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
        Files)).


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
            Msg = {redo_file_changed, Product, RelPath, Error},
            log:info("WARNING: file_changed: ~p~n", [Msg]),
            %% throw(Msg)
            true
            %% log:info("~p~n",[Msg]), error
    end.


%% FIXME: The .do version allows for configuration to add .c and .h
%% files from external repositories.  It uses environment variables to
%% do so.  We do it with explicit configuration items.

%% FIXME: This is a really bad idea!  Currently only do_uc_tools.erl
%% uses this.  I'm going to phase it out.

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
    stamp_hash(Eval, Product, to_filename(Product)).

stamp_hash(Eval, Product, Filename) ->
    {ok, Bin} = read_file(Eval, Filename),
    stamp(Eval, Product, crypto:hash(md5, Bin)).

%% Generic build command dispatch.


%% run/2 will run external commands.  The reason to abstract it is to
%% be able to reconstruct build scripts from trace logs.

%% FIXME: Also add throttling here, e.g. to limit parallellism.

%% Provide a console logging mechanism if there is none.
run(Eval, Thing) ->
    Log =
        case obj:call(Eval, script_log) of
            {ok, L} -> L;
            _ -> fun ?MODULE:default_script_log/1
        end,
    run(Eval, Thing, Log).

%% This step records the specification of the external command to the
%% trace log, before interpreting the command specification.  Note
%% that the trace log is different from the console log, which records
%% command output.
run(Eval, CommandSpec, ConsoleLog) ->
    obj:call(Eval, {log, run, CommandSpec}),
    run_(Eval, CommandSpec, ConsoleLog).


%% Implementation.
%% 1. Some erlang command that generates a new spec.
run_(Eval, {mfa, {M,F,A}}, ConsoleLog) ->
    run_(Eval, apply(M,F,A), ConsoleLog);

%% 2. Bash command with environment.
run_(Eval, {bash_with_vars, EnvMap0, Cmds}, ConsoleLog) ->

    Compile = #{
      root => root(Eval),
      to_filename  => fun to_filename/1,
      to_directory => fun to_directory/1 },
    EnvMap = compile_env(Compile, EnvMap0),

    %% log:info("EnvMap=~n~p~n", [EnvMap]),
    run_(Eval, {bash, run:with_vars(EnvMap, Cmds)}, ConsoleLog);

%% 3. Raw bash command
run_(Eval, {bash, Cmds}, ConsoleLog) ->
    {ok, Dir} = obj:call(Eval, {find_file, ""}),
    run:bash(Dir, Cmds, ConsoleLog);

%% Via rpc.  Note that this does the rpc call here, so we can (later)
%% still do logging on this host, but run the command remotely via
%% whatever means.

%% FIXME: Make sure Log stays local.

run_(Eval, {remote_bash, Var, Cmds}, Log) ->
    case rpc:call(Var, ?MODULE, run, [Eval, {bash, Cmds}, Log]) of
        Rv -> Rv
    end.
   

%% FIXME: This is not a good idea.  The build output can be very
%% verbose when a cascade error happens, to the point of making the
%% emacs erlang shell buffer unusable.  So the default is set to not
%% printing anything.

%% default_script_log({line,Line}) -> log:info("~s~n", [Line]);
%% default_script_log(_) -> ok.

default_script_log({line,_Line}) -> ok;
default_script_log(_) -> ok.


default_log_error({error, Product, Site, {_C,_E,_ST}=Report}) ->
    log:info("ERROR_LOG: ~p:~p:~n~p",
             [Product, Site, Report]).

%% Note there are currently two sites that can generate an error, the worker, and the

log_error(State, Error) ->
    LogError =
        maps:get(
          log_error,
          State,
          fun ?MODULE:default_log_error/1),
    LogError(Error).



%% In order export redo.erl rules to e.g. Makefiles, we attempt to
%% keep the build commands abstract.  I.e. we do not flatten any paths
%% that go into environment variables.  This is to allow some
%% post-processing during export.
%%
%% Note however that for the current implementation of Makefile
%% export, we do not rely on variable names, but on symlinks, as those
%% do not need two levels of variable abstract (Makefile variables and
%% shell variables).

compile_path(Compile = #{
               root := Root,
               to_filename := ToFilename,
               to_directory := ToDirectory },
             Spec) ->
    case Spec of
        {to_filename,Path}      -> ToFilename(Path);
        {to_directory,Dir}      -> ToDirectory(Dir);
        {to_filename_abs,Path}  -> [Root, ToFilename(Path)];
        {to_directory_abs,Dir}  -> [Root, ToDirectory(Dir)];
        {to_filenames,Paths}    -> [[" ", ToFilename(P)] || P <- Paths]; %% FIXME: quote
        %% Convenience.  This could be abstracted behind a formatter.
        {to_includes,EPaths}    -> to_includes(Compile, EPaths)
    end.
        
%% That function is then used to flatten all variables in the shell
%% environment.
compile_env(Compile, Env) ->
    maps:map(
      fun(_,Format) when is_function(Format) -> Format(fun(Spec) -> compile_path(Compile, Spec) end);
         (_,ConvSpec={Type,_}) when is_atom(Type) -> compile_path(Compile, ConvSpec);
         (_,Other) -> Other end,
      Env).
              


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
    need_vals(Eval, Deps),
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




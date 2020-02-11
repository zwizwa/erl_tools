-module(redo).
-export([pull/2, need/2,
         push/3, push/2,
         start_link/1, handle_outer/2, handle/2,
         file_changed/3,
         read_file/2, is_regular/2,
         from_filename/1, to_filename/1,
         update_rule/3, update_file/1,
         run/2,
         gcc_deps/1,
         import/1,
         get/2, set/3,
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

debug(_F,_As) ->
    %% log:info(_F,_As),
    ok.

%% -------- CORE

%% Main entry point.  See test(ex1) below.
pull(Pid, Products) ->
    obj:call(Pid, {pull, Products}, 6124).

%% Trigger update of dependent products from external change notification
push(Pid, Changed, NotChanged) ->
    obj:call(Pid, {push, Changed, NotChanged}, 6123).
%% Providing NotChanged is just an optimization.  Not filling those in
%% will let the network determine change statuse.
push(Pid, Changed) ->
    push(Pid, Changed, []).


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
        {Pid, {pull, Products}} ->
            ok = obj:call(Eval, start_pull),
            obj:reply(Pid, {ok, parallel_eval(Eval, Products)}),
            State;
        {Pid, {push, Changed, NotChanged}} ->
            %% Push is added as an optimization to prune the network.
            %% Pusher can supply information about changed/nochanged.
            %% Any missing information in NotChanged is polled from
            %% stamp info.  Note that any changed items not in Changed
            %% will not cause recomputation unless they are
            %% accidentally part of some other evaluation path.
            case obj:call(Eval, {need_update, Changed}) of
                error ->
                    %% Push requires there to be a dependency network,
                    %% i.e. it only works after "priming" with pull.
                    %% If this fails and you know the outputs, use
                    %% pull instead.
                    obj:reply(Pid, error),
                    State;
                {ok, NeedUpdate} ->
                    ok = obj:call(Eval, {start_push, Changed, NotChanged}),
                    _ = need(Eval, Changed),  %% Make sure stamps are updated,
                    obj:reply(Pid, {ok, parallel_eval(Eval, NeedUpdate)}),
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
       State = #{update := ProductToUpdateFun}) ->
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
            UpdateFun = ProductToUpdateFun(Product),
            RedoPid = self(),
            Deps = maps:get({deps, Product}, State, []),
            spawn_link(fun() -> worker(RedoPid, Product, UpdateFun, Deps) end),
            maps:put({phase, Product}, {waiting, [CallPid]}, State)
    end;


%% Insert update computed by worker and notify waiters.
handle({worker_done, Product, PhaseUpdate}=Msg, State) ->
    {changed, Changed} = maps:get({phase, Product}, PhaseUpdate),
    debug("worker_done: ~p~n", [{Product,PhaseUpdate}]),
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
handle({Pid,{get,Tag}}, State) ->
    obj:reply(Pid, maps:get({product, Tag}, State)),
    State;
handle({Pid,{set,Tag,Val}}, State) ->
    obj:reply(Pid, ok),
    maps:put({product, Tag}, Val, State);

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
        lists:foldr(
          fun(C, S) -> maps:put({phase,C}, {changed, true}, S) end,
          State2, Changed),
    State3;

%% Get the dependencies in a form compatible with depgraph.erl
%% Inverted dependencies are cached.  Not quite sure if that is really
%% a necessary optimization, but it is probably good to have a "tap
%% point" for that happy path.
handle({Pid, {need_update, InputProducts}}, State) ->
    NeedUpdate =
        fun(IDeps) ->
                OutputProducts = depgraph:need_update_tc(IDeps, InputProducts),
                debug("need_update: ~p~n~p~n",[InputProducts, OutputProducts]),
                OutputProducts
        end,
    case maps:get(inv_deps, State, invalid) of
        invalid ->
            Deps =
                maps:fold(
                  fun({deps,Prod},Deps,Acc) -> [{Prod,'_',Deps} | Acc];
                     (_,_,Acc) -> Acc
                  end,
                  [], State),
            case Deps of
                [] ->
                    debug("need_update: no deps~n",[]),
                    obj:reply(Pid, error),
                    State;
                _ ->
                    IDeps = depgraph:invert_deps(Deps),
                    debug("need_update: recomputed inv_deps:~n~p~n",[IDeps]),
                    obj:reply(Pid, {ok, NeedUpdate(IDeps)}),
                    maps:put(inv_deps, IDeps, State)
            end;
        IDeps ->
            debug("need_update: using cached inv_deps~n",[]),
            obj:reply(Pid, {ok, NeedUpdate(IDeps)}),
            State
    end;

%% Allow raw get/set from protected context.
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State).


%% Worker process responsible of producing the target.
worker(RedoPid, Product, Update, OldDeps) ->
    %% need/2 will cause parallel eval of the dependencies.
    NeedUpdate =
        case OldDeps of
            [] -> true; %% First run or polling
            _  -> need(RedoPid, OldDeps)
        end,
    debug("~p: need update: ~p~n", [Product, NeedUpdate]),
    %% It's simplest to compute the state update here.
    PhaseUpdate =
        case NeedUpdate of
            false ->
                #{ {phase, Product} => {changed, false} };
            true ->
                {Changed, NewDepsUnsorted} = Update(RedoPid),
                debug("worker: update changed: ~p~n", [Changed]),
                NewDeps = tools:unique(NewDepsUnsorted),
                %% Special-case the happy path.  Keeping track of dep
                %% graph changes allows caching the inverted dep graph
                %% for "push" operation and allows to prune a need/2
                %% call to stamp the new deps.
                case NewDeps == OldDeps of
                    true ->
                        #{{phase, Product} => {changed, Changed}};
                    false ->
                        debug("worker: deps changed: ~p~n", [{OldDeps,NewDeps}]),
                        _ = need(RedoPid, NewDeps),
                        #{{phase, Product} => {changed, Changed},
                          {deps,  Product} => NewDeps,
                          inv_deps         => invalid}
                end
        end,
    RedoPid ! {worker_done, Product, PhaseUpdate},
    ok.


             




%% Also called "ifchange".
need(RedoPid, Deps) ->
    DepsUpdated = parallel_eval(RedoPid, Deps),
    NeedUpdate =
        lists:any(
          fun(Updated)-> Updated end,
          maps:values(DepsUpdated)),
    debug("need: ~p~n",[{NeedUpdate,DepsUpdated}]),
    NeedUpdate.

parallel_eval(RedoPid, Products) ->
    tools:pmap(
      fun(P) -> obj:call(RedoPid, {eval, P}) end,
      Products).
    

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

set(RedoPid, Tag, Val)    -> obj:call(RedoPid, {set,Tag,Val}).
get(RedoPid, Tag)         -> obj:call(RedoPid, {get,Tag}).



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

read_file(RedoPid, RelPath) ->
    {ok, AbsPath} = obj:call(RedoPid, {find_file, RelPath}),
    file:read_file(AbsPath).

is_regular(RedoPid, RelPath) ->
    {ok, AbsPath} = obj:call(RedoPid, {find_file, RelPath}),
    filelib:is_regular(AbsPath).

%% I would like to keep the "changed" predicate as abstract as
%% possible.  I currently see two ways to do this: filesystem
%% timestamp or file hash, and file hash seems more general.  See also
%% 'updated' case in handle/2 which is needed to prime the table.
file_changed(RedoPid, Product, RelPath, Stamp) ->
    %% Needs to go outside of next call to avoid re-entry.
    {ok, Bin} = read_file(RedoPid, RelPath),
    New = Stamp(Bin),
    MaybeOld = obj:call(RedoPid, {set_stamp, Product, New}),
    Changed = {ok, New} /= MaybeOld,
    %% debug("file_changed ~p~n", [{Product, RelPath, Changed, New, MaybeOld}]),
    Changed.
%% Default is md5 hash.
file_changed(RedoPid, Product, RelPath) ->
    file_changed(RedoPid, Product, RelPath,
                 fun(Bin) ->
                         tools:hex(crypto:hash(md5, Bin))
                 end).
                         

%% Generic build command dispatch.
run(RedoPid, {bash, Cmds}) ->
    {ok, Dir} = obj:call(RedoPid, {find_file, ""}),
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
    Changed1 = pull(Pid, [ex2]), debug("state1: ~p~n", [obj:dump(Eval)]),
    Changed2 = pull(Pid, [ex2]), debug("state2: ~p~n", [obj:dump(Eval)]),

    %% The afterthought "push" mode.
    Changed3 = push(Pid, [ex1], []),

    unlink(Pid),
    exit(Pid, kill),
    {Changed1,Changed2,Changed3};


%% FIXME: I think that "push" is wrong because it is not transitive.
%% It needs to close up to the point that.  Maybe just run the network
%% in reverse explicitly?


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




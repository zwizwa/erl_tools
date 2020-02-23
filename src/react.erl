%% Incremental UI rendering ala react can be implemented on top of the
%% memoizing evaluator by introducing a side effect and modifying the
%% propagation of change.

%% Basically, an incremental evaluator is the same as a
%% non-incremental evaluator except for one change: "smart containers"
%% can either send incremantal updates via a side-channel, or
%% re-render and let their parent container make a similar decision.

%% It is surprising to me that this is simple to express with a
%% pruning memoizing evaluator, but was completely non-trivial to
%% discover.  In fact the problem seemed "upside down": we DON'T just
%% propagate if there is change!

-module(react).
-export([test/1, update/5, compile/4]).


%% Split the idea into two parts:

%% UPDATE function.
%%
%% What needs to be done:
%%
%% - generic evaluator that can implement the side effects such that
%%   expression construction is just applicative.
%%
%% focus on apply.  apply knows about:
%% - the evaluation status of the arguments
%% - the (pure) constructor function
%% - the side channel for incremental updates


%% Eval:        The redo evaluator
%% SideChannel: Side channel receiving update commands
%% Cons:        The pure data constructor
%% OutVar:      Output var name
%% InVars:      Input var names

%% Some properties:

%% - The pure data constructor gets {Var,Val} pairs where the Var are
%%   unique redo node names.  This makes it possible to name nodes,
%%   for later side-channel updates.

%% The redo Eval is at the end here to allow for lambda-lifted
%% "reloadable closures".
update(SideChannel, Cons, OutVar, InVars, Eval) ->
    Map = redo:changed(Eval, InVars),
    ChangeList = maps:values(Map),
    false = lists:member(error, ChangeList),
    case lists:member(false, ChangeList) of
        false ->
            %% If they all changed, just re-render.  That also
            %% handles the initial rendering case.
            InVals = [{V,redo:need_val(Eval, V)} || V <- InVars],
            OutVal = Cons(InVals),
            redo:put_val(Eval, OutVar, OutVal),
            true;
        true ->
            %% If there is one that did not change, we know
            %% that there was no render before.
            lists:foreach(
              fun({_,false}) ->
                      %% Ignore the ones that did not change.
                      ok;
                 ({Var,true}) ->
                      %% Update the others in place.
                      Val = redo:need_val(Eval, Var),
                      SideChannel(Var,Val)
              end,
              maps:to_list(ChangeList)),
            %% And signal upstream that no more changes are
            %% necessary.
            false
    end.

%% COMPILE function

%% This maps {Cons,InVars} -> Outvar, against the Redo/SideChannel
%% context.  Here Cons is the pure function taking {Var,Val} pairs and
%% producting a Val, and InVars, OutVars are the variable names.

%% We use a function 

%% It would be nice to have an applicative description that can
%% construct a redo network from a series of function compostions.
%% That leaves everything parameterizeable. ( Using a data type does
%% not, for instance.  It would require a custom interpreter. )

%% Essentially we need to convert to ANF, creating an intermediate
%% node for each.

%% The update/1 functions that get produced are bound to SideChannel.
%% We need Redo (not Eval!) to create nodes.

%% This calls make_var twice.  Once to get the node name, and a second
%% time to install the function.,

compile(Redo, SideChannel, Cons, InVars) ->
    OutVar = {var, OutVarTag} = redo:make_var(Redo),
    redo:make_var(
      Redo,
      {fun ?MODULE:update/5, [SideChannel, Cons, OutVar, InVars]},
      OutVarTag).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(compile) ->
    Inputs =
        fun (Var) ->
                redo:update_pure(
                  Var,[],
                  fun([]) -> {initval,Var} end
                 )
        end,
    {ok, Redo} = redo:start_link(#{ update => Inputs}),
    SideChannel = fun(KV) -> log:info("side: ~p~n", [KV]) end,

    %% 'A' is the function application syntax of the expression
    %% language we're building, and 'C' is a type-indexed set of
    %% constructors that is a model for XHTML (list-of-nodes structure
    %% + tagged nodes for imperative update).

    C = fun(Type) -> fun(KVList) -> {Type, KVList} end end,
    A = fun(Type,Args) ->
                OutVar = compile(Redo, SideChannel, C(Type), Args),
                log:info("comp: {~p, ~p} -> ~p~n", [Type, Args, OutVar]),
                OutVar
        end,
    
    %% The expressions then become straighforward
    Body =
        A(body,
          [A(list,   [var1, var2]),
           A(select, [var3, var4])]),
    %% Inputs will have to be defined.
    lists:foreach(
      fun(V) -> redo:update_pure(V,[],fun([]) -> {val,V} end) end,
      [var1, var2, var3, var4]),

    %% Pull once to do initial render
    Render = redo:get(Redo, Body),
    log:info("render: ~p~n", [Render]),
    
    

    unlink(Redo),
    exit(Redo, kill),
    ok;


test(Spec) ->
    throw({?MODULE,bad_test,Spec}).



                            
    

%% The tree specification is done using a datatype that can then be
%% converted to a network.  It's a little too awkward to do it using
%% function encoding.


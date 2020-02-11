-module(depgraph).
-export([invert_deps/1,
         need_update_tc/2,
         test/1]).


%% Invert a dependency network as a basis to implement "push" form.
invert_deps(Procs) ->
   lists:foldr(
     fun({_Out, _Fun, Ins}=Proc, Map0) ->
             lists:foldr(
               fun(In, Map1) ->
                       L = maps:get(In, Map1, []),
                       maps:put(In, [Proc|L], Map1)
               end,
               Map0, Ins)
     end,
     #{}, Procs).

%% FIXME: Remove this after removing stale code that depends on this.
%%
%% This does not perform a transitive closure and can be used in
%% recursive evaluation, but is currently not very useful.  It seems
%% simpler to walk the inverted dep tree directly, folding over some
%% state to perform memoization.
%%
%% %% Given an inverted dependency map, compute the needed updates.
%% need_update(InvDepMap, Inputs) ->
%%     lists:foldr(
%%       fun(Input, Map0) ->
%%               lists:foldr(
%%                 fun({Out,Fun,Args}, Map1) ->
%%                         maps:put(Out, {Fun,Args}, Map1)
%%                 end,
%%                 Map0, maps:get(Input, InvDepMap))
%%       end,
%%       #{}, Inputs).


%% For redo.erl we already have an evaluator, and only need a
%% transitive closure from network inputs to network outputs, which
%% then in turn is used to drive the "pull" evaluator.

need_update_tc(InvDepMap, Inputs) ->
    %% log:info("need_update: invdeps: ~p~n", [InvDepMap]),
    maps:keys(
      lists:foldl(
        fun(Input, GlobalOutputs) ->
                need_update_tc1(InvDepMap, Input, GlobalOutputs)
        end,
        #{}, Inputs)).

need_update_tc1(InvDepMap, Input, GlobalOutputs) ->
    %%log:info("need_update_tc1: node ~p (~p)~n", [Input,length(maps:to_list(InvDepMap))]),
    case maps:get(Input, InvDepMap, []) of
        [] ->
            %% This is not influencing anything, so it must be a
            %% network output.
            %% log:info(" - output~n"),
            maps:put(Input, true, GlobalOutputs);
        DepInfo ->
            Outputs = [Out || {Out,_Fun,_Ins} <- DepInfo],
            %% This is influencing another network node, so doesn't
            %% need to be included in the network output list.
            %% log:info(" - input -> ~p~n", [InvDeps]),
            lists:foldl(
              fun(O, Os) -> need_update_tc1(InvDepMap, O, Os) end,
              GlobalOutputs, Outputs)
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

test(all) ->
    [{T,test(T)} || 
        T <- [invert_deps,
              {need_update,[]},
              {need_update,[in1]},
              {need_update,[in1,in2]}]];
test(invert_deps) ->
    invert_deps(
      [{out1, f1, [in1, in2]},
       {out2, f2, [in1, in2]}]);
test({need_update,Ins}) ->
    need_update_tc(test(invert_deps),Ins).



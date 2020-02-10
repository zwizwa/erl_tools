-module(depgraph).
-export([invert_deps/1, need_update/2, test/1]).


%% Dependency inverter.
%%
%% Computations are simplest to express in "pull" form, but what we
%% want is "push" form, i.e. "inverted", such that we can compute what
%% to update when input events come in.  This makes the conversion and
%% produces a map from inputs to Procs.

invert_deps(Procs) ->
   lists:foldr(
     fun({_Res, _Fun, Args}=Proc, Map0) ->
             lists:foldr(
               fun(Arg, Map1) ->
                       L = maps:get(Arg, Map1, []),
                       maps:put(Arg, [Proc|L], Map1)
               end,
               Map0, Args)
     end,
     #{}, Procs).

%% Given an inverted dependency map, compute the needed updates.
need_update(InvDepMap, Inputs) ->
    lists:foldr(
      fun(Input, Map0) ->
              lists:foldr(
                fun({Out,Fun,Args}, Map1) ->
                        maps:put(Out, {Fun,Args}, Map1)
                end,
                Map0, maps:get(Input, InvDepMap))
      end,
      #{}, Inputs).





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
    need_update(test(invert_deps),Ins).



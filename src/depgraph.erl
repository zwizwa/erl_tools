-module(depgraph).
-export([invert/1,
         affected/2,
         test/1]).

%% FIXME: rewrite previous invert_deps/2 and need_update/2 to use
%% invert and affected.

%% Invert a dependency graph, e.g. to implement "push" data flow.
invert(Procs) ->
   lists:foldr(
     fun({Out, _Fun, Ins}, Map0) ->
             lists:foldr(
               fun(In, Map1) ->
                       L = maps:get(In, Map1, []),
                       maps:put(In, [Out|L], Map1)
               end,
               Map0, Ins)
     end,
     #{}, Procs).

%% FIXME: find caller to need_update/2
%% Instead, use transitive closure.

%% Evaluating "push" can be done in two steps: given reverse map, find
%% transitive closure of the inputs, then pull those.

debug(_F,_A) ->
    %% log:info(_F,_A),
    ok.

affected(InvDepMap, Inputs) ->
    debug("affected: invdeps: ~p~n", [InvDepMap]),
    %% Recurse over leaf nodes that do not have dependencies.
    Closure = 
        lists:foldl(
          fun(Input, GlobalOutputs) ->
                  add_global_outputs(InvDepMap, Input, GlobalOutputs)
          end,
          #{}, Inputs),
    %% If any of those are inputs, they need to be removed.  This
    %% happens e.g. when the Input doesn't occur in the map at all.
    Outputs =
        lists:foldr(
          fun maps:remove/2,
          Closure,
          Inputs),
    maps:keys(Outputs).


add_global_outputs(InvDepMap, Input, GlobalOutputs) ->
    debug("add_global_outputs: node ~p (~p)~n", [Input,length(maps:to_list(InvDepMap))]),
    case maps:get(Input, InvDepMap, []) of
        [] ->
            %% This is not influencing anything, so it must be a
            %% network output.
            debug(" - output~n",[]),
            maps:put(Input, true, GlobalOutputs);
        Outputs ->
            %% This is influencing another network node, so doesn't
            %% need to be included in the network output list.
            debug(" - input -> ~p~n", [Outputs]),
            lists:foldl(
              fun(O, Os) -> add_global_outputs(InvDepMap, O, Os) end,
              GlobalOutputs, Outputs)
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

test(all) ->
    [{T,test(T)} || 
        T <- [invert_deps,
              {need_update,[]},
              {need_update,[a]},
              {need_update,[a,b]}]];
test(invert_deps) ->
    invert(
      [{c, f1, [a, b]},
       {d, f2, [c]}]);
test({affected,Ins}) ->
    affected(test(invert_deps),Ins).



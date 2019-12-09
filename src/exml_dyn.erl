-module(exml_dyn).
-export([fold_el/3,
         map_el/2,
         render_el/2,
         deps_el/2]).


%% Dynamic ehtml is ehtml with possible "holes" in every constructor
%% position.  The holes are expressed as model variable names + render
%% function.  ehtml is rendered by combining with an environment.

%% Making the variables explicit is what allows for caching if those
%% varialbes don't change, assuming the functions are pure.
%% Performing the dereference outside of the render function keeps
%% functions pure.

render_el(Env, El) ->
    map_el(fun(Case,Val) -> render(Env,Case,Val) end, El).


%% The "datatype" is defined implicitly through fold and map operations.

%% Fuctor (splices in single element/attribute context).
map_el(F, {dyn,_,_}=E) ->
    F(els,E);
map_el(F, {Tag,As,Es}) ->
    As1  = case As  of {dyn,_,_} -> F(attrs,As); _ -> map_attrs(F, As) end,
    Es1  = case Es  of {dyn,_,_} -> F(els,Es);   _ -> map_els(F, Es) end,
    {Tag, As1, Es1};
map_el(_Env, E) ->
    E.

map_els(F, Es) ->
    lists:append(
      lists:map(
        fun(E) -> map_el(F, E) end,
        Es)).

map_attrs(F, As) ->
    lists:append(
      lists:map(
        fun(A={dyn,_,_}) -> F(attrs,A);
           ({K,V}) -> [{K,V}]
        end,
        As)).

%% Fold
fold_el(F, S, E={dyn,_,_}) -> F(els,E,S);
fold_el(F, S1, {_Tag,As,Es}) ->
    S2 = case As  of {dyn,_,_} -> F(attrs,As,S1); _ -> fold_attrs(F, S1, As) end,
    S3 = case Es  of {dyn,_,_} -> F(els,Es,S2);   _ -> fold_els(F, S2, Es) end,
    S3;
fold_el(_F, S, _E) -> S.
fold_attrs(F, S0, As) ->
    lists:foldl(
      fun(A={dyn,_,_}, S) -> F(attrs,A,S);
         ({_K,_V}, S) -> S
      end,
      S0, As).
fold_els(F, S0, Es) ->
    lists:foldl(
      fun(E, S) -> fold_el(F, S, E) end,
      S0, Es).




%% There are two render cases: elements or attributes.  Conceptually
%% they are the same in that they cannot be distinguished in the
%% model, but at the DOM level they of course need special attention.

render(Env, Case, Dyn0) ->
    case {Case,Dyn0} of
        %% Abstract mapping is necessary to be able to work on render
        %% inputs instead of diffing render outputs in the update
        %% regime.  For initial render, we need to fully expand.  Note
        %% that keys are always kept in sorted order, which makes it
        %% possible to implement "insert" unambiguously at the DOM end.
        {els,
         {dyn, {map,F}, VarListSpec}} ->
            Vars =
                case VarListSpec of
                    {select, Select} ->
                        lists:sort(Select(maps:keys(Env)));
                    _ ->
                        VarListSpec
                end,
            %% Then recurse
            lists:append(
              lists:map(
                fun(Var) -> render(Env, els, {dyn, F, Var}) end,
                Vars));
        %% Element render from a single variable. The nodes in the
        %% viewmodel are in 1-1 correspondence with DOM elements.
        {els,
         {dyn, F, Var}} ->
            {T,A,E} = F(maps:get(Var, Env)),
            [{T,[{id,io_lib:format("~p",[Var])}|A],E}];

        %% FIXME: Support attributes once elements work properly.
        {attrs,
         _} ->
            throw({exml_dyn_attrs,Dyn0})
    end.           
                

            


%% Incremental render.

%% Since some variable collections can be implicit, it is necessary to
%% generate the variable list from the model.  The foldee here knows
%% how to unpack 'select'.

deps_el(Env, El) ->
    fold_el(
      fun(_Type, Dyn={dyn,_,List}, Acc) ->
              Deps = 
                  case List of
                      {select,Select} ->
                          Select(maps:keys(Env));
                      Vars ->
                          Vars
                  end,
              [{Deps,Dyn} | Acc]
      end,
      [], El).



%% Notes.  Mostly simplicications.
%%
%% - Do not generate tags dynamically.  If this is necessary, go one
%%   level up and embed it in an element.
%%
%% - Not sure what to do with splicing.


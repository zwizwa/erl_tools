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
    map_el(fun(Case,Val) -> render_case(Env,Case,Val) end, El).


%% The "datatype" is defined implicitly through fold and map operations.

%% Fuctor (splices in single element/attribute context).
map_el(F, {dyn,_,_}=E) ->
    F(els,E);
map_el(F, {Tag,As,Es}) ->
    Tag1 = case Tag of {dyn,_,_} -> F(tag,Tag);  _ -> Tag end,
    As1  = case As  of {dyn,_,_} -> F(attrs,As); _ -> map_attrs(F, As) end,
    Es1  = case Es  of {dyn,_,_} -> F(els,Es);   _ -> map_els(F, Es) end,
    {Tag1, As1, Es1};
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
fold_el(F, S0, {Tag,As,Es}) ->
    S1 = case Tag of {dyn,_,_} -> F(tag,Tag,S0);  _ -> S0 end,
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


%% Initial render.

%% Mapping is explicit, but done in such a way that elements all have
%% their own identity so this can be used for incremental
%% updates. This is accomplished by using Select to distill a list of
%% variable names from the current model.

render(Env, {dyn, {map, F}, List}) ->
    lists:append(
      lists:map(
        fun(Key) -> render(Env, {dyn, F, [Key]}) end,
        case List of
            {select, Select} ->
                Select(maps:keys(Env));
            _ ->
                List
        end));

render(Env, {dyn, F, Vars}) ->
    Arg = [{Var,maps:get(Var,Env)} || Var <- Vars],
    F(Arg).

render_case(Env, Case, Dyn={dyn,_,_Vars}) ->
    %% log:info("render ~p~n",[_Vars]),
    Fragment = render(Env, Dyn),
    %% Several constructor types are supported.  Each has its own
    %% update procedure.
    %% FIXME
    case Case of
        %% Not sure if it make sense to allow tags to be updated.
        tag   -> ok;
        %% To replace attributes, identify the parent element and set
        %% attributes one by one.
        attrs -> ok;
        %% To replace elements, identify the parent element, locate
        %% the first old element, insert new elements before, then
        %% remove old elements.
        els   -> ok
    end,
    Fragment.


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

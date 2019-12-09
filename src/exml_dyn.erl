-module(exml_dyn).
-export([fold_el/3,
         map_el/2,
         render_el/2,
         deps_el/2,
         update_el/3
]).


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

render(Env, Case, Dyn) ->
    case {Case,Dyn} of
        %% Incremental model rendering needs abstract mapping.
        %% However at initial render time we need to expand fully.
        {els, {dyn, {map, _}, _}} ->
            render_map(Env, Dyn);
            
        %% Element render from a single variable. The nodes in the
        %% viewmodel are in 1-1 correspondence with DOM elements.
        {els, {dyn, F, Var}} ->
            render_dyn({dyn, F, Var}, maps:get(Var, Env));

        %% FIXME: Support attributes once elements work properly.
        {attrs, _} ->
            throw({exml_dyn_attrs,Dyn})
    end.

render_dyn({dyn, F, Var}, Val) ->
    [add_id(Var, F(Val))].


%% List construction always has two components: an element rendering
%% function and a list header.  Allow for 'span' to be a default here.
wrap_and_map(F) when is_function(F) ->
    {fun(Els) -> {'span',[],Els} end, F};
wrap_and_map({Parent,F}) when is_function(Parent) and is_function(F) ->
    {Parent,F}.


expand_select(Env, Select) ->
    {Cid, Subs} = Select(maps:keys(Env)),
    %% Sorted list requirement makes DOM insert unambiguous.
    {Cid,[ Cid ++ [Sub] || Sub <- lists:sort(Subs)]}.

render_map(Env, {dyn, {map, MapSpec}, VarListSpec}) ->
    {Parent, F} = wrap_and_map(MapSpec),

    {ContainerPath,Vars} =
        case VarListSpec of
            {select, Select} ->
                expand_select(Env, Select);
            _ ->
                VarListSpec
        end,
    %% Then recurse to obtain list elements..
    Els = 
        lists:append(
          lists:map(
            fun(Var) -> render(Env, els, {dyn, F, Var}) end,
            Vars)),
    %% .. and wrap the parent container.  To keep it simple: these
    %% wrappers are not dynamic.
    [add_id(
       ContainerPath,
       Parent(Els))].



p(Path) ->                
    tools:format_binary("~p",[Path]).
add_id(Path, {T,A,E}) ->
    {T,[{id,p(Path)}|A],E}.
    


            


%% Incremental render.

%% Since some variable collections can be implicit, it is necessary to
%% generate the variable list from the model.  The foldee here knows
%% how to unpack 'select'.

deps_el(Env, El) ->
    maps:from_list(deps_el_(Env,El)).
deps_el_(Env, El) ->
    fold_el(
      fun(_Type, _Dyn={dyn,{map,MapSpec},ListSpec}, Acc0) ->
              {_, F} = wrap_and_map(MapSpec),
              Vars = 
                  case ListSpec of
                      {select,Select} ->
                          {_, Vs} = expand_select(Env, Select),
                          Vs;
                      _ ->
                          ListSpec
                  end,
              lists:foldr(
                fun(Var,Acc1) -> [{Var,{dyn,F,Var}} | Acc1] end,
                Acc0,
                Vars)
      end,
      [], El).


%% Compute rendered incremental exml update in wsforth command form.
update_el(Dyn,M0,M1) ->
    Deps = deps_el(M1,Dyn),
    Diff = diff:diffi(M0,M1),
    Edits =
        lists:map(
          fun({update,Var,_,Val}) ->
                  HtmlBin = html_var(Deps,Var,Val),
                  [HtmlBin,render,p(Var),ref,replace];
             ({insert,Var,Val}) ->
                  HtmlBin = html_var(Deps,Var,Val),
                  [HtmlBin,render,p(parent(Var)),ref,insert];
             ({delete,Var}) ->
                  [p(Var),delte]
          end,
          Diff),
    %% log:info("M0:~p~n",[M0]),
    %% log:info("M1:~p~n",[M1]),
    %% log:info("deps:~p~n",[Deps]),
    log:info("diff:~p~n",[Diff]),
    %% log:info("edits:~p~n",[Edits]),
    Edits.

parent(Path) ->
    lists:reverse(
      tl(lists:reverse(Path))).

html_var(Deps,Var,Val) ->
    iolist_to_binary(
      exml:exml(
        hd(render_dyn(maps:get(Var,Deps),Val)))).



%% Notes.  Mostly simplicications.
%%
%% - Do not generate tags dynamically.  If this is necessary, go one
%%   level up and embed it in an element.
%%
%% - Not sure what to do with splicing.


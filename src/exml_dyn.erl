-module(exml_dyn).
-export([fold_el/3,
         map_el/2,
         render_el/2,
         deps_el/2,
         update_el/3,
         invert_deps/1,
         need_update/2,
         test/1
]).


%% A simple templating language supporting fast incremental updates.

%% This is an iteration in a series of attempts to do "react without
%% html", by focusing on viewmodel changes instead of converting html
%% diffs into DOM update operations.

%% The approach used here focuses on creating a dataflow structure
%% that can be used to perform fine-grained view updates.

%% Note that this is very different from the react-style html diffing,
%% which is much more powerful, but also necessarily O(N).  This
%% module focuses on finding an O(1) happy path and thus can be used
%% for a higher event rate while still enabling fairly complex view
%% structures.

%% The basic structure:

%% - The viewmodel is in 1-1 correspondence with XHTML view.  Every
%%   variable in the viewmodel refers to an element in the DOM.  They
%%   are related through a render function mapping the value to ehtml.
%%
%% - That scalar model is extended with some structural abstractions,
%%   e.g. 'list' being the first one.
%%
%% - The usual 1-n relation (one conceptual data element maps to many
%%   views), is implemented separately.
%%

%% Note that the viewmodel can be ephemeral, as it can be generated
%% from the datamodel for initial rendering, and datamodel updates can
%% be translated immediately into viewmodel updates.

%% I.e. the idea is centered around these maps, where 'd' indicates
%% a change or edit (differential).

%%  DM ->  VM ->  View 
%% dDM -> dVM -> dView







%% Conclusion: diffing html structure as is done in react is much more
%% powerful than what is done here, but diffing models seems to be a
%% worthwhile approach for simpler "panel-style" applications that
%% have a resonably constant structural backbone.

%% While we need to support every structural modification explicitly
%% (e.g. add/remove element in list), it is possible to create
%% explicit dependency lists, making updates very efficient: in the
%% happy path, a scalar can map directly to a collection of update
%% functions.

%% At this point only one structural element is supported: lists.  It
%% is suprising how much can be done with just this.

%% So in a nutshell: the central idea here is to create 


%% Core ideas:

%% - viewmodel scalar variables map directly to DOM elements
%%
%% - structural formatters (e.g. 'list' is functor structure)
%%   parameterized by collections of scalars.
%%
%% - abstract queries to construct collections of scalars
%%
%% - don't mess with generating finer grain exml (attributes, list
%%   splices, ...).
%%
%% - if additional local (javascript) behavior is necessary, it can
%%   probably be uploaded in an ad-hoc fashion.
%%
%% - no third party webserver (use serv_ws)
%%
%% - no heavyweight client side library (use stack_ws).


%% Recursive expansion is not used. Instead, effort is made to
%% identify a couple of useful structural formatters:
%%
%% - single elements
%% - lists (any parent+child xhml,svg structure)
%% - 2D tables (TODO)
%% - arbitrarily nested lists (TODO, maybe not even necessary)


%% The basic data type exml representing xhtml, together with an
%% extension {dyn,_,_} to represent a dynamic sublanguage for
%% generating exml nodes.

%% exml is rendered by combining with the viewmodel (environment).

%% Making the variables explicit allows for fine-grained caching and
%% updates.  All render functions are pure.

render_el(Env, El) ->
    map_el(fun(Val) -> render(Env, Val) end, El).


%% The datatype is defined implicitly through fold and map operations.
%% Note that these are splicing operations where possible, but this is
%% not used in "render" and "update" code.

%% Map
map_el(F, {dyn,_,_}=E) ->
    F(E);
map_el(F, {Tag,As,Es}) ->
    {Tag, As,
     lists:map(
       fun(E) -> map_el(F, E) end,
       Es)};
map_el(_Env, E) ->
    E.

%% Fold
fold_el(F, S, E={dyn,_,_}) ->
    F(E, S);
fold_el(F, S0, {_Tag,_As,Es}) ->
    lists:foldl(
      fun(E, S) -> fold_el(F, S, E) end,
      S0, Es);
fold_el(_F, S, _E) ->
    S.




%% There are two render cases: elements or attributes.  Conceptually
%% they are the same in that they cannot be distinguished in the
%% model, but at the DOM level they of course need special attention.

render(Env, Dyn) ->
    case Dyn of
        %% Incremental model rendering needs abstract mapping.
        %% However at initial render time we need to expand fully.
        {dyn, {list, _}, _} ->
            render_list(Env, Dyn);
        
        %% Element render from a single variable. The nodes in the
        %% viewmodel are in 1-1 correspondence with DOM elements.
        {dyn, F, Var} ->
            render_scalar({dyn, F, Var}, maps:get(Var, Env))
    end.

render_scalar({dyn, F, Var}, Val) ->
    add_id(Var, F(Var, Val)).


%% Normal form for list rendering is a header/parent + a
%% transformation function for each element.  Allow for 'span' to be a
%% default here.
nf_list_render(ElementF)
  when is_function(ElementF) ->
    nf_list_render(#{ element => ElementF });
nf_list_render(Spec=#{ element := _ }) ->
    maps:merge(
      #{ parent => fun(Els) -> {'span',[],Els} end },
      Spec).

%% Normal form for list specification is prefix + var list.
%%
%% 1. Sorted list requirement makes DOM insert unambiguous.  Keys
%% should be serialized such that text rep in Javascript can use
%% lexical order.
%%
%% 2. The Subs keys might contain structure, but for lists that is
%% ignored except for the sort order.
%%
%% 3. For any container, the parent node is obtained by chopping off
%%    the rightmost coordinate.
nf_vars(Env, {select, Select}) ->
    {Prefix, Subs} = Select(maps:keys(Env)),
    #{prefix => Prefix,
      vars   => [Prefix ++ [Sub] || Sub <- lists:sort(Subs)]};
nf_vars(_Env, #{prefix := _, vars := _}=Spec) ->
    Spec.




%% FIXME: Use the dep generation to do the render?  Code is very
%% similar, but that would require rendering the head node also.
%% Maybe unify?

render_list(Env, {dyn, {list, Listspec}, VarsSpec}) ->

    %% Expand parameterization to normal form.
    #{ parent  := ParentF, 
       element := ElementF } = 
        nf_list_render(Listspec),
    #{ prefix := Prefix, vars := Vars } = 
        nf_vars(Env, VarsSpec),

    %% Recurse to obtain list elements.
    Els = [render(Env, {dyn, ElementF, Var}) || Var <- Vars],

    %% Wrap the parent container around the elements.
    El = ParentF(Els),
    
    %% Add the parent id.
    add_id(Prefix, El).


p(Path) ->                
    tools:format_binary("~p",[Path]).
add_id(Path, {T,A,E}) ->
    {T,[{id,p(Path)}|A],E}.
    


            


%% Incremental render.

%% Dependences are "compiled down" to the scalar level.  Because the
%% 'list' (and other) structures can be dynamic, it needs to be
%% rebuilt every time the model structure changes.

%% E.g. if the diff contains only updates, the previous dependency
%% list can be used.  If there are delete/insert operations, and
%% dynamic variable collections then this needs to be re-evaluated.

deps_el(Env, El) ->
    maps:from_list(deps_el_(Env,El)).
deps_el_(Env, El) ->
    fold_el(
      fun (_Dyn={dyn,{list,ListSpec},VarSpec}, Acc0) ->
              %% LIST
              %%
              %% The parent node is mostly just glue and is not
              %% updated.  We expand elements into individual scalar
              %% dynamic elements.
              #{ element := ElementF} = nf_list_render(ListSpec),
              #{ vars := Vars } = nf_vars(Env, VarSpec),
              lists:foldr(
                fun(Var,Acc1) -> [{Var,{dyn,ElementF,Var}} | Acc1] end,
                Acc0,
                Vars);
         (Dyn={dyn,_F,Var}, Acc0) ->
              %% SCALAR
              [{Var,Dyn} | Acc0]
      end,
      [], El).


%% Compute rendered incremental exml update in stack_ws command form.
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
    %% log:info("diff:~p~n",[Diff]),
    %% log:info("edits:~p~n",[Edits]),
    Edits.

%% Note that paths should be top first to preserve lexical order of
%% ascii representation of keys. 
parent(Path) ->
    lists:reverse(
      tl(lists:reverse(Path))).

html_var(Deps,Var,Val) ->
    iolist_to_binary(
      exml:exml(
        render_scalar(maps:get(Var,Deps),Val))).

%% Factored out.
invert_deps(Procs) ->
    depgraph:invert(Procs).
need_update(DepMap, Inputs) ->
    depgraph:affected(DepMap, Inputs).

test(test) ->
    ok;
test(Spec) ->
    throw({test,?MODULE,Spec}).


%% TODO:
%% - write render in terms of update
%% - re-use dependency lists
%% - table formatter
%% - is 'list' without 'select' actually useful?  (removed cases)


%% A second iteration.  Instead of making this ad-hoc, use actual
%% differential programming.  Differential lambda calculus.  I read a
%% paper on this a couple of years ago.
%% See math.txt 20170908
%% https://www.informatik.uni-marburg.de/~pgiarrusso/papers/pldi14-ilc-author-final.pdf
%% https://github.com/paf31/purescript-incremental-functions/blob/master/src/Data/Incremental.purs

%% So can this be done for Erlang?  If I understand correctly, given a
%% function from Term -> EXML, it would be possible to compute dTerm
%% -> dEXML.


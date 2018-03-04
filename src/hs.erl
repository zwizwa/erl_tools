%% Convert Erlang type annotations to Haskell.
%% FIXME:
%% 1. dump syntax to erlang term
%% 2. parse erlang in haskel and perform embedding there


-module(hs).
-export([parse/1,type/1,x/1]).


parse(Str) ->
    {ok,Toks,_} = erl_scan:string(Str),
    {ok, Form} = erl_parse:parse_form(Toks),
    Form.


%% x({attribute,_,type,{Name,T,_Vars}}) -> {Name,type(T)};
x(Str) ->
    Top = parse(Str),
    iolist_to_binary(typedef(Top)).

%% {atom(),_} is interpreted as a constructor for a new data type.

%% Unions take constructor names from tags.
typedef({attribute,L,type,{Name,{type,_,union,Ts},Vs}}) ->
    ["data ", type({type,L,Name,Vs})," = ", 
     lists:join(" | ", [cons(T) || T <- Ts])];

%% Special case for single-constructor types.
typedef({attribute,L,type,{Name,Alt={type,_,tuple,[{atom,_,_},_]},Vs}}) ->
    ["data ", type({type,L,Name,Vs})," = ", cons(Alt)];

%% Other forms are treated as aliases.  Unions are not allowed inside type nesting.
typedef({attribute,L,type,{Name,T,Vs}}) ->
    ["type ", type({type,L,Name,Vs})," = ", type(T)];

%% Function specs
typedef({attribute,_,spec,{{Fname,_Nargs},[FunType]}}) ->
    [atom_to_list(Fname), " :: ", type(FunType)].



%% Constructor syntax for sum types.     
cons({type,_,tuple,[{atom,_,Cons},T]}) ->
    [type_name(Cons), " (", type(T), ")"].
    

%% Erlang syntax structure is good enough for direct translation.
%% FIXME: tuple
type({type,L,'tuple',[T]}) -> type({user_type,L,s,[T]});  %% No single-element tuple in Haskell
type({type,_,'tuple',Ts}) -> ["(", lists:join(",",[type(T) || T<-Ts]),")"];
type({type,_,'list',[T]}) -> ["[", type(T), "]"];
type({type,_,'fun',[A,R]}) -> [type(A)," -> ",type(R)];
type({type,L,'product',As}) -> type({type,L,'tuple',As});
type({type,_,'union',_}=T) -> throw({only_toplevel_union,T});
type({type,_,T,[]}) -> type_name(T); %% Otherwise this is unit ().
type({type,_,T,Ts}) -> [type_name(T), "(", lists:join(" ", [type(T0) || T0 <- Ts]), ")"];
type({var,_,Var}) -> var_name(Var);
type({user_type,L,T,Vs}) -> type({type, L, T, Vs}). %% Same

%% Base type conversion.
%%type_name(nil) -> "[]";
type_name(Type) ->
    Bs = re:split(atom_to_list(Type),"_"),
    [to_upper_first(binary_to_list(B)) || B <- Bs].
to_upper_first([H|T]) ->
    [H1] = string:to_upper([H]),
    [H1|T].
var_name(Var) ->
    string:to_lower(atom_to_list(Var)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("hs.expect").
expect_test() ->
    expect:run_form(
      filename:dirname(?FILE)++"/hs.expect",
      fun hs_expect/0).
-endif.


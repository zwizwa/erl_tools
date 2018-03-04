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
%% Unions are only allowed as "data" definitions
%% FIXME: if this is done indirectly by referencing user types, the constructors can be properly named.
typedef({attribute,Line,type,{Name,{type,_,union,Ts},Vs}}) ->
    ["data ", type({type,Line,Name,Vs})," = ", 
     lists:join(
       " | ",
       [[cons_name(Name,N,T),
         " ", "(", type(T), ")"] 
        || {N,T} <- tools:enumerate(Ts)])];
typedef({attribute,Line,type,{Name,T,Vs}}) ->
    ["type ", type({type,Line,Name,Vs})," = ", type(T)].

cons_name(NamePrefix,N,T) ->
    case T of
        {user_type,_,Name,_} ->
            type_name(Name);
        _ ->
            io_lib:format("~s~p",[type_name(NamePrefix),N])
    end.

%% Erlang syntax structure is good enough for direct translation.
type({type,_,'union',_}=T) -> throw({only_toplevel_union,T});
type({type,_,'fun',[A,R]}) -> [type(A)," -> ",type(R)];
type({type,_,product,[A]}) -> type(A);  %% No single-element tuple in Haskell
type({type,_,product,As}) -> ["(",lists:join(", ",[type(A) || A <- As]),")"];
type({type,_,T,Ts}) -> [type_name(T), "(", lists:join(" ", [type(T0) || T0 <- Ts]), ")"];
type({var,_,Var}) -> var_name(Var);
type({user_type,Line,T,Vs}) -> type({type, Line, T, Vs}). %% Same

%% Base type conversion.
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


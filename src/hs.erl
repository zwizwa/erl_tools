%% Convert Erlang type annotations to Haskell.

-module(hs).
-export([parse/1,type/1,x/1]).


parse(Str) ->
    {ok,Toks,_} = erl_scan:string(Str),
    {ok, Form} = erl_parse:parse_form(Toks),
    Form.


%% x({attribute,_,type,{Name,T,_Vars}}) -> {Name,type(T)};
x(Str) ->
    Top = parse(Str),
    typedef(Top).
%% Unions are only allowed as "data" definitions
%% FIXME: if this is done indirectly by referencing user types, the constructors can be properly named.
typedef({attribute,Line,type,{Name,{type,_,union,Ts},Vs}}) ->
    iolist_to_binary(["data ", type({type,Line,Name,Vs})," = ", 
                      lists:join(
                        " | ",
                        [[io_lib:format("~s~p",[t(Name),N]),
                          " ", "(", type(T), ")"] 
                         || {N,T} <- tools:enumerate(Ts)])]);
typedef({attribute,Line,type,{Name,T,Vs}}) ->
    iolist_to_binary(["type ", type({type,Line,Name,Vs})," = ", type(T)]).

%% Erlang syntax structure is good enough for direct translation.
type({type,_,'union',_}=T) -> throw({only_toplevel_union,T});
type({type,_,'fun',[A,R]}) -> [type(A)," -> ",type(R)];
type({type,_,product,[A]}) -> type(A);  %% No single-element tuple in Haskell
type({type,_,product,As}) -> ["(",lists:join(", ",[type(A) || A <- As]),")"];
type({type,_,T,Ts}) -> [t(T), "(", lists:join(" ", [type(T0) || T0 <- Ts]), ")"];
type({var,_,Var}) -> v(Var);
type({user_type,Line,T,Vs}) -> type({type, Line, T, Vs}). %% Same

%% Base type conversion.
t(Type) ->
    Bs = re:split(atom_to_list(Type),"_"),
    [t0(binary_to_list(B)) || B <- Bs].
t0([H|T]) ->
    [H1] = string:to_upper([H]),
    [H1|T].
v(Var) ->
    string:to_lower(atom_to_list(Var)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("hs.expect").
expect_test() ->
    expect:run_form(
      filename:dirname(?FILE)++"/hs.expect",
      fun hs_expect/0).
-endif.


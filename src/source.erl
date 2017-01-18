-module(source).
-export([range/2,range/1,to_list/1,to_fold/1,map/2]).

%% Internal iterators -- dual of sink, represented as pairs with
%% delayed tails.

range(N) -> range(0,N).
range(E,N) when E < N -> fun() -> {E, range(E+1,N)} end;
range(_,_) -> fun() -> eof end.

to_list(Src) ->
    case Src() of
        {El, NextSrc} -> [El | to_list(NextSrc)];
        eof -> []
    end.

to_fold(Src) ->
    fun(F,I) -> to_fold(Src, F, I) end.
to_fold(Src, F, S) ->
    case Src() of
        {E, NextSrc} -> to_fold(NextSrc, F, F(E, S));
        eof -> S
    end.
            

map(F,Src) ->
    fun() ->
            case Src() of
                {El, NextSrc} -> {F(El), map(F,NextSrc)};
                eof -> eof
            end
    end.
            
            

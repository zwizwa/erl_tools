-module(source).
-export([range/2,range/1,to_list/1,to_fold/1]).

%% Internal iterators -- dual of sink, represented as pairs with
%% delayed tails.

range(N) -> range(0,N).
range(E,N) when E < N -> fun() -> {E, range(E+1,N)} end;
range(_,_) -> fun() -> eof end.

to_list(Source) ->
    case Source() of
        {E,S} -> [E | to_list(S)];
        eof -> []
    end.

to_fold(Source) ->
    fun(F,I) -> to_fold(Source, F, I) end.
to_fold(Source, F, S) ->
    case Source() of
        {E, Src} -> to_fold(Src, F, F(E, S));
        eof -> S
    end.
            
            

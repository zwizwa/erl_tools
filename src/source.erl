-module(source).
-export([range/2,range/1,to_list/1]).

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
            


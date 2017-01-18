-module(source).
-export([range/2,range/1,to_list/1,to_fold/1,map/2
         ,wind/2, wind_unpack/2]).

%% External iterators, represented as eof or pair wrapped in thunk.

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

%% Wind until predicate is true or eof.
wind_unpack(Pred, Src) ->            
    case Src() of
        {El, NextSrc}=Unpacked ->
            case Pred(El) of
                true -> Unpacked;
                false -> wind_unpack(Pred, NextSrc)
            end;
        eof -> eof
    end.
            
%% Packed version for completeness, though unpacked version is likely
%% more useful.
wind(Pred, Src) ->
    fun() -> wind_unpack(Pred, Src) end.
            
                            


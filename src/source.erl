-module(source).
-export([range/2,range/1,to_list/1,to_fold/1,map/2,filter/2
        ,unpack/1
        ,wind/2, wind_unpack/2
        ,from_list/1]).

%% PURE SEQUENCES

%% External iterators, represented as eof or pair wrapped in thunk.
%% Note: this only works for side-effect free code.

%% Representation can either be unpacked or not.  This is a small hack
%% to avoid re-evaluation.

%% FIXME: change to tagged representation.

unpack(eof) -> eof;
unpack({_,_}=Pair) -> Pair;
unpack(Thunk) when is_function(Thunk) -> Thunk().

range(N) -> range(0,N).
range(E,N) when E < N -> fun() -> {E, range(E+1,N)} end;
range(_,_) -> fun() -> eof end.

to_list(Src) ->
    case unpack(Src) of
        {El, NextSrc} -> [El | to_list(NextSrc)];
        eof -> []
    end.

to_fold(Src) ->
    fun(F,I) -> to_fold(Src, F, I) end.
to_fold(Src, F, S) ->
    case unpack(Src) of
        {E, NextSrc} -> to_fold(NextSrc, F, F(E, S));
        eof -> S
    end.
            

map(F,Src) ->
    fun() ->
            case unpack(Src) of
                {El, NextSrc} -> {F(El), map(F,NextSrc)};
                eof -> eof
            end
    end.

filter(Pred,Src) ->
    fun() ->
            case unpack(Src) of
                {El, NextSrc} ->
                    Tail = filter(Pred, NextSrc),
                    case Pred(El) of
                        true -> {El, Tail};
                        false -> unpack(Tail)
                    end;
                eof ->
                    eof
            end
    end.

                            

%% Wind until predicate is true or eof.
wind_unpack(Pred, Src) ->            
    case unpack(Src) of
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
            
                            

from_list(List) ->
    fun() ->
            case List of
                [] -> eof;
                [H|T] -> {H, from_list(T)}
            end
    end.


%% Note that it is possible to have from_fold/1 but since that is
%% effectful, it seems best to implement that only in egen.erl

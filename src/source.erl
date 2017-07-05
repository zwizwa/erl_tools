-module(source).
-export([range/2,range/1,to_list/1,to_fold/1,map/2,filter/2
        ,unpack/1
        ,wind/2, wind_unpack/2
        ,from_list/1
        ,from_fold/1]).

%% External iterators, represented as eof or pair wrapped in thunk.
%% Note: this only works for side-effect free code.  FIXME: add delay/force to eval only once. 

%% Representation can either be unpacked or not.  This is a small hack
%% to avoid re-evaluation.

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

%% Fold is one-shot, so it needs to be blocked at some point by
%% running it in a separate process.  Note that the iterator needs to
%% be used up all the way to eof otherwise the fold will not terminate
%% and any resources it might need to free will stay open.
from_fold(Fold) ->
    fun() ->
            Sync = fun(Val) ->
                           receive {Pid, next} -> obj:reply(Pid, Val) end
                   end,
            Serv = spawn_link(
                     fun() ->
                             Fold(fun(Val,_) -> Sync({data,Val}) end, nostate),
                             Sync(eof)
                     end),
            Next = fun() -> 
                           obj:call(Serv, next)
                   end,
            from_fold(Sync, Serv, Next)
    end.

from_fold(Sync, Serv, Next) ->
    case Next() of 
        {data, Val} -> {Val, fun() -> from_fold(Sync, Serv, Next) end}; 
        eof -> eof
    end.



                    
    
            
            
    

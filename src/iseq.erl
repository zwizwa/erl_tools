-module(iseq).
-export([cycle/1, take/2, periodic/2, map/2]).

%% iseq : infinite sequences represent as thunks
%% Evaluating the thunk produces an {element, iseq} pair.

cycle(List, []) ->
    cycle(List, List);
cycle(Spec, [Head|Tail]) ->
    {Head, fun() -> cycle(Spec, Tail) end}.
cycle(List=[_|_]) ->
    fun() -> cycle(List, List) end.


periodic({_,List}=Spec, Loop, []) ->
    periodic(Spec, Loop+1, List);
periodic({Period, _} = Spec, Loop, [Head|Tail]) ->
    {Loop * Period + Head, 
     fun() -> periodic(Spec, Loop, Tail) end}.
periodic(Period, List=[_|_]) ->
    fun() -> periodic({Period, List}, 0, List) end.


take(_,0) -> [];
take(Seq,N) when N > 0 -> 
    {E,C} = Seq(),
    [E|take(C, N-1)].


map(Fun, Seq) ->
    fun() -> {Head, Tail} = Seq(), {Fun(Head), map(Fun, Tail)} end.

             
            

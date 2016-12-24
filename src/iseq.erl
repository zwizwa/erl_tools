-module(iseq).
-export([cycle/1, take/2, periodic/2]).

%% iseq : infinite sequences represent as thunks
%% Evaluating the thunk produces an {element, iseq} pair.

cycle({_,Init}=Spec, []) ->
    cycle(Spec, Init);
cycle(Spec, [Head|Tail]) ->
    {Head, fun() -> cycle(Spec, Tail) end}.
cycle(OrdSet=[_|_]) ->
    fun() -> cycle(OrdSet, OrdSet) end.


periodic({_,Init}=Spec, Loop, []) ->
    periodic(Spec, Loop+1, Init);
periodic({Period, _} = Spec, Loop, [Head|Tail]) ->
    {Loop * Period + Head, 
     fun() -> periodic(Spec, Loop, Tail) end}.
periodic(Period, OrdSet=[_|_]) ->
    fun() -> periodic({Period, OrdSet}, 0, OrdSet) end.


take(_,0) -> [];
take(Cycle,N) when N > 1 -> 
    {E,C} = Cycle(),
    [E|take(C, N-1)].

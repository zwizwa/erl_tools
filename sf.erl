%% LICENCE: This code (sf.erl) is placed in the public domain by its
%% author, Tom Schouten.  The code is based on original research in
%% embeddes software patterns in C and other languages, performed
%% throughout the period 2002-2016.

-module(sf).
-export([fold_range/3,
         range/1,
         map/2,
         fold/3,
         empty/0,
         append/1, append/2,
         to_list/1,
         to_rlist/1,
         for/2,
         histogram/1
        ]).
         

%% SF: sequences represented as folds (tail recursive left folds).




%% This allows per-element transformation followed by accumulation
%% without building intermediate data structures, i.e. the approach
%% implements implicit loop folding.  It is similar in purpose to lazy
%% lists, but the Foldl interface is closer to many of the use cases.


%% Tools
fold_range(F,S,N,I) ->
    case I < N of
        false -> S;
        true  -> fold_range(F, F(I,S), N, I+1)
    end.
            
fold_range(F,S,N) ->
    fold_range(F,S,N,0).

empty() ->
    fun(_, Init) -> Init end.


%% A fold representing a sequence is of type 
%% :: ( (E,S) -> S, S ) -> S
range(N) -> fun(F,S) -> fold_range(F,S,N) end.



%% Map over a sequence represented as a fold, returning a new fold
%% representing a sequence.
map(MapFun, SF) ->
    fun(FoldFun, Init) ->
            SF(fun(El,Accu) -> FoldFun(MapFun(El),Accu) end,
               Init)
    end.

%% Makes code more readable, not depending on representation.
fold(F,I,SF) -> SF(F,I).

%% Convenience.
for(Fun, SF) -> SF(fun(E,_)->Fun(E) end, dummy).


    
                 

%% Append (a list of) folds.  Also as fold of folds?
append(S1, S2)  -> fun(Fun, Init) -> S2(Fun, S1(Fun, Init)) end.
append([])      -> empty();
append([S1|Ss]) -> append(S1, append(Ss)).

%% Convert sequence to list.  Since these are left folds, folding a
%% cons will yield a reversed list.
to_rlist(SF) -> SF(fun(E,S)->[E|S] end, []).
to_list(SF) -> lists:reverse(to_rlist(SF)).


%% Some folds.
histogram(Fold) ->
    Fold(fun tools:maps_count/2, #{}).

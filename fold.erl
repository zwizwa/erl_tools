%% Sequences represented as tail recursive left folds.
%% I.e. event processing state machine loops.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to fold.hrl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


%% This approach to abstracting sequences in a strict language allows
%% chaining of stream processors without building intermediate data
%% structures.

%% The point here is to keep the operations purely functional, however
%% they can be extended with processes and messages passing, treating
%% the fold function as a process' main loop.

%% This can be thought of as complimentary to sink.erl The combination
%% of both, and processes, yields input/output behavior.


-module(fold).
-export([fold_range/3,
         range/1,
         map/2,
         fold/3,
         empty/0,
         append/1, append/2,
         from_list/1, to_list/1, to_rlist/1,
         for/2,
         histogram/1,
         split_at/3, split_at/2,
         drop/2,
         gen/1,
         chunks/2
        ]).
         


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

%% And the other way around.
from_list(List) ->                         
    fun(F,I) -> lists:foldl(F,I,List) end.


%% Some folds.
histogram(Fold) ->
    Fold(fun tools:maps_count/2, #{}).


%% Bundle header/data sequence, e.g.
%% H,D,D,H,D,D,D,H,... -> (H,[D,D]), (H,[D,D,D]), (H,[...]), ...
%%
%% This is a problem that occurs a *lot* in practice, e.g. analysis of
%% logfiles, but is surprisingly easy to end up on a path
%% re-specializing the pattern.  Solve it once and for all.
%%

%% The reason to solve it as a fold is that the total sequence can be
%% large, and we do not want to load it into memory.

%% To represent the inner lists as a fold, we need access to a
%% sequence of initial states. The plumbing here becomes a little
%% obscure, so let's keep the inner representation at stacks (which
%% can be reversed using a fold:map).  It is reasonable to assume that
%% the stacks are not large compared to the total length of the
%% initial stream.

%% The "parser" is a state machine that keeps track of all encountered
%% data and dumps it in {H,[D]} form whenever a "transition" is
%% encountered, which corresponds to a D->H transition, or the end of
%% the sequence.

%% To translate this into a fold, we need two kinds of state:
%% - User state
%% - Collection state
    
split_at(HeaderP, Header0, Fold) ->
    fun(UFun, UState0) -> split_at(HeaderP, Header0, Fold, UFun, UState0) end.
            
split_at(HeaderP, Header0, Fold, UFun, UState0) ->
    %% Fold body (= event processor)
    F = fun(Element,         %% Current element
            {Header, Stack,  %% Our state
             UState}) ->     %% User state and update
                case HeaderP(Element) of
                    false -> {Header, [Element | Stack], UState};
                    _     -> {Element, [], UFun({Header,Stack}, UState)}
                end
        end,
    %% Apply it to the sequence
    {H,S,U} = Fold(F, {Header0, [], UState0}),
    %% Flush buffer (end-of-sequence is a delimiter).
    UFun({H,S}, U).
    

split_at(HeaderP, Fold) ->
    drop(1, split_at(HeaderP, dummy_header, Fold)).
    




%% DROP.  Note that without partial continuations, it's not possible
%% to only take a couple of elements leaving the tail as a Fold.  PCs
%% or something similar can likely be emulated with Erlang processes.

%% For now, only drop is necessary, which doesn't have this problem as
%% it can be represented by a state extension without violating the
%% fold interface.

drop(N, Fold) ->
    fun(F, I) -> drop(N, Fold, F, I) end.
drop(N, Fold, UFun, UState0) ->
    {_, S} =
        Fold(fun(Element, {Dropped, UState}) ->
                     case Dropped of
                         N -> {Dropped, UFun(Element, UState)};
                         _ -> {Dropped+1, UState}
                     end
             end,
             {0, UState0}),
    S.

%% Convert a Sink-parameterized generator to a Fold.  In general,
%% prefer Folds, but in some cases it is easier to implement
%% interation in terms of side-effecting sinks.  Note that some flow
%% control is necessary.  Otherwise generator process will just fill
%% up the message buffer.
gen(Gen) ->
    Pid = self(),
    Sink = fun(Msg) ->
                   Pid ! {self(), Msg},
                   receive {Pid, cont} -> ok end
           end,
    GenPid = spawn_link(fun() -> Gen(Sink) end),
    fun(F, I) -> gen_fold(F,I,GenPid) end.

%% FIXME: can there be leaks when there are errors in current thread

%% Note: the above doesn't work if the generator makes calls to a port
%% process, as that can only be done in-proces and we call the
%% generator in a new process.  See tools:gen_to_list/1


gen_fold(Fun, Accu, GenPid) ->
    receive
        {GenPid, eof} -> Accu;
        {GenPid, {data, El}} ->
            NextAccu = Fun(El, Accu),
            GenPid ! {self(), cont},
            gen_fold(Fun, NextAccu, GenPid)
    end.

%% FIXME: evaluating a generator fold twice will hang.  Annoying, but
%% in general, folds are instances i.e. single use iterators.
                    



%% Similar in idea to iolists, it is often the case in low level data
%% processing that streams are 2-level: streams of chunks (packets).
%% Assuming there are no boundary violations (i.e. split elements),
%% this can be solved easily in a generic way, combining:
%% - a fold over the chunks
%% - a fold generator (mapping chunk to fold over elements)
%% to yield a fold over elements.


chunks(Chunks, Chunk2Elements) ->
    %% Chunks and Elements are folds.
    fun(Fun, Init) ->
            Chunks(
              fun(Chunk, Accu) ->
                      Elements = Chunk2Elements(Chunk),
                      Elements(Fun, Accu)
              end,
              Init)
    end.
    

                         

                         
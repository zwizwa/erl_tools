%% Sequences represented as tail recursive left folds.
%% I.e. event processing state machine loops.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to fold.erl
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
         filter/2, filter_map/2, filter_index/2, drop/2,
         empty/0,
         append/1, append/2,
         from_list/1, to_list/1, to_rlist/1,
         dump/1, print/1,
         for/2,
         histogram/1,
         split_at/3, split_at/2,
         gen/1, gen/2,
         chunks/2,
         iterate/2,
         split_sub/1,
         split_size/1,
         enumerate/1
        ]).
         
-export_type([iterspec/0, update/1, chunk/0, sink/0]).
-type update(State) :: fun((any(), State) -> State).
-type chunk() :: {data, any()} | eof.
-type sink() :: fun((chunk()) -> any()).
-type iterspec() :: {foldl, update(State), State} 
                  | {foldr, update(State), State}
                  | {sink, sink()}
                  | list.




%% -type foldee(E,S) :: fun((E :: any(), S :: State) -> State).

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

%% Print elements.
dump(SF)  -> SF(fun(E,S) -> io:format("~p~n",[E]), S end, ok).
print(SF) -> SF(fun(E,S) -> io:format("~s",[E]), S end, ok).
                       
    


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
    




%% DROP.  Note that without partial continuations (e.g. emulated by
%% Erlang tasks), it's not possible to only take a couple of elements
%% leaving the tail as a Fold.  The entire loop needs to be
%% transformed.

%% It seems simplest to implement this in terms of enumerate and
%% filter, and also to generalize it to arbitrary index filtering.

drop(N, Fold) ->
    filter_index(fun(I) -> I >= N end, Fold).
filter_index(PredIndex, Fold) ->
    filter_map(fun({I, El}) -> {PredIndex(I), El} end,
               enumerate(Fold)).

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
                    

%% gen/2 assumes the generator argument is passed last.
gen(Fun, Args) ->
    gen(fun(Sink) -> apply(Fun, Args ++ [Sink]) end).


%% Similar in idea to iolists, it is often the case in low level data
%% processing that streams are 2-level: streams of chunks (packets).

%% This function converts a fold over arbitrary chunks to a fold over
%% re-packaged based on a Splitter machine.  The splitter machine
%% performs the fold over the elements, threading along its splitter
%% state.
chunks(Fold, {foldl, Split, SplitInit}=_Splitter) ->
    fun(Fun, FunInit) ->
            {FunResult, _} =
                Fold(
                  fun(Chunk, {FunState, SplitState}) ->
                          SplitFold = Split(Chunk, SplitState),
                          SplitFold(Fun, FunState)
                  end,
                  {FunInit, SplitInit}),
            FunResult
    end.

%% Convert a stateless splitter (only that only subdivides partent
%% chunks and doesn't merge them) into a stateful splitter with dummy
%% state.
split_sub(Splitter) ->
    fun(Chunk, _) -> {Splitter(Chunk), nostate} end.
    

%% Convert sequence of iolist into sequence of fixed-size binary.
%% Ignore leftovers.
split_size(Size) ->       
    {foldl,
     fun(Chunk, ChunkState) ->
             fun(Fun, State) ->
                     split_size(
                       Size, iolist_to_binary([ChunkState, Chunk]),
                       Fun, State)
             end
     end,
     <<>>}.
split_size(Size, ChunkState, Fun, State) ->
    case ChunkState of
        <<Element:Size/binary, Rest/binary>> ->
            split_size(Size, Rest, Fun, Fun(Element, State));
        _ ->
            {State, ChunkState}
    end.
     

%% fold:to_list(fold:chunks(fold:from_list([1,2,3,4,5,6]), fold:split_size(2))).

    

%% Filter elements.  filter_map is quite common and useful for
%% optimization, so implement that as main routione.
filter(Pred, Fold) ->
    filter_map(fun(El) -> {Pred(El), El} end, Fold).
filter_map(PredEl, Fold) ->
    fun(Fun, Init) ->
            Fold(
              fun(El, Accu) ->
                      case PredEl(El) of
                          {true, FEl} -> Fun(FEl, Accu);
                          {false, _} -> Accu
                      end
              end, Init)
    end.
   
                         
%% Based on left and right fold, implement sink and list patterns.
iterate({FoldLeft, FoldRight}, IterSpec) ->
    case IterSpec of
        {foldl, F, S} -> FoldLeft(F, S);
        {foldr, F, S} -> FoldRight(F, S);
        {sink, Sink}  ->
            FoldLeft(fun(Data,_) -> Sink({data,Data}) end,
                     none),
            Sink(eof);
        list ->
            FoldRight(fun(H,T) -> [H|T] end,
                      [])
    end;

%% Based on only left fold.
iterate(FoldLeft, IterSpec) ->
    case IterSpec of
        list ->
            lists:reverse(
              FoldLeft(fun(H,T) -> [H|T] end, []));
        _ ->
            iterate({FoldLeft,no_foldr}, IterSpec)
    end.


                         
enumerate(Fold) ->
    fun(Fun,Init) ->
            {_,State} =
                Fold(fun(Value,{N, State}) ->
                             {N+1, Fun({N, Value}, State)}
                     end,
                     {0, Init}),
            State
    end.



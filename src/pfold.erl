%% Sequences as Partial Folds
%% --------------------------

%% http://okmij.org/ftp/papers/LL3-collections-enumerators.txt
%% Extension to fold.erl

%% The early stop protocol expects these return values from the foldee:
%%   {next,  State}       Proceed normally
%%   {stop, FinalState}  Stop iteration, return value
%%


%% FIXME: making the folds lazy will add an additional layer of
%% indirection, e.g. making append simpler.

-module(pfold).
-export([range/1,
         append/2, append/1,
         to_list/1, to_rlist/1,
         take/2,
         next/1,
         with_stop_exit/1
        ]).
         
-export_type([update/1, chunk/0, sink/0, control/1, seq/0]).
-type control(State) :: {next, State} | {stop, State}.
-type update(State) :: fun((any(), State) -> control(State)).
-type chunk() :: {data, any()} | eof.
-type sink() :: fun((chunk()) -> any()).
-type seq() :: fun((update(State), State) -> State).

%% Wrap an ordinary foldee so it can be used with a folder that
%% expects the early stop protocol, by not stoping.
next(F) -> fun(E,S) -> {next, F(E,S)} end.
    

range(F,State,N,I) ->
    case I < N of
        true ->
            case F(I,State) of
                {next, NextState} -> range(F,NextState,N,I+1);
                {stop, FinalState} -> FinalState
            end;
        false ->
            State
    end.
range(N) -> fun(F,S) -> range(F,S,N,0) end.



%% Perform fold, and annotate return value with finished/stopped.
%% Used to support append.
tag_stop(S,F,I) ->
    S(fun(El, {Tag, State}) ->
              case F(El, State) of
                  {stop, FinalState} -> {stop, {stopped, FinalState}};
                  {next, NextState}  -> {next, {Tag, NextState}}
              end
      end, {finished, I}).

append(S1, S2)  ->
    fun(F, I1) -> 
            case tag_stop(S1, F, I1) of 
                {finished, I2} -> 
                    S2(F, I2);
                {stopped, F2} ->
                    %% S2 might contain cleanup code so we do need to
                    %% run it.  (Folds are one-shot, not lazy).  Can
                    %% there be side-effects?
                    S2(fun(_, _) -> {stop, none} end, none),
                    F2
            end
    end.
append([]) ->
    fold:empty();
append([S1|S2]) ->
    append(S1, append(S2)).

to_rlist(SF) -> SF(fun(E,S)->{next, [E|S]} end, []).
to_list(SF) -> lists:reverse(to_rlist(SF)).

take(SF, MaxNb) ->
    fun(F, I) ->
            {_, FinalState} =
                SF(fun(El, {Count, State} = CS) ->
                           case Count >= MaxNb of 
                               true  -> {stop, CS};
                               false -> {Tag, NextState} = F(El, State),
                                        {Tag, {Count + 1, NextState}}
                           end
                   end,
                   {0, I}),
            FinalState
    end.
    
%% pfold:to_list(pfold:take(pfold:append(pfold:range(10), pfold:range(10)), 4)).

%% Take a fold.erl fold, and wrap it as a pfold.erl fold which raises
%% an error on exit.
with_stop_exit(Fold) ->
    fun(F,I) ->
            Fold(
              fun(E,S) ->
                      case F(E,S) of
                          {next, S} -> S;
                          {stop, S} -> exit({stop, S})
                      end
              end,
              I)
    end.

%% Folds with early abort
%% ----------------------

%% Extension to fold.erl

%% The early abort protocol expects these return values from the foldee:
%%   {continue, State}   Proceed normally
%%   {abort, FinalState} Abort iteration, return value
%%
%% To propagate the condition upward, return states are tagged with
%% {finished, State}, {aborted, State}.

-module(afold).
-export([range/1,
         append/2, append/1,
         to_list/1, to_rlist/1,
         take/2,
         continue/1]).
         

%% Wrap an ordinary foldee so it can be used with a folder that
%% expects the early abort protocol, by not aborting.
continue(F) -> fun(E,S) -> {continue, F(E,S)} end.
    

range(F,S,N,I) ->
    case I < N of
        true ->
            case F(I,S) of
                {continue, NS} -> range(F,NS,N,I+1);
                {abort,    FS} -> {aborted, FS}
            end;
        false ->
            {finished, S}
    end.
range(N) -> fun(F,S) -> range(F,S,N,0) end.





append(S1, S2)  ->
    fun(F, I1) -> 
            case S1(F, I1) of
                {finished, I2} -> 
                    S2(F, I2);
                {aborted, F2} ->
                    %% S2 might contain cleanup code so we do need to
                    %% run it.  (Folds are one-shot, not lazy).  Can
                    %% there be side-effects?
                    S2(fun(_, _) -> {abort, F2} end, none)
            end
    end.
append([]) ->
    fold:empty();
append([S1|S2]) ->
    append(S1, append(S2)).

to_rlist(SF) -> {_, List} = SF(fun(E,S)->{continue, [E|S]} end, []), List.
to_list(SF) -> lists:reverse(to_rlist(SF)).

take(SF, MaxNb) ->
    fun(F, I) ->
            {ETag, {_, FinalState}} =
                SF(fun(El, {Count, State} = CS) ->
                           case Count >= MaxNb of 
                               true  -> {abort, CS};
                               false -> {CTag, NextState} = F(El, State),
                                        {CTag, {Count + 1, NextState}}
                           end
                   end,
                   {0, I}),
            {ETag, FinalState}
    end.
    
%% afold:to_list(afold:take(afold:append(afold:range(10), afold:range(10)), 4)).


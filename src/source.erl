-module(source).
-export([range/2,range/1,to_list/1,to_fold/1,map/2,filter/2
        ,unpack/1
        ,wind/2, wind_unpack/2
        ,from_list/1
        ,take/2
        ,append/1]).

%% PURE/ITEMPOTENT SEQUENCES

%% Delayed sequences with eof.
-export_types([source/1, unpacked_source/1]).
-type unpacked_source(El) :: eof | {El, source(El)}.
-type source(El) :: unpacked_source(El) | fun(() -> source(El)).

%% Note that there might be more than one thunk wrapper -- this is
%% done to ease implementation of some primitives.  The thunks might
%% be evaluated multiple times, so they either need to be pure, or
%% their effects need to be idempotent.  See remark at the bottom.

unpack(eof) -> eof;
unpack({_,_}=Pair) -> Pair;
unpack(Thunk) when is_function(Thunk) -> 
    unpack(Thunk()).

%% FIXME: use tagging to make types more explicit?
%% unpack({source, Src}) -> unpack(Src); ??

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
take(N,Src) -> take(N,[],Src).
take(0,Es,Src) -> {lists:reverse(Es),Src};
take(N,Es,Src) -> 
    case unpack(Src) of
        eof -> take(0,Es,eof);
        {E, NextSrc} -> take(N-1,[E|Es],NextSrc)
    end.



%% Append multiple sources.

%% TODO: append + unpack multiple thunks.

append([]) -> eof;
append([Src]) -> Src;
append([Src|Srcs]) ->
    fun() ->
            case unpack(Src) of
                eof -> append(Srcs);
                {E,Src1} -> {E,append([Src1|Srcs])}
            end
    end.
                 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("source.expect").
expect_test() ->
    expect:run_form(
      filename:dirname(?FILE)++"/source.expect",
      fun source_expect/0).
-endif.



%% Note: once-only evaluation cannot easily be implemented in Erlang
%% in the way that delay/force would be implemented in e.g. Scheme,
%% because of Erlang's purity.
%%
%% It might be possible to use processes to store the state that is
%% necessary to perform once-only evaluation, however that would still
%% need garbage collection for those processes, for which I do not see
%% a solution.

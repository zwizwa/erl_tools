
%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to tools.hrl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0



unhex([]) -> [];
unhex([H|[L|T]]) ->
    [list_to_integer([H,L], 16) | unhex(T)].

hex_list([]) -> [];
hex_list([H|T]) -> [hex8(H)] ++ hex_list(T);
hex_list(X) -> hex_list(as_list(X)).

hex(L) -> lists:flatten(hex_list(L)).

hex16(Val) -> [_|Hex] = integer_to_list(16#10000 + (Val band 16#FFFF),16), Hex.
hex8(Val)  -> [_|Hex] = integer_to_list(16#100   + (Val band 16#FF),  16), Hex.


hex_u32(Value) ->
    hex(binary_to_list(<<Value:32/little>>)).

%% FIXME: find a better naming scheme.
int(Bytes) ->
    case binary_to_list(Bytes) of
        [$0,$x | Hex] -> list_to_integer(Hex, 16);
        Dec           -> list_to_integer(Dec, 10)
    end.

float(Bytes) ->
    list_to_float(binary_to_list(Bytes)).

hex_data(HexBytes) ->
    tools:unhex(binary_to_list(HexBytes)).


% Chunking.

%% Chop list up in chunks.
chunks(N,List) ->
    case N >= length(List)  of
        true  -> [List];
        false ->
            {H,T} = lists:split(N,List),
            [H | chunks(N,T)]
    end.

%% Chop number up in chunks.
nchunks(Offset, Endx, Max) ->
    Left = Endx - Offset,
    case Left > Max of
        true  -> [{Offset, Max} | nchunks(Offset + Max, Endx, Max)];
        false -> [{Offset, Left}]
    end.

% Character reader.
cr_loop(Sock, []) ->
    case gen_tcp:recv(Sock, 0) of
        {error, Error} -> exit(Error);
        {ok, Data} ->
            % info("Data ~p~n", [Data]),            
            cr_loop(Sock, Data)
    end;
cr_loop(Sock, [Char|Rest]) ->
    receive {get, Pid}
            -> Pid ! {self(), Char}, 
               cr_loop(Sock, Rest) 
    end.
creader(Sock) ->
    Pid = spawn_link(fun() -> cr_loop(Sock,[]) end),    
    fun() -> Pid ! {get, self()}, 
             receive {Pid, Char} -> 
                     %% info("Char ~p~n", [Char]),
                     Char end
    end.


enumerate(Lst) ->
    lists:zip(lists:seq(0,length(Lst)-1),Lst).

getter(Dict) ->
    fun(Key) -> 
            %% info("find: ~p~n", [Key]),
            case maps:find(Key, Dict) of
                {ok, Val} -> Val;
                Error -> info("key error: ~p~n~p~n~p~n",
                              [Error, Key, Dict]), exit({getter, key}) 
            end
    end.

%% Unpack a nested tree of binary messages.  The binary messages
%% themselves need to be word-aligned.

%% U = unpack binary
%% H = head
%% T = tail
%% S = stack

unpack(_, <<>>, [])    -> []; 
unpack(U, <<>>, S)     -> unpack(U, S, []);
unpack(U, [],   S)     -> unpack(U, S, []);
unpack(U, [H],  S)     -> unpack(U, H, S);
unpack(U, [H | T], S)  -> unpack(U, H, [T | S]);  % T != []
unpack(U, Bin, S)      ->
    {H,T} = U(Bin),
    %% info("~p ~p ~p~n",[H,T,S]),
    [H | unpack(U, T, S)].

unpack(U, L)  -> unpack(U, L, []).


unpack_s32(L) ->     
    unpack(fun(<<H:32/signed-little,T/binary>>) -> {H,T} end, L).

unpack_u16(L) ->     
    unpack(fun(<<H:16/little,T/binary>>) -> {H,T} end, L).

unpack_s16(L) ->     
    unpack(fun(<<H:16/signed-little,T/binary>>) -> {H,T} end, L).




%% Modulo with positive remainder.
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

%% Midpoint, picking smallest if neighbours.
mid(A,B) -> (A + B) div 2.

%% Same with limited resolution.
mid(A,B,Res) -> Res * mid(A div Res, B div Res).

%% Binary search
bisect_pick(Pick,A,B) ->
    %%info("bisect(~p,~p)~n",[A,B]),
    M = mid(A,B),
    case M of
        A -> {A,B};
        B -> {A,B};
        _ ->
            {A_,B_} = Pick(A,M,B),
            bisect_pick(Pick,A_,B_)
    end.
%% Bisect by picking left interval if predicate applied to left
%% interval is true.
bisect_left(Pred, A, B) ->             
    bisect_pick(
      fun(L, M, R) ->
              case Pred(L, M) of
                  true  -> {L, M}; 
                  false -> {M, R}
              end
      end, A, B).

             
%% FIXME: quick&dirty.
csv_read(File) ->
    {ok, Data} = file:read_file(File),
    csv_parse(Data).
csv_parse(Data) ->
    [lists:map(fun binary_to_list/1, re:split(Line, ",", []))
     || Line <- re:split(Data, "\r|\n|\r\n", []),
        Line =/= <<"">>].

%% Join binary without flattening.
binary_join([A,B|T], S) -> [A,S|binary_join([B|T], S)];
binary_join(E, _) -> E.


    
    
enumerate([],_) -> [];
enumerate([H|T],N) -> [{N,H}|enumerate(T,N+1)].


%% It is a little unfortunate that Erlang uses 1-base addressing in
%% lists:nth, but 0-base for Binary.  To reduce confusion, it makes
%% sense to use 0-base for lists as well.
list_at(List, Index) ->
    lists:nth(Index + 1, List).
    

             
%% Record to Maps translation
tuple_to_list(Tuple) ->
    [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))].

%% Project onto binary representation.
as_binary(X) when is_binary(X) -> X;
as_binary(X) when is_list(X) -> list_to_binary(X);
as_binary(X) when is_atom(X) -> atom_to_binary(X, utf8).
    
as_binaries(Lst) -> lists:map(fun as_binary/1, Lst).
    
  
as_list(X) when is_list(X) -> X;
as_list(X) when is_binary(X) -> binary_to_list(X);
as_list(X) when is_atom(X) -> atom_to_list(X).
     
as_atom(X) when is_atom(X) -> X;
as_atom(X) when is_binary(X) -> binary_to_atom(X,utf8);
as_atom(X) when is_list(X) -> list_to_atom(X).
     
     
%% Parallel map
%% Return list is unsorted.
pmapu(Fun, Keys) ->
    Pid = self(),
    lists:foreach(fun(Key) -> Pid ! {pmapu_result, Key, Fun(Key)} end, Keys),
    pmapu_gather(#{}, length(Keys)).
pmapu_gather(Results, 0) ->
    Results;
pmapu_gather(Results, Left) ->
    receive
        {pmapu_result, Key, Val} ->
            pmapu_gather(maps:put(Key, Val, Results), Left-1)
    end.

pmap(Fun, Keys) ->
    M = pmapu(Fun, Keys),
    [maps:get(Key, M) || Key <- Keys].


binary_fold(Fun, State, Bin, I) ->     
    case size(Bin) of
        I -> State;
        _ -> binary_fold(Fun, Fun(binary:at(Bin,I), State), Bin, I+1)
    end.
binary_fold(Fun, Init, Bin) ->
    binary_fold(Fun, Init, Bin, 0).
binary_sum(Bin) ->            
    binary_fold(fun(A,B)->A+B end, 0, Bin).


%% Data sinks.
sink_gen_tcp(Sock) ->
    fun(Msg) -> 
            case Msg of
                {data, Data} -> gen_tcp:send(Sock, Data);
                _ -> ok
            end
    end.

mask_bits(0) -> 0;
mask_bits(Mask) when Mask < 0 -> exit(mask_bits_negative);
mask_bits(Mask) -> (Mask band 1) + mask_bits(Mask bsr 1).


run_sync(Port, OK) ->
    receive
        {Port, {data, {eol, OK}}} ->
            ok;
        {Port, {data, {eol, Data}}} ->
            info("script: ~p~n", [Data]),
            run_sync(Port, OK);
        {Port, Anything} ->
            {error, Anything}
    after
        2000 -> {error, timeout}
    end.

run_script(Cmd,OK) ->
    Port = open_port({spawn, Cmd}, [{line, 1024}, use_stdio]),
    run_sync(Port, OK).



%% Append value to a list tagged in a map.
maps_apply(Tag, Fun, Default, Map) ->
    Val = maps:get(Tag, Map, Default),
    maps:put(Tag, Fun(Val), Map).

maps_append(Tag, Val, Map) ->
    maps_apply(Tag, fun (List) -> List ++ [Val] end, [], Map).

maps_count(Tag, Map) ->    
    maps_apply(Tag, fun (Val) -> 1 + Val end, 0, Map).
                             

%% Create tagged index map from fold.
%% Sentinels are abstracted as:
%%   TagIndex({index, Index}) == {none,Index}
%%   TagIndex(...)            == {Tag, Index}
%% Foldl is a partially applied left fold over the data structure.
%%
%% Why? Use this when data is indexable in principle, but does not
%% have an intrinsic index, e.g. a (large) list of concatenated
%% heterogenous messages.  Abstract the iteration in a C Port, and use
%% a 2-pass: one to build an index, and one to (repeatedly) access the
%% data using the index created by tagged_index/2.
tagged_index(TagIndex, Foldl) ->
    Update =
        fun(Prev, Next, Map) ->
                {Tag, Start} = TagIndex(Prev),
                {_,   Endx}  = TagIndex(Next),
                maps_append(Tag, {Start, Endx}, Map)
        end,
    {Last, Map} = 
        Foldl(
          fun(Next, {Prev, Map}) ->
                  {Next, Update(Prev, Next, Map)}
          end,
          {{index, 0}, #{}}),
    Update(Last, {index, -1}, Map).
    
                      


map_to_list(M) ->
    [{K,maps:get(K,M)} || K <- lists:sort(maps:keys(M))].

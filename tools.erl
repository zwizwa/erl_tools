-module(tools).
-export([info/1, info/2, info/3,
         unhex/1, hex/1, hex_list/1, hex4/1, hex8/1, hex16/1, hex_u32/1,
         strunk/1, getter/1, 
         format/2, creader/1, int/1, float/1, hex_data/1, enumerate/1, chunks/2, nchunks/3,
         unpack/2, unpack_s32/1, unpack_u16/1, unpack_s16/1,
         mod/2,
         mid/2, mid/3,
         csv_read/1,
         padded_at/2, padded_range/3,
         enumerate/2,
         list_at/2,
         tuple_to_list/1, as_binary/1, as_binaries/1, as_list/1, as_atom/1,
         pmap/2,
         foldn/3,
         binary_fold/3, binary_sum/1, binary_join/2,
         mask_bits/1,
         run_script/2,
         maps_apply/4, maps_append/3, maps_count/2,
         tagged_index/2,
         map_to_list/1,
         line_splitter_update/4,
         read_eval_print/2,
         annotate_pid/1,
         maps_map_keys/2,
         script_line/1,
         s2u_16/1,
         random_uniform_list/2,
         random_binary/1,
         timestamp/0,
         timestamp_us/0,
         filter_tag/2, filter_tags/2,
         register/2
        ]).

%% Ad-hoc tools collection.


      
%% Collection of generic tools for Erlang.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to tools.erl
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
hex4(Val)  -> [_|Hex] = integer_to_list(16#10    + (Val band 16#F),   16), Hex.


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

strunk([])    -> [];
strunk([0|_]) -> [];
strunk([H|T]) -> [H|strunk(T)].


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
% FIXME: turn this into a de-chunker
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
as_binary(X) when is_binary(X)  -> X;
as_binary(X) when is_list(X)    -> list_to_binary(X);
as_binary(X) when is_atom(X)    -> atom_to_binary(X, utf8);
as_binary(X) when is_integer(X) -> as_binary(integer_to_list(X)).
    
as_binaries(Lst) -> lists:map(fun as_binary/1, Lst).
    
  
as_list(X) when is_list(X)    -> X;
as_list(X) when is_integer(X) -> integer_to_list(X); 
as_list(X) when is_binary(X)  -> binary_to_list(X);
as_list(X) when is_atom(X)    -> atom_to_list(X).
     
as_atom(X) when is_atom(X) -> X;
as_atom(X) when is_binary(X) -> binary_to_atom(X,utf8);
as_atom(X) when is_list(X) -> list_to_atom(X).
     
     
%% Parallel map

%% Run operation in parallel on elements of map.
pmap(Fun, Input) when is_map(Input) ->
    Pid = self(),
    maps:fold(
      fun(Key, Value, _) ->
              spawn_link(
                fun() ->
                        Pid ! {pmap_result, Key, Fun(Value)}
                end)
      end,
      nostate,
      Input),
    tools:foldn(
      fun(_, Accu) ->
              receive
                  {pmap_result, Key, Val} ->
                      maps:put(Key, Val, Accu)
              end
      end,
      #{},
      maps:size(Input));

%% Same, but keys are treated as inputs.
pmap(Fun, Keys) ->
    pmap(Fun, maps:from_list([{K,K} || K <- Keys])).




foldn(Fun,Init,Nb) ->
    foldn(Fun,Init,Nb,0).
foldn(Fun,State,Nb,Count) ->
    case Count of
        Nb -> State;
        _  -> foldn(Fun, Fun(Count, State), Nb, Count+1)
    end.



binary_fold(Fun, State, Bin, I) ->     
    case size(Bin) of
        I -> State;
        _ -> binary_fold(Fun, Fun(binary:at(Bin,I), State), Bin, I+1)
    end.
binary_fold(Fun, Init, Bin) ->
    binary_fold(Fun, Init, Bin, 0).
binary_sum(Bin) ->            
    binary_fold(fun(A,B)->A+B end, 0, Bin).



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



%% Line splitter body.  Send {Key, Binary} and it will call Sink({Key,
%% IO_List}) for each line, keeping a map of partial lines, one for
%% each key.
%% P=spawn(fun() -> tools:line_splitter(fun(K,L) -> tools:info("~p: ~s~n", [K,L]) end) end).

%% Define server behavior elsewhere.
%line_splitter(Sink) ->
%    line_splitter(Sink, #{}).
%line_splitter(Sink, Partials) ->
%    receive 
%        {Tag, Bin} -> 
%            line_splitter(Sink, line_splitter_update(Sink, Partials, Tag, Bin))
%    end.

line_splitter_update(Sink, Partials, Key, Bin) ->
    Partial = maps:get(Key, Partials, <<"">>),
    [First | Rest] = binary:split(Bin, <<"\n">>, [global]),
    {Lines, NextPartial} = pop_tail([[Partial, First] | Rest]),
    lists:foreach(fun(Line) -> Sink(Key, Line) end, Lines),
    maps:put(Key, NextPartial, Partials).


pop_tail(List) ->
    {First, [Last]} = lists:split(length(List)-1, List),
    {First, Last}.


read_eval_print(Str, Bindings) ->
    V = try erl_eval:exprs(string_to_exprs(Str), Bindings) of
        {value, Value, _Bindings} -> Value
    catch
        error:Error -> Error;
        Throw -> Throw
    end,
    tools:format("~p~n", [V]).
                                         
string_to_exprs(Str) ->
    {ok, Tokens, _Endloc} = erl_scan:string(Str),
    {ok, Exprs}           = erl_parse:parse_exprs(Tokens),
    Exprs.


%% Transform keys of a nested map structure.b
maps_map_keys(Fun, Map) ->
    maps:from_list(
      maps:fold(
        fun(Key, Val, List) ->
                [{Fun(Key),
                  if
                      is_map(Val) -> maps_map_keys(Fun, Val);
                      true -> Val
                  end}
                 | List]
        end,
        [], Map)).
      
              
                       
    
script_line(Cmd) ->
    Port = open_port({spawn, Cmd},
                     [{line, 1024}, use_stdio, exit_status]),
    receive
        {Port, {data, {eol, Elf}}} -> Elf
    end.

pair({H,T})   -> [H|T];
pair([_|_]=P) -> P.

    
filter_tag(Tag,List) ->
    [tl(pair(El)) || El <- lists:filter(fun(El) -> hd(pair(El)) == Tag end, List)].

filter_tags([],List) -> List;
filter_tags([T|TS],List) ->
    filter_tags(TS,filter_tag(T,List)).



timestamp() ->
    {Mega, Secs, _} = erlang:timestamp(),
    Mega * 1000000 + Secs.

timestamp_us() ->
    {Mega, Secs, USecs} = erlang:timestamp(),
    (Mega * 1000000 + Secs) * 1000000 + USecs.
    
    
format(Msg, Args) ->
    lists:flatten(io_lib:format(Msg, Args)).


annotate_pid(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        _ -> Pid
    end.

info(Msg) -> info(Msg,[]).
info(Msg, Args) -> info(annotate_pid(self()), Msg, Args).
info(Tag, Msg, Args) ->
    Str = format("~p: " ++ Msg, [Tag|Args]),
    io:format("~s",[Str]).


%% Bytes with default.
padded_at({Pad,Bytes},Index) ->
    case (Index >= 0) and (Index < byte_size(Bytes)) of
        true  -> binary:at(Bytes, Index);
        false -> Pad
    end.
padded_range(Mem, Begin, Nb) ->
    [padded_at(Mem,I) || I <- lists:seq(Begin,Begin+Nb-1)].



    

%% Signed to Unsigned, 16-bit
s2u_16(S) ->
    case (S < 0) of
        true  -> S + 16#10000;
        false -> S
    end.



register(Name, Pid) ->
    case whereis(Name) of
        Pid ->
            ignore_already_registered;
        undefined ->
            erlang:register(Name, Pid);
        Other ->
            info("register: ~p,~p taken ~p~n",
                 [Name, Pid, Other])
    end.


random_uniform_list(Max,N) ->
    [random:uniform(Max) || _ <- lists:seq(1,N)].
random_binary(N) ->
    list_to_binary(random_uniform_list(255,N)).






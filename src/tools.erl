%% (c) 2018 Tom Schouten -- see LICENSE file

-module(tools).
-export([info_p/1, info/1, info/2, info/3, info_subscribe/1, info_unsubscribe/1,
         unhex/1, unhex_string/1, hex/1, hex_list/1, hex4/1, hex8/1, hex16/1, hex_u32/1,
         strunk/1, getter/1, 
         format/2, format_binary/2,
         creader/1, int/1, float/1, hex_data/1, enumerate/1, chunks/2, nchunks/3, grid_chunks/3,
         unpack/2, unpack_s32/1, unpack_u16/1, unpack_s16/1,
         p_rem/2, p_div_rem/2, p_div/2,
         n_rem/2, n_div_rem/2, n_div/2,
         round_up/2, round_down/2,
         snap_to_grid/3,
         not_false/1,
         mid/2, mid/3,
         csv_read/1,
         pad/3,
         padded_at/2, padded_range/3, padded_insert/4,
         enumerate/2,
         list_at/2, list_update_with/3, unique/1,
         list_divide/2,
         tuple_to_list/1, as_binary/1, as_binaries/1, as_list/1, as_atom/1,
         pmap/2,
         foldn/3,
         binary_fold/3, binary_sum/1, binary_join/2,
         binary_concat/1,
         mask_bits/1,
         maps_apply/4, maps_append/3, maps_count/2, maps_inverse/1, maps_inverse_partition/1,
         maps_update_path/4, maps_find_path/2, maps_merge_paths/2,
         tagged_index/2,
         map_to_list/1,
         pop_tail/1,
         read_eval_print/2, string_to_exprs/1,
         annotate_pid/1,
         maps_map_keys/2,
         map_find_list/2,
         members/2,
         s2u_16/1,
         random_uniform_list/2,
         random_binary/1,
         timestamp/0,
         timestamp_us/0,
         filter_tag/2, filter_tags/2,
         register/2,
         register_suffix/2,
         port_pid/1, port_kill/2, spawn_port/3,
         xmlElement_attributes/1, xmlAttribute_pair/1, xmlElement_attributes_proplist/1,
         proxy/1,
         max_gt/2, max_i/2, min_i/2,
         map_inverse/1,
         first_ok/1,
         re_case/2,
         apply/2,
         become/1,
         process_dictionary_get_value/2,
         reload_from_beam_list/1,
         transpose/1,
         filename_extension/1,
         clean_filename/1,
         format_stacktrace/2,
         intersperse/2,
         node_to_host/1,
         tmpdir/2,
         race/3,
         head_and_tails/2
        ]).

-ifdef(EUINIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Ad-hoc tools collection.


      
%% Collection of generic tools for Erlang.


unhex([]) -> [];
unhex([H|[L|T]]) ->
    [list_to_integer([H,L], 16) | unhex(T)].

unhex_string(Hex) ->
    List = unhex(Hex),
    {Str,_} = lists:splitwith(fun(V) -> V =/= 0 end, List),
    Str.
                         

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

-spec strunk(string()) -> string().
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

%% Similar, but grid-aligned, and using start/endx rep.
grid_chunks(_SZ, Start, Endx) when Start >= Endx -> [];
grid_chunks(SZ, Start, Endx) -> 
    Low = tools:p_div(Start, SZ) * SZ,
    High = Low + SZ,
    if (Endx > High) -> [{Start,High} | grid_chunks(SZ, High, Endx)];
       true -> [{Start,Endx}] end.




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
%% themselves need to be word-aligned.  See fold:chunks for a more
%% flexible approach.

-ifdef(EUNIT).
unpack_test_() ->
    [?_assert(unpack_u16(<<1,2,3,4>>)         =:= [16#0201, 16#0403]),
     ?_assert(unpack_u16([<<1,2>>,<<3,4>>])   =:= [16#0201, 16#0403]),
     ?_assert(unpack_u16([<<1,2>>,[<<3,4>>]]) =:= [16#0201, 16#0403])
    ].
-endif.


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




%% integer division with positive and negative remainder.
-ifdef(EUNIT).
p_n_test_() ->
    [?_assert(p_div_rem( 10,3) =:= { 3, 1}),
     ?_assert(p_div_rem(-10,3) =:= {-4, 2}),
     ?_assert(n_div_rem(-10,3) =:= {-3,-1}),
     ?_assert(n_div_rem( 10,3) =:= { 4,-2})
    ].
-endif.

%% Divide and modulo with remainder >= 0
-spec p_rem(integer(), pos_integer()) -> non_neg_integer().
p_rem(X,Y) ->
    R = X rem Y,
    case R >= 0 of
        true -> R;
        false -> R + Y
    end.
-spec p_div_rem(integer(), pos_integer()) -> {integer(), non_neg_integer()}.
p_div_rem(X,Y) ->
    R = p_rem(X,Y),
    Q = (X - R) div Y,
    {Q,R}.
-spec p_div(integer(), pos_integer()) -> integer().
p_div(X,Y) -> {Q,_} = p_div_rem(X,Y), Q.


%% Divide and modulo with remainder =< 0
-type non_pos_integer() :: integer().  %% FIXME: how to express this?
-spec n_rem(integer(), pos_integer()) -> non_pos_integer().
n_rem(X,Y) ->
    R = X rem Y,
    case R =< 0 of
        true -> R;
        false -> R - Y
    end.
-spec n_div_rem(integer(), pos_integer()) -> {integer(), non_pos_integer()}.
n_div_rem(X,Y) ->
    R = n_rem(X,Y),
    Q = (X - R) div Y,
    {Q,R}.
-spec n_div(integer(), pos_integer()) -> integer().
n_div(X,Y) -> {Q,_} = n_div_rem(X,Y), Q.

    

%% Round up/down to next, prev multiple
round_up  (El,Chunk) -> Chunk * n_div(El, Chunk).
round_down(El,Chunk) -> Chunk * p_div(El, Chunk).

snap_to_grid(Value, Offset, Step) ->
    Units = (Value - Offset) / Step,
    N = round(Units),
    Offset + N * Step.

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

binary_concat(List) ->
    lists:foldr(
      fun(A,B) -> <<A/binary, B/binary>> end,
      <<>>, List).


transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
    
    
enumerate([],_) -> [];
enumerate([H|T],N) -> [{N,H}|enumerate(T,N+1)].


%% It is a little unfortunate that Erlang uses 1-base addressing in
%% lists:nth, but 0-base for Binary.  To reduce confusion, it makes
%% sense to use 0-base for lists as well.
list_at(List, Index) ->
    lists:nth(Index + 1, List).

%% Also 0-base index    
list_update_with(Index, Fun, List) ->
    [case N of Index -> Fun(E); _ -> E end
     || {N,E} <- enumerate(List)].

%% unique(L) ->
%%     lists:sort(
%%       sets:to_list(sets:from_list(L))).
unique(List) ->
    dedup_sorted(lists:sort(List)).
dedup_sorted([A|[B|_]=Rest]) ->
    SRest = dedup_sorted(Rest),
    case A == B of
        true  -> SRest;
        false -> [A | SRest]
    end;
dedup_sorted(L) -> L.


             
%% Record to Maps translation
tuple_to_list(Tuple) ->
    [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))].

%% Divide up a list into a couple of sublists.
list_divide(List, N) ->
    list_divide(List, N, n_div(length(List), N)).
list_divide(_, 0, _) -> [];
list_divide(List, N, Nb) -> 
    Head = lists:sublist(List, Nb),
    Tail = lists:nthtail(length(Head), List),
    [Head | list_divide(Tail, N-1, Nb)].
    
    
    


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
-ifdef(EUNIT).
pmap_test_() ->
    F = fun(X) -> X + 1 end,
    [?_assert(pmap(F, [1, 2, 3])         =:= #{1 => 2, 2 => 3, 3 => 4}),
     ?_assert(pmap(F, #{a => 1, b => 2}) =:= #{a => 2, b => 3})].
-endif.

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
pmap(Fun, Keys) when is_list(Keys) ->
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





%% Append value to a list tagged in a map.
maps_apply(Tag, Fun, Default, Map) ->
    Val = maps:get(Tag, Map, Default),
    maps:put(Tag, Fun(Val), Map).

maps_append(Tag, Val, Map) ->
    maps_apply(Tag, fun (List) -> List ++ [Val] end, [], Map).

maps_count(Tag, Map) ->    
    maps_apply(Tag, fun (Val) -> 1 + Val end, 0, Map).

%% Compute inverse.  Dupliate keys will get dropped according the
%% natural sort order.
maps_inverse(Map) ->
    maps:from_list(
      lists:sort(
        [{V,K} || {K,V} <- maps:to_list(Map)])).

%% Same, but collect duplicates in a list.
maps_inverse_partition(Map) ->
    lists:foldl(
      fun({V,K},M) ->
              Vs = maps:get(K,M,[]),
              maps:put(K,[V|Vs],M)
      end,
      #{},
      maps:to_list(Map)).
    


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

%% Collect whatever is found.      
map_find_list(Keys,Map) ->
    lists:append(
      lists:map(
        fun(Key) ->
                case maps:find(Key, Map) of
                    {ok, Val} -> [{Key,Val}];
                    _ -> []
                end
        end,
        Keys)).
              
%% Convert member function to member list
members(IsMember, ProbeList) when is_function(IsMember) ->
    lists:foldl(
      fun(El,List) ->
              case IsMember(El) of
                  true -> [El|List];
                  false -> List
              end
      end, [], ProbeList).
                      

    

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

format_binary(Msg, Args) ->
    iolist_to_binary(io_lib:format(Msg, Args)).

process_dictionary_get_value(Pid, Key) ->
    case erlang:process_info(Pid, [dictionary]) of
        [{dictionary, PropList}] -> proplists:get_value(Key, PropList);
        _ -> undefined
    end.
        

%% If process is registered then return that name.  Otherwise, return
%% the Atom associated to the name key in the process dictionary, or
%% the Pid if that was not defined.
annotate_pid(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} ->
            Name;
        _ ->
            case process_dictionary_get_value(Pid, info_name) of
                undefined -> Pid;
                {annotate_recursive, OtherPid, Tag} ->
                    %% Allow for recursive annotation for tasks that
                    %% do not have a name until later.
                    {annotate_pid(OtherPid), Tag};
                Name -> Name
            end
    end.

%% Use serv:bc to forward to multiple clients.
%% FIXME: use gen_event
info(Msg) -> info(Msg,[]).
info(Msg, Args) -> info(annotate_pid(self()), Msg, Args).
info(Tag, Msg, Args) ->
    TagMsg = "~999p: " ++ Msg,
    TagArgs = [Tag|Args],
    case whereis(info_bc) of
        undefined -> io:format(TagMsg,TagArgs);
        BC -> BC ! {foreach, fun(Pid) -> catch io:format(Pid,TagMsg,TagArgs) end}, ok
    end.
    
info_p(Msg) ->
    tools:info("~p~n",[Msg]).

info_bc() -> 
    %% Start if not started, but make sure it is not linked.
    BC = serv:up(info_bc, {spawner, fun serv:bc_start/0}),
    unlink(BC),
    BC.
    

%% E.g. Pid == erlang:group_leader()
info_subscribe(Pid) ->
    BC = info_bc(),
    info_bc ! {subscribe, Pid},
    {ok, BC}.

info_unsubscribe(Pid) ->
    BC = info_bc(),
    info_bc ! {unsubscribe, Pid},
    {ok, BC}.
    

%% Bytes with default.
padded_at({Pad,Bytes},Index) ->
    case (Index >= 0) and (Index < byte_size(Bytes)) of
        true  -> binary:at(Bytes, Index);
        false -> Pad
    end.
padded_range(Mem, Begin, Nb) ->
    [padded_at(Mem,I) || I <- lists:seq(Begin,Begin+Nb-1)].

padded_insert({Pad, Original}, Offset, Replacement, MaxLen) ->
    OriginalEnd = size(Original),
    ReplacementEnd = Offset + size(Replacement),
    End = max(OriginalEnd, ReplacementEnd),
    [if
         %% Mutation+padding as combiation of 3 index functions.
         ((N >= Offset) and (N < ReplacementEnd)) ->
             binary:at(Replacement, N-Offset);
         N < OriginalEnd ->
             binary:at(Original, N);
         true ->
             Pad
     end || N <- lists:seq(0, min(End,MaxLen)-1)].


%% Similar, more straightforward.
pad(IOList, N, Fill) ->
    binary_part(
      iolist_to_binary(
        [IOList,
         binary:copy(Fill, N)]),
      {0,N}).
         
    

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

register_suffix(Name, Pid) ->
    register_suffix(Name, Pid, 0).
register_suffix(Name, Pid, N) ->
    NewName = list_to_atom(format("~s~p",[Name,N])),
    case (catch erlang:register(NewName, Pid)) of
        true -> true;
        _ -> register_suffix(Name, Pid, N+1)
    end.
    

%% FIXME: note that this is 1-Max, not 0-Max-1
random_uniform_list(Max,N) ->
    [rand:uniform(Max) || _ <- lists:seq(1,N)].
random_binary(N) ->
    list_to_binary(random_uniform_list(255,N)).


max_gt(Gt, [H|T]) ->
    lists:foldl(
      fun(El,Max) -> case Gt(El,Max) of true -> El; false -> Max end end,
      H,T).
max_i(I, List) ->
    max_gt(fun(A,B) -> I(A) > I(B) end, List).
min_i(I, List) ->
    max_gt(fun(A,B) -> I(A) < I(B) end, List).
              

not_false(false) -> false;
not_false(_) -> true.


port_pid(Port) ->
    {_, OsPid} = proplists:lookup(os_pid, erlang:port_info(Port)),
    OsPid.

port_kill(Port,Signal) ->
    os:cmd(io_lib:format("kill -~s ~p", [Signal, port_pid(Port)])).



%% Abstract open_port
%%
%% Suppose that an Erlang service is integrated into a heterogeneous
%% system that is a mix of Erlang and non-Erlang systems, and has
%% networking components that are not based on Erlang distribution.
%%
%% The Erlang program needs to delegate starting of a port to the
%% system in which it is integrated.  This can be done by having the
%% host 1. Resolve names, and 2. start the program using Erlang's
%% open_port/3.
%%
%% The API then consists of the following:
%% - An abstraction over open_port/3  (tools:spawn_port/3)
%% - A way to represent remote locations
%%
%% Note that remote locations do not need to be Erlang nodes.  They
%% can be anything that is meaningful in the larger system.  These
%% references are passed into the Erlang service from the outside and
%% are only used in an abstract way, e.g. by passing it to a
%% spawn_port function.


%% Some examples: if you can't run Erlang on a particular piece of hardware,
%% instead run a C program that exposes the node's hardware (sensors,
%% actuators, communication devices) as a port program, and run the
%% Erlang side of the driver somewhere else in the hive.  I've used
%% this on tiny OpenWRT nodes that start port programs through
%% dropbear SSH, and on hub nodes that expose a collection of
%% USB-connected microcontrollers.

%% The host system provides the spawner as a "reloadable closure" that
%% needs to be invoked using tools:apply/3.
%% The following functions implement the API.

%% spawn_port(Cmd, Args, Opts).
%% - Env:  misc extra information, e.g. host
%% - Cmd:  iolist command name
%% - Args: list of iolist arguments
%% - Opts: options passed to erlang:open_port/3


%% Some examples:

%% case: Spawner is abstract and defined in the local environment.
%% This can e.g. be used when starting of an Erlang port management
%% process is done in an environment that has some global
%% infrastructure, e.g. exo's SSH nodes.
spawn_port(Env = #{ spawn_port := SpawnPort }, Program={_Cmd,_ArgList}, Opts) ->
    log:info("~p~n", [Env]),
    SpawnPort(Env, Program, Opts);


%% case: To start a local process, we only need to know the directory
%% of the port binary.
spawn_port(_Env = #{ port_dir := PortDir }, {Cmd,ArgList}, Opts) ->
    log:info("~p~n", [_Env]),
    CmdLine = run:shell_command(tools:format("~s/~s",[PortDir,Cmd]), ArgList),
    CmdLineBin = iolist_to_binary(CmdLine),
    open_port({spawn, CmdLineBin}, Opts);

%% case: If the port belongs to an app, we can find the port dir.
spawn_port(_Env = #{ app := App }, Program, Opts) ->
    log:info("~p~n", [_Env]),
    spawn_port(#{ port_dir => code:priv_dir(App) }, Program, Opts);

%% case: The default case is a program exported in this app.
spawn_port(_Env, Program, Opts) ->
    log:info("~p~n", [_Env]),
    spawn_port(#{ app => erl_tools }, Program, Opts).



%% FIXME: this hangs: (include-lib "xmerl/include/xmerl.hrl"), so
%% define accessors based on actual tuple structure instead.
xmlElement_attributes({xmlElement,_,_,_,_,_,_,As,_,_,_,_}) -> As.
xmlAttribute_pair({xmlAttribute,Tag,_,_,_,_,_,_,Val,_}) -> {Tag, Val}.
xmlElement_attributes_proplist(El) -> %% Convert element's attributes to proplist
    lists:map(fun xmlAttribute_pair/1, xmlElement_attributes(El)).


proxy(Pid) ->
    receive Msg -> Pid ! Msg end,
    proxy(Pid).
             

map_inverse(Map) ->
    maps:fold(
      fun(K,V,M) -> maps:put(V,K,M) end, #{}, Map).



first_ok([{ok,_}=V|_]) -> V;
first_ok([_|L])        -> first_ok(L); 
first_ok([])           -> error.

%% Generic regexp router.
re_case(_, []) -> false;
re_case(Data, [{Regex,Fun}|Rest]) ->
    case re:run(Data,Regex,[{capture,all,list}]) of
        {match,[_|Args]} ->
            Fun(Args);
        _Err ->
            %%tools:info("~p~n",[_Err]),
            re_case(Data, Rest)
    end.


%% Generalized function application: anonymous functions and
%% lambda-lifted closures.
apply({M,F,EnvArgs}, Args) when
      is_atom(M) and is_atom(F) and is_list(EnvArgs) and is_list(Args) ->
    erlang:apply(M,F,EnvArgs ++ Args);
apply(F, Args) when
      is_function(F) and is_list(Args) ->
    erlang:apply(F, Args).


become(Name) ->
    case whereis(Name) of
        undefined ->
            erlang:register(Name, self());
        Pid ->
            tools:info("stopping ~s~n", [Name]),
            Pid ! stop,
            timer:sleep(1000),
            become(Name)
    end.

maps_find_path([Key], Map) -> 
    maps:find(Key, Map);
maps_find_path([Key|Path], Map) -> 
    case maps:find(Key, Map) of
        {ok, SubMap} -> maps_find_path(Path, SubMap);
        Error -> Error
    end.
    

%% maps:update_with/3 is not available in all releases.
maps_update_with(Key, Fun, Map, Default) ->
    Init = maps:get(Key, Map, Default),
    maps:put(Key, Fun(Init), Map).

%% Update variable in nested map structure, creating path if it doesn't exist.
maps_update_path([Key], Fun, Map, Default) ->
    maps_update_with(Key, Fun, Map, Default);
maps_update_path([Top|Path], Fun, Map, Default) ->
    maps_update_with(
      Top, 
      fun(SubMap) -> maps_update_path(Path, Fun, SubMap, Default) end,
      Map,
      #{}).

maps_merge_paths(Map, PathMap) ->
    maps:fold(
      fun(Path,Val,M) ->
              maps_update_path(
                Path, 
                fun(_) -> Val end,
                M,
                default_ignored)
      end,
      Map, PathMap).



%% RELOAD.  This is for development only.  The build system will
%% provide a list of rebuilt modules in a file, one module per line.
%% See erl_tools/bin/update-beam.lst
reload(M) ->
    info("reload ~p~n", [M]),
    code:purge(M), code:load_file(M).
reload_from_beam_list(Filename) ->
    %% Keep database open during reload.  
    %% Killing it seems to cause all kinds of sync problems.
    %% DB = db:db(), unlink(DB), exit(DB, kill), 
    lists:foreach(fun reload/1, updated_modules(Filename)).

updated_modules(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinMods = binary:split(Data, [<<"\n">>], [global,trim_all]),
    Mods = [binary_to_atom(BinMod, utf8) || BinMod <- BinMods],
    info("updated_modules = ~p~n", [Mods]),
    Mods.


clean_filename(Filename) ->
    lists:foldl(
      fun(Pattern, Name) ->
              re:replace(Name, Pattern, "_", [global])
      end,
      Filename,
      %% FIXME: There are probably more invalid characters.
      %% There should be a library routine for this...
      ["\s", "/", "\\\\"]).

format_stacktrace(_FixPath, none) -> %% ??
    ["<no stacktrace>"];

format_stacktrace(FixPath, Lst) ->
    [format_stacktrace_line(FixPath, Entry) || Entry <- Lst].


format_stacktrace_line(FixPath, {Module, Function, Arity, PropList}) ->
    %% FIME: Why is erlang:module_info/1 source different from what's recorded in the stacktrace?
    %%File = proplists:get_value(file, PropList),
    File = reflection:module_source_raw(Module),
    Line = proplists:get_value(line, PropList, 0),  %% Default makes Emacs parser work.
    FmtFun =
        case Arity of
            %% See documentation of erlang:get_stacktrace()
            _ when is_number(Arity) ->  %% FIXME: is arity() always a number?
                format("~p:~p/~p", [Module,Function,Arity]);
            _ when is_list(Arity) ->
                format("~p:~p/~p, Args=~p", [Module, Function, length(Arity), Arity])
        end,


    format("~s:~p: ~s~n", [FixPath(File), Line, FmtFun]).


filename_extension(File) ->
    binary_to_atom(
      lists:last(re:split(filename:basename(File),"\\.")),
      utf8).

intersperse([],_) -> [];
intersperse(List, Sep) ->
    tl(lists:append([[Sep, El] || El <- List])).
node_to_host(Node) ->                
    [_,IP] = re:split(atom_to_list(Node),"@"), IP.
    

tmpdir(Base, Tag) ->
    tmpdir(Base, Tag, 6).

tmpdir(Base, Tag, NbBytes) ->
    Suffix = 
        base64:encode(
          binary:part(
            crypto:hash(
              sha,
              term_to_binary(make_ref())),
            0, NbBytes)),
    Name =
        tools:format("~s/~s.~s",[Base, Tag, Suffix]),
    case file:make_dir(Name) of
        ok -> {ok, Name};
        {error, eexists} -> tmpdir(Base, Tag, NbBytes);
        Error={error, _} -> Error
    end.

%% Perform functions against nodes in parallell and return the first
%% successful result.  Note that Nodes can be anything.
race(Nodes, TimeOut, MaybeNodeFun) ->
    MainPid = self(),
    MainRef = erlang:make_ref(),
    spawn(
      fun() -> 
          WaitPid = self(),
          WaitRef = erlang:make_ref(),
          lists:foreach(
            fun(Node) ->
                spawn(
                  fun() ->
                      WaitPid ! {WaitRef, Node, MaybeNodeFun(Node)}
                  end)
            end,
            Nodes),
          Rv = race_wait(WaitRef, Nodes, TimeOut),
          MainPid ! {MainRef, Rv}
      end),
    receive
        {MainRef, Rv} -> Rv
    after TimeOut ->
            log:info("WARNING: race_nodes: timeout~n"),
            error
    end.
race_wait(_WaitRef, [], _) -> error;
race_wait(WaitRef, Nodes, TimeOut) ->
    receive
        {WaitRef, _Node, {ok, _}=Rv} ->
            Rv;
        {WaitRef, Node, error} ->
            race_wait(WaitRef, lists:delete(Node,Nodes), TimeOut)
        after TimeOut ->
                log:info("WARNING: race_nodes_wait: timeout~n"),
                error
    end.


head_and_tails(As,Bs) ->
    {H1,As1,Bs1} = htt([], As,Bs),
    {lists:reverse(H1),As1,Bs1}.
htt(H,[],Bs) -> {H,[],Bs};
htt(H,As,[]) -> {H,As,[]};
htt(H,[A|_]=As,[B|_]=Bs) ->
    case A == B of
        true  -> htt([A|H],tl(As),tl(Bs));
        false -> {H,As,Bs}
    end.


-ifdef(TEST).
-include("../test/tools.expect").
expect_test() ->
    expect:run_form(
      filename:dirname(?FILE)++"/../test/tools.expect",
      fun tools_expect/0).
-endif.



%% Keep these here.  They don't pretty-print well as expect tests.
-ifdef(EUNIT).
hex_test_() ->
    [?_assert(hex16(16#0123) =:= "0123"),
     ?_assert(hex8 (16#01)   =:= "01"),
     ?_assert(hex4 (16#01)   =:= "1"),
     ?_assert(unhex("012345")  =:= [16#01, 16#23, 16#45]),
     ?_assert(hex([16#01, 16#23, 16#45]) =:= "012345"),
     ?_assert(hex_u32(16#01234567) =:= "67452301")
    ].
-endif.

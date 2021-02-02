-module(leb128).
-export([test/1]).

%% Erlang reference implementation of LEB128 parser/printer + tree
%% structure extension.

%% Companion to tag_u32.
%% Consider tag_u32 "path message" to be the zipper of a leb128 tree traversal.
%%
%% leb128 can be used to represent transactions, with state dump being
%% one of the special cases (machine load/save).

%% The array type consists of two variant: typed arrays, where every
%% element is the same and only one type tag is used per array, and
%% uples, where every element can be any type and is prefixed by its
%% own type tag.  This is necessary to avoid lots of type tags in
%% situations where data structure are statically typed.


%% base type tags  %% mnemonics
-define(T_NOP,0).  %% n0p, padding
-define(T_INT,1).  %% 1 = I
-define(T_TUP,2).  %% 2ple
-define(T_BIN,3).  %% 3 = B
-define(T_ARR,4).  %% 4 = A 
-define(T_SYM,5).  %% 5 = S
%% 6
-define(T_TAG,7).  %% 7 = T



%% Data is self-delimiting, so eof is always an error.
read_byte(_Env=#{igen := Gen}) ->
    case igen:read(Gen) of
        {data, Data} -> Data;
        eof -> throw(eof)
    end.
read_bytes(Env, N) ->
    iolist_to_binary(
      [read_byte(Env) || _ <- lists:seq(1,N)]).

shift({Val, N}, NewVal) ->
    {Val + (NewVal bsl N), N+7}.
val({Val,_N}) ->
    Val.
read_int(Env) ->
    read_int(Env, {0,0}).
read_int(Env, Accu) ->
    Byte = read_byte(Env),
    <<More:1, Int:7>> = <<Byte>>,
    Accu1 = shift(Accu, Int),
    case More of
        0 -> val(Accu1);
        1 -> read_int(Env, Accu1)
    end.

%% Tuples are arrays that contain any combination of types.
read_tup(Env) ->
    N = read_int(Env),
    list_to_tuple([read(Env) || _ <- lists:seq(1,N)]).
%% Arrays are typed.  It is done this way to make arrays of small
%% integers have a compact representation.
read_arr(Env) ->
    N = read_int(Env),
    Type = read_int(Env),
    read_elements(Env, Type, N).

read_elements(Env, Type, N) ->
    [read_type(Env, Type) || _ <- lists:seq(1,N)].

read_bin(Env) ->
    N = read_int(Env),
    read_bytes(Env, N).

read_sym(Env) ->
    binary_to_atom(read_bin(Env), utf8).


read_type(Env, Type) ->
    case Type of
        %% Type tags.
        ?T_INT -> read_int(Env);
        ?T_TUP -> read_tup(Env);
        ?T_ARR -> read_arr(Env);
        ?T_BIN -> read_bin(Env);
        ?T_SYM -> read_sym(Env);
        ?T_TAG -> read_tag(Env);
        Tag -> throw({type,Tag})
    end.

read(Env) ->
    Type = read_int(Env),
    case Type of
        %% Nop is special: it is completely ignored and is just there
        %% for padding/aligning.
        ?T_NOP -> read(Env);
        _ -> read_type(Env, Type)
    end.

read_tag(Env) ->
    NF = read_int(Env), From = read_elements(Env, ?T_INT, NF),
    NT = read_int(Env), To   = read_elements(Env, ?T_INT, NT),
    NB = read_int(Env), Bin  = read_bytes(Env, NB),
    {tag,{From,To,Bin}}.

write_byte(_Env = #{sink := Sink }, Byte) ->
    Sink({data,Byte}).
write_bytes(Env, Bytes) ->
    List = binary_to_list(Bytes),
    lists:foreach(fun(B) -> write_byte(Env, B) end, List).
write_int(Env, N) ->
    case N =< 127 of
        true ->
            write_byte(Env, N);
        false ->
            write_byte(Env, 128 bor (N band 127)),
            write_int(Env, N bsr 7)
    end.

write_type(Env, Type, Term) ->
    %% Assert.  This is necessary for arrays.
    Type = type(Term),  
    case Type of
        ?T_INT -> 
            write_int(Env, Term);
        ?T_BIN ->
            write_int(Env, size(Term)),
            write_bytes(Env, Term);
        ?T_SYM ->
            Bin = atom_to_binary(Term, utf8),
            write_int(Env, size(Bin)),
            write_bytes(Env, Bin);
        ?T_TUP ->
            List = tuple_to_list(Term),
            write_int(Env, length(List)),
            lists:foreach(
              fun(El) -> write(Env, El) end, List);
        %% Special "static" types that have structural properties.
        %%
        %% Arrays structure: all elements are the same type.  Type is
        %% stored once.
        %%
        %% FIXME: Nested arrays are not encoded properly.  The types
        %% need to propagate all the way up.  Currently nested arrays
        %% can have inner arrays that are of different types (but
        %% still arrays), eg, [[1,2,3],[abc]]. That just seems odd.
        ?T_ARR ->
            Type1 =
                case Term of
                    [] -> ?T_INT;
                    [E|_] -> type(E)
                end,
            write_int(Env, length(Term)),
            write_int(Env, Type1),
            write_elements(Env, Type1, Term);

        %% leb128-encoded tag_u32.
        ?T_TAG ->
            {tag,{From,To,Bin}} = Term,
            write_int(Env, length(From)), write_elements(Env, ?T_INT, From),
            write_int(Env, length(To)),   write_elements(Env, ?T_INT, To),
            write_int(Env, size(Bin)),    write_bytes(Env, Bin)
    end.

write_elements(Env, Type, List) ->
    lists:foreach(
      fun(E) -> write_type(Env, Type, E) end,
      List).


write(Env, Term) ->
    Type = type(Term),
    write_int(Env, Type),
    write_type(Env, Type, Term).

type({tag,_})             -> ?T_TAG;
type(N) when is_number(N) -> ?T_INT;
type(B) when is_binary(B) -> ?T_BIN;
type(T) when is_tuple(T)  -> ?T_TUP;
type(L) when is_list(L)   -> ?T_ARR;
type(A) when is_atom(A)   -> ?T_SYM.

to_list(Term) ->
    sink:gen_to_list(
      fun(Sink) ->
              write(#{sink => Sink}, Term),
              Sink(eof)
      end).

test({read,List}) ->
    Env = #{ igen => igen:from_list(List) },
    read(Env);
test({write,Term}) ->
    to_list(Term);
test({loop,Term}) ->
    test({read,test({write,Term})});
test(loop) ->
    Term = {abc,<<"def">>,
            1,10,100,1000,10000,100000,
            10000000000000000000000000,
            [],[123],[1,2,3],[[4,5],[6,7]],
            {tag,[],[1,2,3],<<"bin">>}},
    List = test({write, Term}),
    Nops = [0,0,0,0,0],
    true = (Term == test({read, Nops ++ List ++ Nops})).

                             



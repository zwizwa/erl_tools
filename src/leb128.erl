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


%% type tags       %% mnemonics
-define(T_BIN,0).  %% zero: type unknown
-define(T_INT,1).  %% 1 = I
-define(T_TUP,2).  %% twople / tuple
-define(T_TRE,3).  %% 3 = tree node / path encoded
-define(T_ARR,4).  %% 4 = A 
-define(T_SYM,5).  %% 5 = S

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
        Tag -> throw({type,Tag})
    end.

read(Env) ->
    read_type(Env, read_int(Env)).


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
            lists:foreach(
              fun(E) -> write_type(Env, Type1, E) end,
              Term)
    end.

write(Env, Term) ->
    Type = type(Term),
    write_int(Env, Type),
    write_type(Env, Type, Term).

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
    test({read,test({write,Term})}).
                             



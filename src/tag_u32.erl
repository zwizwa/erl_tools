%% Protocol wrappers for TAG_U32-based RPC protocol.

-module(tag_u32).
-export([call/2, call/3,
         find/3, resolve/2,
         meta/3, name/2, type/2,
         dict/1, sub/2,
         apply/3]).

%% Generic RPC
%%
%% In this module we use reverse path notation, bottom-to-tom,
%% i.e. zipper or stack view, which is easier to work with.  TAG_U32
%% uses top-to-bottom order.
callr(Pid, RPath) ->
    call(Pid, lists:reverse(RPath)).

call(Pid, Path) ->
    call(Pid, Path, <<>>).

call(Pid, Path, Bin) ->
    %% FIXME: Later, use only resolved paths.
    NPath = resolve(Pid, Path),
    obj:call(Pid, {req_u32, NPath, Bin}, 2000).


%% Incremental resolve
find(Pid, Path, Atom) when is_atom(Atom) ->
    Bin = atom_to_binary(Atom,utf8),
    case call(Pid, Path ++ [-1,4], Bin) of
        {[0,Id],<<>>} -> {ok, Id};
        _ -> error
    end.
resolve(Pid, Path) ->
    resolve(Pid,[],Path).
resolve(_,_,[]) -> [];
resolve(Pid, Upper, [X|Rest]) ->
    Id = if is_number(X) -> X; true -> {ok, N} = find(Pid, Upper, X), N end,
    [Id|resolve(Pid, Upper++[Id], Rest)].


%% %% Resolve based on dictionary.
%% resolve(_Dict, []) -> [];
%% resolve(Dict, [Tag|Path]) ->
%%     case maps:find(Tag, Dict) of
%%         {ok, {N, Sub}} when is_map(Sub) ->
%%             [N | resolve(Sub, Path)];
%%         {ok, {N, Type}} when is_atom(Type) ->
%%             %% The remainder is opaque.
%%             %% FIXME: Check that it consists of integers?
%%             [N | Path]
%%     end.


%% Metadata commands
meta(Pid, [N|RPath0], Cmd) ->
    case callr(Pid, [N, Cmd, 16#FFFFFFFF | RPath0]) of
        {[0], Name} ->
            {ok, binary_to_atom(Name,utf8)};
        _ ->
            error
    end.

%% Map identifier to name string.
name(Pid, RPath) -> meta(Pid, RPath, 2).
type(Pid, RPath) -> meta(Pid, RPath, 3).
    
    

%% Retreive the whole dictionary.

%% Old style.  Currently just using name/2.
%% dict(Pid) ->
%%     fold:to_list(
%%       fold:map(
%%         fun(Idx) ->
%%                 Id = atom_index_to_id(Pid, Idx),
%%                 {Id, name(Pid, Id)} end,
%%         fold:range(nb_atoms(Pid)))).
    
dict(Pid) ->
    sub(Pid, []).
sub(Pid, Path) ->
    maps:from_list(dict_list(Pid, 0, Path)).
dict_list(Pid,N,Path) ->
    case name(Pid,[N|Path]) of
        error -> [];
        {ok, Name} -> 
            Path1 = [N|Path],
            {ok, Type} = type(Pid,Path1),
            Sub =
                case Type of
                    map ->
                        %% Recurse.
                        sub(Pid, Path1);
                    _ ->
                        %% Atoms don't have substructure, just a type.
                        Type
                end,
            [{Name,
              {N,Sub}
              %% Sub
             } 
             |dict_list(Pid, N+1, Path)]
    end.
            

%% FIXME: API is not stable.
apply(Pid, Proc, Nodes) ->
    {[Node],<<>>} = call(Pid,[class,Proc,apply] ++ Nodes),
    Node.

    

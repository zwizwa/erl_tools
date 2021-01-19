%% Protocol wrappers for TAG_U32-based RPC protocol.

-module(tag_u32).
-export([call/2, call/3, send/3,
         find/3, resolve/2,
         dir/2,
         apply/3]).

%% Generic RPC
%%
%% In this module we use reverse path notation, bottom-to-tom,
%% i.e. zipper or stack view, which is easier to work with.  TAG_U32
%% uses top-to-bottom order.
call_rev(Pid, RPath) ->
    call(Pid, lists:reverse(RPath)).

call(Pid, Path) ->
    call(Pid, Path, <<>>).

call(Pid, Path, Bin) ->
    %% FIXME: Later, use only resolved paths.
    NPath = resolve(Pid, Path),
    %% log:info("tag_u32:call ~p~n", [{Path,Bin}]),
    obj:call(Pid, {req_u32, NPath, Bin}, 2000).

%% This is mostly to plug into epid, so we do some automatic wrapping
%% here in case of single numbers.
send(Pid, Path, Thing) ->
    Msg = case is_number(Thing) of true -> [Thing]; false -> Thing end,
    %% Just use synchronous calls for now.  It crashes earlier...
    Path1 = Path ++ Msg,
    %% log:info("tag_u32:send ~p~n", [Path1]),
    call(Pid, Path1).
    

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
maybe_named_rev(Pid, [N|RPath0], Cmd) ->
    case call_rev(Pid, [N, Cmd, 16#FFFFFFFF | RPath0]) of
        {[0], <<>>} ->
            %% Empty string means the node is there, but doesn't have
            %% a name.  So we return the number instead.
            {ok, N};
        {[0], Name} ->
            {ok, binary_to_atom(Name,utf8)};
        _ ->
            error
    end.

%% Map identifier to name string.
name_rev(Pid, RPath) -> maybe_named_rev(Pid, RPath, 2).
type_rev(Pid, RPath) -> maybe_named_rev(Pid, RPath, 3).
    
    

%% Retreive the whole dictionary.

%% Old style.  Currently just using name/2.
%% dict(Pid) ->
%%     fold:to_list(
%%       fold:map(
%%         fun(Idx) ->
%%                 Id = atom_index_to_id(Pid, Idx),
%%                 {Id, name(Pid, Id)} end,
%%         fold:range(nb_atoms(Pid)))).

%% Directory is represented as lists, where index in list corresponds
%% to the id used in the protocol.
    
dir(Pid, Path) ->
    dir_rev(Pid, lists:reverse(Path)).
dir_rev(Pid, RPath) ->
    dict_list_rev(Pid, 0, RPath).
dict_list_rev(Pid,N,RPath) ->
    RPath1 = [N|RPath],
    case name_rev(Pid,RPath1) of
        error -> [];
        {ok, Name} -> 
            {ok, Type} = type_rev(Pid,RPath1),
            Sub =
                case Type of
                    map ->
                        %% Recurse.
                        dir_rev(Pid, RPath1);
                    _ ->
                        %% Atoms don't have substructure, just a type.
                        Type
                end,
            [{Name,Sub} 
             |dict_list_rev(Pid, N+1, RPath)]
    end.
            

%% FIXME: API is not stable.
apply(Pid, Proc, Nodes) ->
    {[Node],<<>>} = call(Pid,[class,Proc,apply] ++ Nodes),
    Node.




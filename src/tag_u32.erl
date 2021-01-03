%% Protocol wrappers for TAG_U32-based RPC protocol.

-module(tag_u32).
-export([call/2, meta/3, name/2, type/2, dict/1]).

%% Generic RPC
%%
%% In this module we use reverse path notation, bottom-to-tom,
%% i.e. zipper or stack view, which is easier to work with.  TAG_U32
%% uses top-to-bottom order.
callr(Pid, RPath) ->
    call(Pid, lists:reverse(RPath)).
call(Pid, Path) ->
    obj:call(Pid, {req_u32, Path}, 2000).

%% Metatdata commands
meta(Pid, [N|Path0], Cmd) ->
    {[], Name} = callr(Pid, [N, Cmd, 16#FFFFFFFF | Path0]),
    binary_to_atom(Name,utf8).

%% Map identifier to name string.
name(Pid, Path) -> meta(Pid, Path, 2).
type(Pid, Path) -> meta(Pid, Path, 3).

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
        '' -> [];
        Name -> 
            Path1 = [N|Path],
            Type = type(Pid,Path1),
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
              %% {N,Sub}  %% I really don't care about node indices atm.
              Sub
             } 
             |dict_list(Pid, N+1, Path)]
    end.
            

%% Protocol wrappers for TAG_U32-based RPC protocol.

-module(tag_u32).
-export([call/2, ctrl/2, nb_atoms/1, name/2, dict/1]).

%% Generic RPC
call(Pid, Sub) ->
    obj:call(Pid, {req_u32, Sub}, 2000).
%% Generic control path
ctrl(Pid, Sub) ->
    call(Pid, [-1 | Sub]).
%% Get number of atoms in dictionary.
nb_atoms(Pid) ->
    {[NbEl], _} = ctrl(Pid, [0]), NbEl.
%% Map index in dictionary list to identifier (this is likely the identity function)
atom_index_to_id(Pid, Idx) ->
    {[Id], _} = ctrl(Pid, [1, Idx]), Id.
%% Map identifier to name string.
name(Pid, Id) ->
    %% log:info("name ~p~n",[Id]),
    {[], Name} = ctrl(Pid, [2,Id]),
    binary_to_atom(Name,utf8).
%% Retreive the whole dictionary.
dict(Pid) ->
    fold:to_list(
      fold:map(
        fun(Idx) ->
                Id = atom_index_to_id(Pid, Idx),
                {Id, name(Pid, Id)} end,
        fold:range(nb_atoms(Pid)))).
    

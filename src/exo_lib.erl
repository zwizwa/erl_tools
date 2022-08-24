%% Exo refers to exocortex: an external information processing system
%% that augments the brain's biological high-level cognitive
%% processes.
%%
%% I take that to mean "network operating system".
%%
%% At the Erlang level exo is implemented as a supervisor registerd as
%% 'exo_sup'.  This module 'exo_lib' exposes functionality that can be
%% used to implement a (private) 'exo' Erlang app.  (FIXME: Example)
%%
%% Required functionality in 'exo'  (FIXME: Erlang module spec).
%%
%% child_mfa/1   %% Maps task ID to {Module,Function,Args) start method


-module(exo_lib).
-export([start/1, stop/1,
         start_child/1,
         child_spec/1
        ]).

start(Id) ->
    supervisor:start_child(
      exo_sup,
      child_spec(Id)).
stop(Id) ->
    supervisor:terminate_child(exo_sup, Id),
    supervisor:delete_child(exo_sup, Id).

%% Supervisor child specification for each task.
child_spec(Id) ->
    #{modules => [exo],  %% Why?
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      id => Id,
      start => {exo,start_child,[Id]}}.

start_child(Id) ->
    {M,F,A}=_MFA = exo:child_mfa(Id),
    {ok, Pid} = apply(M,F,A),
    case is_atom(Id) of
        false -> ok;
        true ->
            try register(Id, Pid), log:info("exo:start_child(~p): registered~n")
            catch C:E ->log:info("exo:start_child(~p): ~p~n", [Id, {C,E}]) end
    end,
    {ok, Pid}.


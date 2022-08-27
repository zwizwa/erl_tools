%% The name "exo" refers to exocortex: an external information
%% processing system that augments the brain's biological high-level
%% cognitive processes.
%%
%% I take that to mean "network operating system".
%%
%% At the Erlang level exo is implemented as a supervisor registerd as
%% 'exo_sup'.  This module 'exo_lib' exposes functionality that can be
%% used to implement a (private) 'exo' Erlang app.  (FIXME: Example)
%%


-module(exo_lib).
-export([start/1, stop/1, need/1, need_hubs/1,
         start_child/1, child_spec/1, children/0,
         pids/1, pid/2, pid/1, pid_maybe/2, pid_maybe/1
         
        ]).

start(Id) ->
    supervisor:start_child(
      exo_sup,
      child_spec(Id)).
stop(Id) ->
    supervisor:terminate_child(exo_sup, Id),
    supervisor:delete_child(exo_sup, Id).

need(Id) ->
    case exo:start(Id) of
        {error,{already_started,Pid}} -> Pid;
        {ok, Pid} -> Pid
    end.
need_hubs(Name) ->
    case pids(Name) of
        [] -> [need(Name)];
        Pids -> Pids
    end.



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

children() ->
    maps:from_list(
      [{Id,Pid} || {Id,Pid,_,_} <- supervisor:which_children(exo_sup)]).

exo_nodes() ->
    [node()|nodes()].


pid(ExoName, Node) ->
    case pids_(ExoName, Node) of
        [Pid] -> Pid;
        [] -> throw({no_pid, ExoName, Node})
    end.
pid(ExoName) ->
    %% Always try local first
    case pids_(ExoName, 
               fun children/0, 
               fun() -> whereis(ExoName) end) of
        [Pid] -> Pid;
        _ ->
            case pid_maybe(ExoName, nodes()) of
                {ok, Pid} -> Pid;
                error -> throw({no_pid,ExoName})
            end
    end.

pids(ExoName) ->
    lists:flatten(
      maps:values(
        tools:pmap(
          fun(Node) -> pids_(ExoName, Node) end,
          exo_nodes()))).
pids_(ExoName, Node) ->
    ChildMap = fun() -> rpc_call(Node,exo,children,[]) end,
    Whereis  = fun() -> rpc_call(Node,erlang,whereis,[ExoName]) end,
    pids_(ExoName, ChildMap, Whereis).
pids_(ExoName, ChildMap, Whereis) ->
    case maps:find(ExoName, ChildMap()) of
        {ok, Pid} -> [Pid];
        _ when is_atom(ExoName) ->
            %% special case for now.  FIXME: don't rely
            %% on registered processes.
            case Whereis() of
                Pid when is_pid(Pid) -> [Pid];
                _ -> []
            end;
        _ -> []
    end.

%% Find a representative.  Note that this no longer cares about
%% duplicates: it just picks the first one it finds.  pids/1 is too
%% slow because it queries every node.
pid_maybe(ExoName, Nodes) ->
    tools:race(
      Nodes,
      2000,
      fun(Node) ->
              case pids_(ExoName, Node) of
             [Pid|_] -> {ok, Pid};
             _ -> error
         end
      end).
pid_maybe(ExoName) ->    
    try {ok, pid(ExoName)}
    catch {no_pid,_} -> error end.



rpc_call(Node,M,F,A) ->
    case rpc:call(Node,M,F,A) of
        {badrpc,_}=Err ->
            throw(Err);
        Rv ->
            Rv
    end.

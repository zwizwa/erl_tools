%% Generic DAG compilation routines for interop with epid_app.
-module(epid_dag).
-export([%% A. GATHER
         handle_epid_app/2,
         handle_epid_kill/2,
         outputs/2,
         %% B. PROCESS
         connect_inputs/1,
         internalize/2,
         subgraphs/1
        ]).

%%%%%% A. GATHER DAG

%% Handlers to implement some of the bookkeeping for generating a
%% dataflow graph, keeping state in the proxy object.  See
%% lab_board.erl
handle_epid_app({Caller, {epid_app, OpType, InputPids}}, State) ->
    {InputPids1, State1, Tmp} = compile_inputs(self(), InputPids, State),
    {Epid, State2} = compile_app(OpType, InputPids1, State1),
    obj:reply(Caller, #{ out => Epid, tmp => Tmp }),
    State2.
handle_epid_kill({Caller, _Msg = {epid_kill, {epid, _, Node}}}, State = #{epid_env := Env}) ->
    %% log:info("~p~n", [_Msg]),
    obj:reply(Caller, ok),
    %% Delete from dispatcher
    State1 = maps:remove({epid_dispatch,Node}, State),
    %% Delete from environment (DAG)
    Env1 = maps:remove(Node, Env),
    maps:put(epid_env, Env1, State1).

compile_app(OpType, InputPids, State) ->
    Env = maps:get(epid_env, State, #{}),
    Node = maps:get(epid_count, State, 0),
    Epid = {epid, self(), Node},
    {Epid,
     maps:merge(
       State,
       #{ epid_count => Node+1,
          epid_env => maps:put(Node, {OpType, InputPids}, Env)
        })}.


%% Before commit is called, connections have been made.
%% So we can just collect everything here.
outputs(Procs, State) ->
    Outputs = 
        lists:foldl(
          fun(_Binding = {Node, {_Proc, _Args}}, Os) ->
                  %% log:info("~p~n",[_Binding]),
                  case maps:find({epid_dispatch, Node}, State) of
                      {ok, _Epid} -> [Node|Os];
                      error -> Os
                  end
          end, [], Procs),
    Outputs.


%% 'traverse' the inputs, and create a buffer for each non-local signal
compile_inputs(Self, InputPids, State) ->
    maps:fold(
      fun(Var,Epid,{I,S,Tmp}) ->
              case Epid of
                  {epid, Self, _} ->
                      %% Input is already local.
                      I1 = maps:put(Var, Epid, I),
                      {I1,S,Tmp};
                  _ ->
                      %% Replace Var with a buffered version
                      {Epid1,S1} = compile_app(input, #{in => Epid}, S),
                      I1 = maps:put(Var, Epid1, I),
                      {I1,S1,[Epid1|Tmp]}
              end
      end, {InputPids, State, []}, InputPids).
    



%%%%%% B. PROCESS DAG

%% This gathers some generic code from epid_cproc.  Patterns seem to
%% come back, so this is likely reusable.

env_to_seq(M) ->
    [{K,maps:get(K,M)} || K <- lists:sort(maps:keys(M))].

%% Separate internal and external references, as they are handled
%% differently.  The resulting representation can be used to generate
%% the C code and the Erlang dispatcher.
internalize(LocalPid, Env) ->

    {Im, Nm} =
        maps:fold(
          fun(N, {Proc, Inputs}, {Is, Ns}) ->
                  case Proc of
                      input ->
                          %% External epids
                          InEpid = maps:get(in, Inputs),
                          {maps:put(N,InEpid, Is),
                           Ns};
                      _ ->
                          %% All processor inputs are "simple",
                          %% e.g. internal nodes.
                          InNodes =
                              maps:map(
                                fun(_InName, InEpid) ->
                                        case InEpid of
                                            {epid, LocalPid, InN} -> InN
                                        end
                                end,
                                Inputs),
                          {Is,
                           maps:put(N, {Proc, InNodes}, Ns)}
                  end
          end,
          {#{},#{}},
          Env),
    Is = env_to_seq(Im),
    Ns = env_to_seq(Nm),
    #{ inputs => Is, procs => Ns}.

%% When only part of the input changes, we can just execute a part of
%% the graph.  We can encode this by guarding each update clause with
%% an input mask.  This function creates that mask as a list of input
%% indices for each node.
%%
%% Compute input dependencies for each node by computing transitive
%% closure of all dependencies for each node, then filtering out only
%% the input nodes using indexed coordinates.  
%%
subgraphs(_DAG = #{ inputs := Inputs, procs := Procs }) ->
    %% Map Node name to Input index, which is what is used for the
    %% subgraph mask.
    InputMap = maps:from_list(
                 [{N,I} || {I,{N,_Epid}} <- tools:enumerate(Inputs)]),
    log:info("InputMap=~n~p~n", [InputMap]),
    %% log:info("DAG=~n~p~n", [_DAG]),

    %% Crate a map from a node to its parent node set.
    NodeToDeps =
        lists:foldl(
          fun({OutNode,{_Proc,InNodes}},Deps) ->
                  %% All dependencies of this node
                  OutNodeDeps =
                      maps:fold(
                        fun(_InName, InNode, D) ->
                                %% Record the node, and its transitive closure.
                                D1 = maps:put(InNode, true, D),
                                maps:merge(D1, maps:get(InNode, Deps, #{}))
                        end, #{}, InNodes),
                  maps:put(OutNode, OutNodeDeps, Deps)
          end, #{}, Procs),
    log:info("NodeToDeps=~n~p~n", [NodeToDeps]),

    %% Filter out only the indexed inputs.  That's all we need to
    %% "switch on" a subgraph.
    SubGraphs =
        maps:map(
          fun(_Node, Deps) ->
                  lists:foldl(
                    fun({N,true}, Idxs) ->
                            case maps:find(N,InputMap) of
                                {ok, Idx} -> [Idx|Idxs];
                                error -> Idxs
                            end
                    end,[],maps:to_list(Deps))
          end,
          NodeToDeps),

    log:info("SubGraphs=~n~p~n", [SubGraphs]),
    SubGraphs.


%% Generated code knows the input index mapping, so we can use
%% just that to make the connections.
connect_inputs(Inputs) ->
    lists:foreach(
      fun({Index,{_Node,Epid}}) ->
              epid:connect(Epid, {epid, self(), Index})
      end, tools:enumerate(Inputs)).


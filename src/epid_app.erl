

%% This module allows specification of dataflow connections in single
%% assigment "applicative" form, on top of the epid mechanism.

%% This is an integration module that brings together:
%%
%% - The epid protocol, which allows fine-grained naming of external
%%   processes that support message send and receive through Erlang
%%   proxy processes.
%%
%% - A pub-sub "connect" model on top of epid messages, that allows
%%   definition of epid sources and sinks and connections between
%%   them.
%%
%% - A stateful processor abstraction epid_app, that abstracts
%%   "applicative" data flow processors, abstracted as epids.
%%
%% - This module, implementing a higher order abstract syntax dataflow
%%   description language that generates instantiation and connection
%%   sequencing.  E.g. it allows description of connectivity, and
%%   performs instantiation of stateful processes via parameterized
%%   epid instantiator.
%%


-module(epid_app).
-export([op/2, instantiate/2, handle/2]).


%% Operators are just type names.  Those can be arbitrary data
%% structures, as long as bind/3 knows how to instantiate them.
op(OpType, Args) ->
    dsl:op(OpType, Args).

%% See exo_patch.erl for an example Spec.
%%
%% We use dsl.erl in conjuction with a bind/3 that performs eager
%% instaniation.  I.e. no intermediate data structure is generated (as
%% e.g. in the example in dsl.erl)
%%
%% MakeEpid in exo_patch is memoized, i.e. it will re-use an existing
%% processor if the input pids are the same.  This makes incremental
%% network updates possible.

bind(Op, Args, State = #{ make_epid := MakeEpid, refs := Refs }) ->
    %% Intermediate nodes produced by previous bind are already in
    %% normal {epid,_,_} form, but we need to make sure that named
    %% input nodes are also flattened.
    InputEpids = maps:map(fun(_, Name) -> MakeEpid(Name) end, Args),
    OutputEpid = MakeEpid({epid_app, {Op, InputEpids}}),
    State1 = maps:put(refs, maps:put(OutputEpid, true, Refs), State),
    {OutputEpid, State1}.

instantiate(MakeEpid, Spec) ->
    Config = #{ bind => fun bind/3, make_epid => MakeEpid, refs => #{} },
    State = dsl:eval(Config, Spec, []),
    {ok, #{value := FlatSpec}} = State,
    %% The convention is that Spec will evaluate to a map of output
    %% bindings, which can then be connected to the generated nodes:
    lists:foreach(
      fun({Dst, Src}) -> epid:connect(Src,  MakeEpid(Dst)) end,
      maps:to_list(FlatSpec)),
    State.




%% The handler is experimental.  This is implements bookkeeping to
%% keep track of a DAG in the proxy.
handle({Caller, {epid_app, OpType, InputPids}}, State) ->
    {InputPids1, State1, Tmp} = compile_inputs(self(), InputPids, State),
    {Epid, State2} = compile(OpType, InputPids1, State1),
    obj:reply(Caller, #{ out => Epid, tmp => Tmp }),
    State2;
handle({Caller, _Msg = {epid_kill, {epid, _, Node}}}, State = #{epid_env := Env}) ->
    %% log:info("~p~n", [_Msg]),
    obj:reply(Caller, ok),
    %% Delete from dispatcher
    State1 = maps:remove({epid_dispatch,Node}, State),
    %% Delete from environment (DAG)
    Env1 = maps:remove(Node, Env),
    maps:put(epid_env, Env1, State1);
handle({Caller, {epid_compile, Cmd}}, State = #{ epid_env := Env }) ->
    obj:reply(Caller, ok),
    case Cmd of
        clear ->
            State;
        commit ->
            %% The dag representation can be reduced by splitting
            %% inputs and internal nodes.
            DAG = epid_cproc:compile(self(), Env),

            %% Compute the "evented" subgraphs, encoded as a map from
            %% node number to indexed input, to be used in clause
            %% gating.
            Subgraphs = epid_cproc:subgraphs(DAG),

            %% The DAG representation gets mapped to two things: input
            %% buffer mapping and C code.

            %% C code knows the input index mapping, so we can use
            %% just that to make the connections.
            #{ inputs := Inputs, procs := Procs } = DAG,
            lists:foreach(
              fun({Index,{_Node,Epid}}) ->
                      epid:connect(Epid, {epid, self(), Index})
              end, tools:enumerate(Inputs)),

            %% Before commit is called, connections have been made.
            %% So we can just collect everything here.
            Outputs = 
                lists:foldl(
                  fun(_Binding = {Node, {_Proc, _Args}}, Os) ->
                          %% log:info("~p~n",[_Binding]),
                          case maps:find({epid_dispatch, Node}, State) of
                              {ok, _Epid} -> [Node|Os];
                              error -> Os
                          end
                  end, [], Procs),

            %% log:info("DAG:~n~p~nCode:~n~s", [DAG, Code]),
            Code = epid_cproc:code(DAG, Outputs, Subgraphs),

            %% FIXME: It's not necessary to keep these intermedates.
            %% Just pass them as values.
            maps:merge(
              State,
              #{ code => Code,
                 dag  => DAG })
    end.

compile(OpType, InputPids, State) ->
    Env = maps:get(epid_env, State, #{}),
    Node = maps:get(epid_count, State, 0),
    Epid = {epid, self(), Node},
    {Epid,
     maps:merge(
       State,
       #{ epid_count => Node+1,
          epid_env => maps:put(Node, {OpType, InputPids}, Env)
        })}.

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
                      {Epid1,S1} = compile(input, #{in => Epid}, S),
                      I1 = maps:put(Var, Epid1, I),
                      {I1,S1,[Epid1|Tmp]}
              end
      end, {InputPids, State, []}, InputPids).
    



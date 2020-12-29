

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
handle({Caller, {epid_kill, {epid, _, Node}}}, State = #{epid_env := Env}) ->
    obj:reply(Caller, ok),
    Env1 = maps:remove(Node, Env),
    maps:put(epid_env, Env1, State);
handle({epid_compile, Cmd}=_Tag, State = #{ epid_env := Env }) ->
    case Cmd of
        clear ->
            State;
        commit ->
            %% The dag representation can be reduced by splitting
            %% inputs and internal nodes.
            Reduced = epid_cproc:compile(self(), Env),
            log:info("reduced to internal rep:~n~p~n" ,[Reduced]),
            State
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
                      I1 = maps:put(Var, Epid),
                      {I1,S,Tmp};
                  _ ->
                      %% Replace Var with a buffered version
                      {Epid1,S1} = compile(input, #{in => Epid}, S),
                      I1 = maps:put(Var, Epid1, I),
                      {I1,S1,[Epid1|Tmp]}
              end
      end, {InputPids, State, []}, InputPids).
    


%% To compile to C what comes out of this:
%% bp2: {12,{input,#{in => {epid,<22052.32472.1>,{filter,{port,in,4},{cc,0,14}}}}}}
%% bp2: {13,{count,#{in => {epid,<0.24700.266>,12}}}}
%% Split nodes into input and other
%% Count inputs, then do something like
%% uint32_t input[1]; // total number of inputs
%% uint32_t input12 = input[0]; // for each input
%% Then generate the rest of the code using postfixed names.
%% Or just do the transformation before generating C.



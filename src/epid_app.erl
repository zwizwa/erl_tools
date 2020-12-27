

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
-export([op/2, instantiate/2]).


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
%% dsl:compile_dataflow basically only keeps track of instance
%% numbers, starting with N=0 per unique OpType.

bind({_OpType, _N}=InstanceId,
     Args,
     State = #{ make_epid := MakeEpid }) ->
    %% Intermediate nodes produced by previous bind are already in
    %% normal {epid,_,_} form, but we need to make sure that named
    %% input nodes are also flattened.
    InputEpids = maps:map(fun(_, Name) -> MakeEpid(Name) end, Args),
    OutputEpid = MakeEpid({epid_app, InstanceId, InputEpids}),
    {OutputEpid, State}.

instantiate(MakeEpid, Spec) ->
    Config = #{ bind_dfl => fun bind/3, make_epid => MakeEpid },
    {ok, #{value := FlatSpec}} = dsl:compile_dataflow(Spec, [], Config),
    %% The convention is that Spec will evaluate to a map of output
    %% bindings, which can then be connected to the generated nodes:
    lists:foreach(
      fun({Dst, Src}) -> epid:connect(Src,  MakeEpid(Dst)) end,
      maps:to_list(FlatSpec)),
    FlatSpec.





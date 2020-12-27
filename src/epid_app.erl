

%% This module allows specification of dataflow connections in single
%% assigment "applicative" form, on top of the epid mechanism.


%% This is an integration module that brings together a lot of other
%% functionality:
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
%% - This higher order abstract syntax dataflow description language
%%   that generates instantiation and connection sequencing.  E.g. it
%%   allows description of connectivity, and performs instantiation of
%%   stateful processes via parameterized epid instantiator.
%%


-module(epid_app).
-export([op/2, instantiate/2]).


%% Operators are just type names that are passed to the epid
%% instantiator.  All dispatch happens in instantiate_op/2.
op(TypeName, Args) ->
    dsl:op(TypeName, Args).

%% See exo_patch.erl for an example Spec.
%%
%% To instantiate, convert the Spec to a concrete ANF datatype,
%% instantiate each binding, and connect output nodes.

instantiate(MakeEpid, Spec) ->
    {FlatSpec, Intermediates} =
        dsl:compile_dataflow(Spec, []),
    Instances =
        lists:foldl(
          fun(Binding, Instances) ->
                  instantiate_binding(MakeEpid, Binding, Instances)
          end,
          #{}, Intermediates),
    lists:foreach(
      fun({Dst, Node}) ->
              Src = ref(Node, Instances), 
              epid:connect(Src,  MakeEpid(Dst))
      end,
      maps:to_list(FlatSpec)),
    {Instances, FlatSpec}.


%% Instantiation of bindings happens in two steps: convert {node,_}
%% references to previously instantiated epids, perform any remaining
%% epid name -> epid conversion, and use the concrete epids to
%% instantiate the processor.  All name resolution is abstracted in
%% MakeEpid.

instantiate_binding(MakeEpid,
                    {{node,InstanceId}=R,{op,{_TypeName, InputSpec}}},
                    Instances) ->
    InputEpids =
        maps:map(
          fun(_InputName, Ref) ->
                  MakeEpid(ref(Ref, Instances)) end,
          InputSpec),
    %% Operator is instantiated in unbound state.
    OutputEpid = MakeEpid({epid_op, InstanceId}),
    epid:connect_proc(InputEpids, OutputEpid),
    maps:put(R, OutputEpid, Instances).

ref({node,_}=R, Instances) -> maps:get(R, Instances);
ref(Epid,    _)            -> Epid.




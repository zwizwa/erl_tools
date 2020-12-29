

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
    Env = maps:get(epid_env, State, #{}),
    Node = maps:get(epid_count, State, 0),
    Epid = {epid, self(), Node},
    obj:reply(Caller, Epid),
    maps:merge(
      State,
      #{ epid_count => Node+1,
         epid_env => maps:put(Node, {OpType, InputPids}, Env)
       });
handle({Caller, {epid_kill, {epid, _, Node}}}, State = #{epid_env := Env}) ->
    obj:reply(Caller, ok),
    Env1 = maps:remove(Node, Env),
    maps:put(epid_env, Env1, State);
handle({epid_compile, Cmd}=_Tag, State = #{ epid_env := Env }) ->
    case Cmd of
        clear ->
            State;
        commit ->
            Nodes = lists:sort(maps:keys(Env)),
            lists:foreach(
              fun(Node) ->
                      Binding = maps:get(Node, Env),
                      log:info("~999p~n", [{Node, Binding}])
              end,
              Nodes),
            State
    end.



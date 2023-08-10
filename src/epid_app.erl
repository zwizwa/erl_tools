%% TL;DR
%%
%% - The epid nodes created by epid application need to live
%%   somewhere.  This module implements a daemon that contains those.
%%
%% - Creating nodes happens through bind/3, which is used by
%%   dsl:eval/2 during evaluation of DSL syntax (abstracted as Erlang
%%   function).



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
%% - Higher order syntax implemented by dsl.erl which bridges DSL
%%   programs (just syntax encoded as erlang functions, without
%%   semantics) with an evaluator / compiler process that communicates
%%   using obj:call/2 RPC.
%%
%% - This moduel, which implements an evaluator/compiler for the DSL,
%%   generating instantiation and connection sequencing.  E.g. it
%%   allows description of connectivity, and performs instantiation of
%%   stateful processes via parameterized epid instantiator.
%%


-module(epid_app).
-export([op/2, instantiate/2, update/3,
         start_link/1, handle/2,  mixin/3]).


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

instantiate(MakeEpid, DSLFun) ->
    Config = #{ bind => fun bind/3, make_epid => MakeEpid, refs => #{} },
    State = dsl:eval(Config, DSLFun, []),
    {ok, #{value := FlatSpec}} = State,
    %% The convention is that Spec will evaluate to a map of output
    %% bindings, which can then be connected to the generated nodes:
    lists:foreach(
      fun({Dst, Src}) ->
              ES = MakeEpid(Src),
              ED = MakeEpid(Dst),
              epid:connect(ES, ED)
      end,
      maps:to_list(FlatSpec)),
    State.






%% Patcher core.

%% Generic code. See exo_patch for a parameterized version.

%% What we do here is mostly retaining an instance dictionary, and
%% implementing the naming scheme for processors.  The dictionary is
%% implemented as a process.

start_link(InitMemo) when is_map(InitMemo) ->
    {ok,
     serv:start(
       {handler,
        fun() -> InitMemo end,
        fun ?MODULE:handle/2})}.


handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);

handle({Caller, {all_proxies, Msg}}, State) ->
    Timeout = 3000, %% FIXME
    Rv = tools:pmap(
           fun(Proxy) ->
                   %% log:info("Proxy=~p~n",[Proxy]),
                   try obj:call(Proxy, Msg, Timeout)
                   catch C:E ->
                           Error = {dead_proxy,Proxy,{C,E},State},
                           throw(Error)
                           %%log:info("WARNING: ~p~n", [Error]),
                           %%Error
                   end

           end,
           proxies(State)),
    obj:reply(Caller, Rv),
    State;

handle({Caller, {all_epids, Msg}}, State) ->
    lists:foreach(
      fun({_Ref,Epid}) -> epid:send(Epid, Msg) end,
      maps:to_list(State)),
    obj:reply(Caller, ok),
    State;

%% Implement epid_app instantiation here.  Instances are reused when
%% possible.  Note that processing is pure, in the sense that the
%% processor type and the concrete input epids fully determine the
%% output epid, so it can go into a memoization table.
handle({Caller, {need, {epid_app, {Proc, _InputEpids}=Tag}}}, State) ->
    {Epid1, State1} =
        case maps:find(Tag, State) of
            {ok, #{ out := Epid }} ->
                %% Don't log non-changes.
                %% log:info("old: ~p~n", [Epid]),
                {Epid, State};
            error ->
                %% Concrete types.  It seems simplest to implement the
                %% API at the module level.
                case Proc of
                    {Module, _} ->
                        module_epid_app(Module, Tag, State);
                    %% We could also implement a generic processor,
                    %% and then dispatch on the type of the inputs to
                    %% determine what variant to instantiate.  This
                    %% allows us to eliminate communication, by moving
                    %% processing closer to data sources.
                    _AbstractType ->
                        throw('FIXME_specialize')
                end
        end,
    obj:reply(Caller, Epid1),
    State1;

handle({Caller, {collect_garbage, Refs}}, State) ->
    obj:reply(Caller, ok),
    maps:filter(
      fun(_Tag, #{ out := Epid, kill := Kill, tmp := TmpEpidList }) ->
              case maps:get(Epid, Refs, false) of
                  true ->
                      true;
                  false ->
                      lists:foreach(
                        %% Needs to be synchronous, but if process is
                        %% no longer there we can just ignore the
                        %% error.
                        fun(E) -> catch Kill(E) end,
                        [Epid|TmpEpidList]),
                      log:info("del: ~p~n", [Epid]),
                      false
              end
      end,
      State);

%% {'DOWN',#Ref<0.0.2097153.215859>,process,<0.5245.183>,killed}

%% FIXME: Replace this with a try-catch block that catches match
%% errors and translates them into Handled=false signalling.
handle(_, _) ->
    error.



%% Mixin wrapper for handle/2.  Note that the mixin does treat every
%% State map entry as a node.  (FIXME This is a refactoring leftover.
%% Might not be needed as currently handle is self-contained.. ).

mixin(Handled, Msg, State) ->
    case Handled of
        true ->
            {false, State};
        false ->
            case handle(Msg, State) of
                error  -> {false, State};
                State1 -> {true, State1}
            end
    end.



proxies(State) ->
    maps:keys(
      lists:foldl(
        fun({_Ref,#{ out := {epid,Proxy,_}}}, P) -> maps:put(Proxy, true, P) end,
        #{}, maps:to_list(State))).


%% FIXME: Currently it's not possible to instantiate based on input
%% types.  We would like to be able to do that, e.g. instantiate as
%% epid_proc, or as processor inside a different host.

module_epid_app(Module,{Name,InputEpids}=Tag,State) ->
    %% Module:epid_app produces a new node, and a possibly empty list
    %% of temporaries.  We need to keep track of temporaries here so
    %% we can issue Module:epid_kill/1.  E.g. proxies do not to do any
    %% resource management: they can just set things up and tear
    %% things down on request.
    #{ out := Epid, tmp := TmpEpidList } = 
        Module:epid_app(Name, InputEpids),
    log:info("new: ~p~n", [Epid]),
    {Epid,
     maps:put(
       Tag,
       #{ out => Epid,
          tmp => TmpEpidList,
          kill => fun Module:epid_kill/1 },
       State)}.


%% Main user API entry point.  Given the Dag abstract syntax, the
%% "node container" service, and the epid name resolver / factory
%% (which e.g. is bound to redo Eval in exo), instantiate the Dag.

%% See do_patch.erl for example

update(ServerPid, MakeEpid, Dag) -> Timeout = 3000, %% FIXME

    %% Instantiate or re-use nodes,
    {ok, #{ state := #{refs := Refs }}} = instantiate(MakeEpid, Dag),

    %% Perform garbage collection for all unused nodes.
    obj:call(ServerPid, {collect_garbage, Refs}, Timeout),

    %% Send a finalizer message in case a particular proxy performs
    %% some kind of compilation step.

    %% FIXME: union proxy before and after!

    Rv = obj:call(ServerPid, {all_proxies, {epid_compile, commit}}, Timeout),
    %% log:info("all_proxies epid_compile:~n~p~n",[Rv]),
    %% log:info("exo_patch memo table:~n~p~n",[call(dump)]),
    Rv.
    

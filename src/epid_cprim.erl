%% Alternative to epid_cproc: this instantiates C at a much lower
%% level.  Currently not connected to any substrate.

%% This can serve as a minimal isolated exaple of DAG gather +
%% compile.

-module(epid_cprim).
-export([epid_app/2, handle_epid_compile/2, start_link/1, handle/2]).


%% FIXME: This is still old code generator.

epid_cprim() ->
    exo:need(epid_cprim).

epid_app(OpType, InputEpids) ->
    Pid = epid_cprim(),
    obj:call(Pid, {epid_app, OpType, InputEpids}).

start_link(Init) when is_map(Init) ->
    {ok,
     serv:start(
       {handler,
        fun() -> maps:merge(#{ epid_env => #{} }, Init) end,
        fun ?MODULE:handle/2})}.

handle({epid_send,Src,{epid_subscribe,Dst}}=_Msg,   State) -> epid:subscribe(Src, Dst, State);
handle({epid_send,Src,{epid_unsubscribe,Dst}}=_Msg, State) -> epid:unsubscribe(Src, Dst, State);

handle(Msg={_, {epid_app, _, _}},  State) -> epid_dag:handle_epid_app(Msg, State);
handle(Msg={_, {epid_kill, _}},    State) -> epid_dag:handle_epid_kill(Msg, State);
handle(Msg={_, {epid_compile, _}}, State) ->
    State1 = epid_cprim:handle_epid_compile(Msg, State),
    log:info("~n~p~n",[State]),
    State1;

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).
    
handle_epid_compile({Caller, {epid_compile, Cmd}}, State = #{ epid_env := Env }) ->
    obj:reply(Caller, ok),
    case Cmd of
        clear ->
            State;
        commit ->
            %% The dag representation can be reduced by splitting
            %% inputs and internal nodes.
            DAG = epid_dag:internalize(self(), Env),

            %% Compute the "evented" subgraphs, encoded as a map from
            %% node number to indexed input, to be used in clause
            %% gating.
            Subgraphs = epid_dag:subgraphs(DAG),

            %% The DAG representation gets mapped to two things: input
            %% buffer mapping and C code.
            #{ inputs := Inputs, procs := Procs } = DAG,

            %% C code knows the input index mapping, so we can use
            %% just that to make the connections.
            epid_dag:connect_inputs(Inputs),

            %% C code needs to send out messages for internal output nodes.
            Outputs = epid_dag:outputs(Procs, State),


            %% FIXME: It's not necessary to keep these intermedates.
            %% Just pass them as values.
            maps:merge(
              State,
              #{ outputs => Outputs,
                 subgraphs => Subgraphs,
                 dag  => DAG })
    end.

%% Stateful multi-in, single-out event processors.

-module(epid_proc).
-export([start_link/1, handle/2, epid/1,
         %% The rest are all machine update routines
         count/3
]).


%% TODO: Generatlize this, by defining an epid protocol that allows
%% the creation of other machine instances.  E.g. jack, pd, ...


%% Start a service that hosts a simple epid state machine, and fits in
%% the dataflow "applicative" processor framework, e.g. the processor
%% is identified by its output.

%% See also: exo_epid.erl, exo_dataflow.erl, exo.erl
%%
%% In the "applicative" view, a state machine's output is the main
%% commodity. I.e. it is the thing that behaves as a value.
%%
%% I find this hard to articulate, but it is an important insight!
%%
%% So, to name such a thing, we need:
%%
%% - a Tag to identify the instance (state).  We use this to store a
%%   process some registry or supervisor tree.
%%
%% - a Type to identify the constructor.  If an instance is not up,
%%   this is what is used to instantiate it.
%%
%% - a collection of inputs
%%


%% Note that applicative processors necessarily have only one output!
%% Some other protocol will need to be devised to push unpack into the
%% machine, providing multiple outputs.

-define(PROC_TAG,epid_proc).

%% This should dispatch based on Type.
%% Note that _Tag only needs to be unique wrt Type.
epid({epid_proc, _Tag, _Type}=Spec) ->
    {ok, Pid} = start_link(#{spec => Spec}),
    {epid, Pid, ?PROC_TAG}.

start_link(InitState = #{ spec := Spec}) ->
    {epid_proc, _Tag, Type} = Spec,
    log:info("start_epid_machine ~p~n", [Type]),
    {ok,
     serv:start(
       {handler,
        fun() -> InitState end,
        fun ?MODULE:handle/2})}.
handle(Msg, State = #{spec := Spec}) ->
    {epid_proc, _Tag, Type} = Spec,
    {Module, Function} =
        case Type of
            {M,F} when is_atom(M) and is_atom(F) -> {M, F};
            F when is_atom(F) -> {?MODULE, F}
        end,
    case Msg of
        {_, dump} ->
            obj:handle(Msg, State);
        {epid_send, ?PROC_TAG, {epid_subscribe, Dst}} ->
            epid:subscribe(?PROC_TAG, Dst, State);
        {epid_send, ?PROC_TAG, {epid_unsubscribe, Dst}} ->
            epid:unsubscribe(?PROC_TAG, Dst, State);
        %% Note that all processors that support the applicative model
        %% can reference the input nodes by composing the output tag
        %% name with the input tag.
        {epid_send, {?PROC_TAG, InputTag}, Value} ->
            {Outputs,State1} =
                Module:Function(InputTag,Value,State),
            lists:foreach(
              fun(OutputValue) ->
                      %% log:info("dispatch ~p~n", [OutputValue]),
                      epid:dispatch(?PROC_TAG, OutputValue, State1)
              end, Outputs),
            State1;
        _ ->
            log:info("~p: ~p~n", Spec, [Msg])
    end.



%% The whole point of this is to easily host event handlers that do
%% not need a lot of unpacking or other processing.  So make the
%% interface very simple.

%% API for the update function:
%% - input values are tagged
%% - single output value is listed (e.g. zero and multiple out events are possible)

count(InputTag, Value, State) ->
    N = maps:get(count, State, 0),
    log:info("~p <- ~p~n", [InputTag,Value]),
    {[N], maps:put(count, N+1, State)}.




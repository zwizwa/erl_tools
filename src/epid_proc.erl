%% Stateful multi-in, single-out event processors.

-module(epid_proc).
-export([start_link/1, handle/2, epid_app/2, epid_kill/1,
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

-define(PROC_OUTPUT_TAG,epid_proc_out).

%% This should dispatch based on Type.
%% Note that _Tag only needs to be unique wrt Type.
epid(Type) ->
    {ok, Pid} = start_link(#{type => Type}),
    {epid, Pid, ?PROC_OUTPUT_TAG}.

epid_app(Type, InputEpids) ->
    Epid = epid(Type),
    epid:connect_proc(InputEpids, Epid),
    #{ out => Epid, tmp => [] }.

epid_kill({epid,Pid,_}) ->
    unlink(Pid),
    exit(Pid, normal),
    ok.


start_link(InitState = #{ type := _Type }) ->
    %% log:info("epid_proc start ~p~n", [_Type]),
    {ok,
     serv:start(
       {handler,
        fun() -> InitState end,
        fun ?MODULE:handle/2})}.
handle(Msg, State = #{type := Type}) ->
    {Module, Function} =
        case Type of
            {M,F} when is_atom(M) and is_atom(F) -> {M, F};
            F when is_atom(F) -> {?MODULE, F}
        end,
    case Msg of
        {_, dump} ->
            obj:handle(Msg, State);
        {epid_send, ?PROC_OUTPUT_TAG, {epid_subscribe, Dst}} ->
            epid:subscribe(?PROC_OUTPUT_TAG, Dst, State);
        {epid_send, ?PROC_OUTPUT_TAG, {epid_unsubscribe, Dst}} ->
            epid:unsubscribe(?PROC_OUTPUT_TAG, Dst, State);
        {epid_send, ?PROC_OUTPUT_TAG, epid_unsubscribe_all} ->
            epid:unsubscribe_all(?PROC_OUTPUT_TAG, State);
        %% Note that all processors that support the applicative model
        %% can reference the input nodes by composing the output tag
        %% name with the input tag.
        {epid_send, {?PROC_OUTPUT_TAG, InputTag}, Value} ->
            {Outputs,State1} =
                Module:Function(InputTag,Value,State),
            lists:foreach(
              fun(OutputValue) ->
                      %% log:info("dispatch ~p~n", [OutputValue]),
                      epid:dispatch(?PROC_OUTPUT_TAG, OutputValue, State1)
              end, Outputs),
            State1;
        %% We don't need compilation.
        {epid_compile, _} ->
            State
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




-module(epid).
-export([send/2, connect/2,
         %% Machinery for aggregating proxy.
         register/2,
         unregister/2,
         dispatch/3]).

%% INTRODUCTION
%%
%% External Pid: process-like resources accessed through Erlang proxy
%% processes.
%%
%% This model works well for event sources and event sinks that have
%% very fine granulairty, but cannot be represented as actual Erlang
%% processes for practical reasons, e.g. generic sensors, actuators,
%% test equipment, midi input boxes, audio input/output and effect
%% processors etc..  Any configurable dataflow setup.
%%
%% Such processes are represented by {epid, Proxy, Id}, where Proxy is
%% the Erlang process responsible for bridging the Erlang world and
%% the external event/process world, and Id is an identifier for the
%% external resource that behaves as a process, but does not or cannot
%% use Erlang's native PID format.
%%
%% Contrasts this to:

%% - Pub/Sub: we're doing the reverse, making granularity finer,
%%   undoing classes of events and exposing a finer unit.
%%
%% - Erlang C nodes: avoids multi-component identifiers by mapping
%%   internal process resources to actual Erlang pids.

%% PROTOCOL
%%
%% Messages can be sent to an epid, just like a normal process.  The
%% ProxyPid will implement delivery.
%%
send({epid, ProxyPid, SinkId}, Msg) ->
    ProxyPid ! {epid_send, SinkId, Msg}.

%% The main driver for this abstraction is a standardized way to
%% connect event sources and sinks on edge devices that are managed by
%% an Erlang backbone.  To perform a connection, we interpret it as a
%% subscribe message sent to the epid of the event source, carrying
%% enough information such that events can be sent to the epid of a
%% sink.
connect(Source, Sink) ->    
    send(Source, {epid_subscribe, Sink}).

%% I've found that in typical setups there will be an "aggregator"
%% process that recevies all events from the external world, but the
%% event sources do not have any individual behavior other than just
%% producing events.  In this case the aggregator can use the
%% subscribe mechanism to filter events and send them to their
%% destination.  See midi_raw.erl for an example of this.




%% NOTES
%%
%% The general idea is that Erlang is a great model, but I found it's
%% process granularity still too cource in many practical situations.
%% I set out to be able to do the following:
%%
%% - Send a message to a knob on a Midi controller to "connect" it to
%%   a parameter in a soft synth, such that subsequent knob events are
%%   sent directly to that parameter slot.  This allows to create an
%%   archive of connections separate from the source and sink object
%%   code.
%%
%% - Erlang processes are too heavy-weight for this, so we use the
%%   same conceptual model, but reduce the granularity.  This requires
%%   two kinds of problems to be solved:
%%
%%   - Sinks are addressed relative to a proxy Pid, e.g. {epid, Pid,
%%     Id} will arrive at the proxy Pid first, and will then be
%%     propagated to a non-Erlang entity.
%%
%%   - Events typically go through a pub/sub aggregators when they
%%     enter the Erlang world.  E.g. event sources typically have no
%%     object identity: there are just events coming from _somewhere_.
%%     However, the aggregator can restore the object model by using
%%     event tags as object identifiers.
%%
%% - This also leaves room for optimization: When the source and sink
%%   proxy Pids are the same, often an internal connection mechanism
%%   is more appropriate.  This intelligence can be implemented
%%   locally ( e.g. jack midi/audio client connections, C callbacks
%%   inside a uC ).
%%
%% - This mechanism of routable process identifiers can be combined
%%   with a name resolution step that is performed only once.
%%   Typically name resolution would need to be combined with Erlang
%%   style monitors to allow re-resolving when a subsystem is
%%   restarted or physically moved.
%%
%% - The tradeoff to creating C nodes is not well understood.
%%   Currently going by the assumption that 2-step routing is much
%%   simpler to implement, and also easier to debug.





%% Examples: connect a midi input to a uC pin.
%% (Sorry, internal example only.  Ask Tom.)

%% Note that this is very straightforward.  The real trick is in
%% identifying the difference between names and routable addresses.
%% Name resolution is an essential part!

%% - uC pin:
%%   - Pid  = {relay_devrack,'exo@10.1.3.29'}
%%   - Sink = {relay, $B}
%%   This requires adding a case in relay_board.erl to translate the event.
%%   relay_solderstation ! {forward, {relay, $B}, 1}.
%%   epid:send({epid, {relay_solderstation,'exo@10.1.3.29'}, {relay,$B}}, 1).

%% - knob on the midi hub.  in midi_raw.erl it required handling
%%   epid_connect, to store the sink -> epid mapping, and then when
%%   device events come in, convert them to sink form and retreive
%%   epid.
%%   - Pid = {midi_raw,'exo@10.1.3.19'}
%%   - Src = {{zora,1},{cc,0,14}}
%%   to test:
%%   epid:connect({epid, {midi_raw,'exo@10.1.3.19'}, {{zora,1},{cc,0,14}}}, {epid, {relay_solderstation, 'exo@10.1.3.29'}, {relay, $B}}).
%%   epid:connect({epid, exo:pid(midi_raw), {{roza,1},{cc,0,14}}}, {epid, {relay_solderstation, 'exo@10.1.3.29'}, {relay, $B}}).



%% Some mechanics for implementing a registry.  See midi_hub.erl

%% There are 3 phases:
%% - register:   set up local state for dispatch and unregister
%% - dispatch:   look up epids and forward
%% - unregsiter: clean up dead connection


register({epid_send, EventId, {epid_subscribe, {epid, Pid, _}=Epid}}, State) ->

    %% Each event is forwarded to a list of epids.
    Epids0 = maps:get(EventId, State, []),
    Epids = [Epid|Epids0],
    
    %% For each registration, create a separate monitor that we can
    %% use to remove the epid from the list.
    Ref = erlang:monitor(process, Pid),

    %% Create maps in two directions to make lookup simpler.
    maps:merge(
      State,
      #{ Ref => {EventId, Epid},  %% For 'DOWN'
         EventId => Epids }).     %% For dispatch
    
unregister({'DOWN',Ref,process,_Pid,_Reason}=_Msg, State) ->
    %% log:info("epid:unregister:~p~n",[_Msg]),
    case maps:find(Ref, State) of
        {ok, {EventId, Epid}} ->
            Epids0 = maps:get(EventId, State),
            Epids = lists:delete(Epid, Epids0),
            {true,
             maps:put(
               EventId, Epids, 
               maps:remove(Ref, State))};
        error ->
            %% Returning false allows chaining, e.g. caller doesn't
            %% need to know if Ref corresponds to an epid or to some
            %% other monitor.
            {false, State}
    end.

dispatch(EventId, Msg, State) ->    
    case maps:find(EventId, State) of
        {ok, Epids} ->
            lists:foreach(
              fun(Epid) -> send(Epid, Msg) end,
              Epids);
        error ->
            ok
    end,
    State.
    
    


-module(epid).
-export([send/2, connect/2]).

%% Extended Pid: Fine-grained source and sink protocol, implemented as
%% Erlang proxy processes.

%% Where Pid is any erlang Pid substitute, and SinkId is a sink ID
%% local to the proxy object identified by the Pid.

%% This model generalizes neatly to generic sensors, actuators, test
%% equipment, midi input boxes, speakers, effect processors etc..




%% Erlang is a great model, but is too course in practice.  The driver
%% to solve this is to be able to do the following:
%%
%% - Send a message to a knob on a Midi controller "connect" it to a
%%   parameter in a soft synth, such that subsequent knob events are
%%   sent directly to that parameter slot.
%%
%% - Erlang processes are too heavy-weight for this, so what we do in
%%   this module is to use the same model, but reduce the granularity.
%%   This requires two kinds of problems to be solved:
%%
%%   - Sinks are accessed by routers, e.g. {Pid, SinkId} will arrive at a
%%     hub Pid first, and will then be propagated to a non-Erlang
%%     entity.
%%
%%   - Sources go through aggregators (pub/sub), and will cover the
%%     granularity at the other end, where it is often easier to
%%     forward all events of a submodule to then propagate at that
%%     end.
%%
%% - When the source and sink Pids are the same, often an internal
%%   connection mechanism is more appropriate.  This intelligence can
%%   be implemented locally (e.g. jack midi connactions, or even C
%%   pointers inside a uC).
%%
%% - This can be combined with a name resolution step such that
%%   routing information is always explicit.  E.g. actual Pids, and
%%   current device nodes.  This could even be extended with the idea
%%   of monitor for extended Pids.
%%   
%%
%%
%% Anything that benefits from very fine object granularity, while
%% keeping connections fairly generic.  Any dataflow setup.
%%


%% The basic idea is then that sending a {subscribe, Sink, Tag}
%% message to the Source address will result in {Tag, Data} messages
%% to arrive at the sink, where all ad-hoc routing is abstracted.


%% Send a message to a sink hubbed by the proces.
send({epid, Pid, SinkId}, Msg) ->
    Pid ! {epid_send_sink, SinkId, Msg}.

%% Ask Src to send its events to Dst, tagged with Tag.  Typically this
%% is implemented by the aggregator.
connect(Src, Dst) ->    
    send(Src, {epid_connect, Dst}).


%% Examples: connect a midi input to a uC pin.
%% (Sorry, internal example only.  Ask Tom.)

%% Note that this is very straightforward.  The real trick is in
%% identifying the difference between names and routable addresses.
%% Name resolution is an essential part!

%% - uC pin:
%%   - Pid = {relay_devrack,'exo@10.1.3.29'}
%%   - Uid = {relay, 'B'}
%%   This requires adding a case in relay_board.erl to translate the event.
%%   relay_solderstation ! {forward, {relay, $B}, 1}.
%%   epid:send({epid, {relay_solderstation,'exo@10.1.3.29'}, {relay,$B}}, 1).

%% - knob on the midi hub.  to test:
%%   epid:connect({epid, {midi_raw,'exo@10.1.3.20'}, {{roza,1},{cc,0,6}}}, {epid, {relay_solderstation, 'exo@10.1.3.29'}, {relay, $B}}).
%%   epid:connect({epid, {midi_raw,'exo@10.1.3.19'}, {{zora,1},{cc,0,14}}}, {epid, {relay_solderstation, 'exo@10.1.3.29'}, {relay, $B}}).


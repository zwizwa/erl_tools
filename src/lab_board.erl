-module(lab_board).
-export([init/2, handle/2, handle_from_port/2]).

%% companion to uc_tools/gdb/lab_board.c

%% This can run under gdbstub_hub.erl as well as standalone.
%% Communication uses SLIP wrapping.

%% For now, only gdbstub_hub type objects are supported.

-include("slip.hrl").

%% Application tags:
-define(TAG_SET_PIN, 16#0000).
-define(TAG_STATUS,  16#0001).



init(Pid, _PacketProto) ->
    Pid ! {set_forward, fun ?MODULE:handle/2},
    %%Info = obj:dump(Pid),
    %%log:info("lab_board: ~p~n",[Info]),
    ok.

handle(<<?TAG_STATUS:16,_Status/binary>>, State) ->
    %% log:info("lab_board: status: ~p~n", [_Status]),
    NbStatus = maps:get(nb_status, State, 0),
    %% Status update.
    maps:put(nb_status, NbStatus+1, State);

%% Print data to erlang console, like info messages.
%%handle(<<?TAG_UART:16,Data/binary>>, State) ->
%%    gdbstub_hub:decode_info(Msg, State);


%% These tags are bridged via socat.
handle(<<?TAG_UART:16, _/binary>>=Msg, State) ->
    handle_to_port(uart_port, {pty,"/tmp/uart"}, Msg, State);

handle(<<?TAG_PLUGIO:16, _/binary>>=Msg, State) ->
    %% log:info("TAG_PLUGIO: ~p~n", [Msg]),
    %% handle_to_pty(plugin_pty, {pty,"/tmp/plugin"}, Msg, State);
    handle_to_port(plugin_port, {tcp_listen,5555}, Msg, State);

%% Support the fine-grained sinks protocol.  See exo_connect.erl
handle({epid_send_sink,Sink,Msg}, State) ->
    case Sink of
        {relay, Relay} when is_number(Msg) ->
            %% Translate to relay protocol
            Self = self(),
            spawn(
              fun() -> 
                      true = lists:member(Relay, "ABCD"),
                      Off = Relay + $\a - $\A,
                      On  = Relay,
                      Code = case Msg of 0 -> Off; _ -> On end,
                      gdbstub_hub:call(Self, <<0,0,Code>>, 1000)
              end),
            ok;
        _ ->
            log:info("WARNING: message ~p for unkown Sink ~p~n", [Msg, Sink])
    end,
    State;

handle(Msg, State) ->
    log:info("lab_board: passing on: ~p~n",[Msg]),
    gdbstub_hub:default_handle_packet(Msg, State).


%% Handle traffic to and from port.  These are created on demand by
%% data coming from the uC.

handle_from_port({Port, {data,Data}}, State) ->
    {_,_,Tag} = maps:get({port_info,Port}, State),
    Packet = <<Tag:16, Data/binary>>,
    %% log:info("handle_from_port: ~p~n", [Packet]),
    self() ! {send_packet, Packet},
    State;
handle_from_port({Port, {exit_status,_Status}}, State) ->
    %% Re-open
    {PortName,Spec,Tag} = maps:get({port_info,Port}, State),
    {_Port, State1} = open_socat({PortName,Spec,Tag}, State),
    State1.

open_socat({PortName,Spec={SocatPortType,Arg},Tag}, State) ->
    log:info("open_socat ~p~n", [Spec]),
    P = socat:SocatPortType(Arg),
    {P, maps:merge(
          State,
          #{ PortName => P,
             {port_info, P} => {PortName,Spec,Tag},
             {handle, P} => fun ?MODULE:handle_from_port/2 })}.

handle_to_port(PortName, {_SocatPortType, _Arg}=Spec, <<Tag:16,Data/binary>>, State) ->
    {Port, State1} =
        case maps:find(PortName, State) of
            {ok, P} ->
                {P, State};
            _ ->
                open_socat({PortName,Spec,Tag}, State)
        end,                                
    Port ! {self(), {command, Data}},
    State1.

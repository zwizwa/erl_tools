-module(lab_board).
-export([init/2, handle/2, handle_from_port/2]).

%% companion to uc_tools/gdb/lab_board.c

%% This can run under gdbstub_hub.erl as well as standalone.
%% Communication uses SLIP wrapping.

%% For now, only gdbstub_hub type objects are supported.

%% The idea is to export certain functionality such that other board
%% drivers can use the handler components.

-include("slip.hrl").

%% Application tags:
-define(TAG_SET_PIN, 16#0000).
-define(TAG_STATUS,  16#0001).



init(Pid, _PacketProto) ->
    Pid ! {set_forward, fun ?MODULE:handle/2},
    %%Info = obj:dump(Pid),
    %%log:info("lab_board: ~p~n",[Info]),
    ok.

handle(<<4:4, _:4, _/binary>>=Msg, State) ->
    _ = Msg,
    %% log:info("ipv4: ~p bytes~n", [size(Msg)]),
    State;

handle(<<?TAG_STATUS:16,_Status/binary>>, State) ->
    %% log:info("lab_board: status: ~p~n", [_Status]),
    NbStatus = maps:get(nb_status, State, 0),
    %% Status update.
    maps:put(nb_status, NbStatus+1, State);

%% Print data to erlang console, like info messages.
%%handle(<<?TAG_UART:16,Data/binary>>, State) ->
%%    gdbstub_hub:decode_info(Msg, State);


%% These tags are bridged via ecat.
handle(<<?TAG_STREAM:16, StreamId:16, _/binary>>=Msg, State) ->
    case StreamId of
        0 -> handle_to_port(uart_port, {pty,"/tmp/uart"}, Msg, State)
    end;

handle(<<?TAG_PLUGIO:16, _/binary>>=Msg, State) ->
    %% log:info("TAG_PLUGIO: ~p~n", [Msg]),
    %% handle_to_pty(plugin_pty, {pty,"/tmp/plugin"}, Msg, State);
    handle_to_port(plugin_port, {tcp_listen,5555}, Msg, State);

handle(<<?TAG_U32:16, NbArgs:16, Bin/binary>>=_Msg, State) ->
    %% This needs to go to epid dispatch.
    %% log:info("TAG_U32 ~p\n", [_Msg]),
    BArgsLen = NbArgs*4,
    BArgs = binary:part(Bin, 0, BArgsLen),
    BTail = binary:part(Bin, BArgsLen, size(Bin)-BArgsLen),
    %% log:info("TAG_U32 ~p\n", [{NbArgs,BArgs}]),
    %% FIXME: Tail
    Args = [Arg || <<Arg:32>> <= BArgs],
    case Args of
        [Tag|Rest] ->
            epid:dispatch(Tag, Rest, State);
        _ ->
            ok
    end,
    %% log:info("TAG_U32 ~p\n", [{Args,BTail}]),
    State;

%% Support the fine-grained sinks protocol.  See exo_connect.erl
handle({epid_send,Src,{epid_subscribe,Dst}}=_Msg, State) ->
    epid:subscribe(Src, Dst, State);
handle({epid_send,Src,{epid_unsubscribe,Dst}}=_Msg, State) ->
    epid:unsubscribe(Src, Dst, State);

handle({epid_send,Sink,Msg}, State) ->
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
        midi ->
            %% FIXME: Jack interface produces a list of messages?
            lists:foreach(
              fun(Midi) ->
                      case Midi of
                          {cc,_,CC,Val} ->
                              self() ! {send_u32,[CC,Val]},
                              ok;
                          _ ->
                              log:info("ignore midi ~p~n", [Msg]),
                              ok
                      end
              end,
              Msg);
        _ ->
            log:info("WARNING: message ~p for unkown Sink ~p~n", [Msg, Sink])
    end,
    State;


%% Temperature and humidity data
handle(<<16#FFF30000:32, OK:8, RH:16, T:16, _/binary>>=_Msg, State = #{name := Name}) ->
    %% log:info("Msg=~p~n",[_Msg]),
    Parsed = {_Type, _RH1,_T1} = 
        if RH > 1000 -> {dht11, RH/256,T/256};
           true      -> {dht22, RH/10,T/10}
        end,
    %% log:info("OK=~p, RH=~p, T=~p~n",[OK,_RH1,_T1]),
    %% Use thermostat log
    lists:foreach(
      fun(Pid) -> Pid ! {notify_external, {dht11,Name,{OK,RH,T},Parsed}} end,
      exo:pids(thermostat)),
    State;

%% FIXME: It is probably not ok to make this this catch-all.
%% But it is very convenient to have the command interface be the default.
handle({Name, Args}, State) when is_atom(Name) and is_list(Args) ->
    %% Use TAG_U32 to access Forth console commands.
    %% This uses the first argument ==0 to dispatch on.
    Data = atom_to_binary(Name, utf8),
    self() ! {send_u32, Args, Data},
    State;
handle(Name, State) when is_atom(Name) ->
    handle({Name,[]}, State);

handle(Msg, State) ->
    %% log:info("lab_board: passing on: ~p~n",[Msg]),
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
    {_Port, State1} = open_ecat({PortName,Spec,Tag}, State),
    State1.

open_ecat({PortName,Spec={EcatPortType,Arg},Tag}, State) ->
    log:info("open_ecat ~p~n", [Spec]),
    P = ecat:EcatPortType(Arg),
    {P, maps:merge(
          State,
          #{ PortName => P,
             {port_info, P} => {PortName,Spec,Tag},
             {handle, P} => fun ?MODULE:handle_from_port/2 })}.

handle_to_port(PortName, {_EcatPortType, _Arg}=Spec, <<Tag:16,Data/binary>>, State) ->
    {Port, State1} =
        case maps:find(PortName, State) of
            {ok, P} ->
                {P, State};
            _ ->
                open_ecat({PortName,Spec,Tag}, State)
        end,                                
    Port ! {self(), {command, Data}},
    State1.

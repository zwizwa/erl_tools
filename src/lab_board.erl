-module(lab_board).
-export([init/2, handle/2, handle_from_pty/2]).

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
    handle_to_pty(uart_pty, {pty,"/tmp/uart"}, Msg, State);

handle(<<?TAG_PLUGIO:16, _/binary>>=Msg, State) ->
    handle_to_pty(plugin_pty, {pty,"/tmp/plugin"}, Msg, State);

handle(<<?TAG_FORTH:16, _/binary>>=Msg, State) ->
    handle_to_pty(forth_pty, {pty,"/tmp/forth"}, Msg, State);

handle(Msg, State) ->
    %% log:info("lab_board: passing on: ~p~n",[Msg]),
    gdbstub_hub:default_handle_packet(Msg, State).


%% Handle traffic to and from pty.  These are created on demand by
%% data coming from the uC.

handle_from_pty({Port, {data,Data}}, State) ->
    Tag = maps:get({tag,Port}, State),
    Packet = <<Tag:16, Data/binary>>,
    %% log:info("handle_pty: ~p~n", [Packet]),
    self() ! {send_packet, Packet},
    State.

handle_to_pty(PtyName, {SocatType,Link}, <<Tag:16,Data/binary>>, State) ->
    {Port, State1} =
        case maps:find(PtyName, State) of
            {ok, P} ->
                {P, State};
            _ ->
                log:info("open pty ~p~n", [Link]),
                P = socat:SocatType(Link),
                {P, maps:merge(
                      State,
                      #{ PtyName => P,
                         {tag, P} => Tag,
                         {handle, P} => fun ?MODULE:handle_from_pty/2 })}
        end,                                
    Port ! {self(), {command, Data}},
    State1.

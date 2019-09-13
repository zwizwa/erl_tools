-module(lab_board).
-export([init/2, handle/2]).

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

%%handle(<<?TAG_UART:16,Data/binary>>, State) ->
%%    log:info("lab_board: uart: ~p~n", [Data]),
%%    State;

%% FIXME: Connect this to an actual serial port (pty) via socat.
handle(<<?TAG_UART:16, Msg/binary>>, State) ->
    gdbstub_hub:decode_info(Msg, State);

handle(Msg, State) ->
    %% log:info("lab_board: passing on: ~p~n",[Msg]),
    gdbstub_hub:default_handle_packet(Msg, State).


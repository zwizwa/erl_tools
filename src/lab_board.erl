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

handle(<<?TAG_REPLY:16,Ack/binary>>, State) ->
    log:info("relay ack: ~p~n",[Ack]),
    State;

handle(<<?TAG_INFO:16,Log/binary>>, State) ->
    log:info("log: ~p~n", [Log]),
    State;

handle(<<?TAG_STATUS:16,_Status/binary>>, State) ->
    %% log:info("lab_board: status: ~p~n", [_Status]),
    NbStatus = maps:get(nb_status, State, 0),
    %% Status update.
    maps:put(nb_status, NbStatus+1, State);


handle(Msg, State) ->
    log:info("lab_board: unknown: ~p~n",[Msg]),
    State.

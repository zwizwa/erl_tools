-module(lab_board).
-export([init/2, handle/2]).

%% companion to uc_tools/gdb/lab_board.c

%% This can run under gdbstub_hub.erl as well as standalone.
%% Communication uses SLIP wrapping.

%% For now, only gdbstub_hub type objects are supported.

init(Pid, _PacketProto) ->
    Pid ! {set_forward, fun ?MODULE:handle/2},
    %%Info = obj:dump(Pid),
    %%log:info("lab_board: ~p~n",[Info]),
    ok.

handle(<<2,_/binary>>, State) ->
    NbStatus = maps:get(nb_status, State, 0),
    %% Status update.
    maps:put(nb_status, NbStatus+1, State);

handle(Msg, State) ->
    log:info("lab_board: unknown: ~p~n",[Msg]),
    State.

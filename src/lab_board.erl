-module(lab_board).
-export([init/2, handle/2, handle_pty/2]).

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

%
% FIXME: Connect this to an actual serial port (pty) via socat.
%% Use the name as the board as the link parameter.
%% Use a generic tag<->Erlang port mapper
%% FIXME: Standardize the key name and filesystem link so it can
%% also be used elsewhere.  Also, opening a TCP port might be
%% better?
handle(<<?TAG_UART:16, Data/binary>>, State) ->
    {Port, State1} =
        case maps:find(uart_pty, State) of
            {ok, P} ->
                {P, State};
            _ ->
                Link = "/tmp/uart",
                log:info("open pty ~p~n", [Link]),
                P = socat:pty(Link),
                {P, maps:merge(
                      State,
                      #{ uart_pty => P,
                         {handle, P} => fun ?MODULE:handle_pty/2 })}
        end,                                
    Port ! {self(), {command, Data}},
    State1;

handle(Msg, State) ->
    %% log:info("lab_board: passing on: ~p~n",[Msg]),
    gdbstub_hub:default_handle_packet(Msg, State).


%% See handle/2 ?TAG_UART clause
handle_pty({_Port, {data,Data}}, State) ->
    Packet = <<?TAG_UART:16, Data/binary>>,
    %% log:info("handle_pty: ~p~n", [Packet]),
    self() ! {send_packet, Packet},
    State.

%% FIXME: Just a stub.

%% Terminal emulator.

%% The original driver for this was to run ghcid and snarf some
%% output.  However that is not necessary, as ghcid can be told to
%% save raw or json output.

%% This is left here anyway as a continuation point for further
%% development.  Escape codes are a bit of a mess, but it would
%% probably be good to have some idea about how it works.

%% http://man7.org/linux/man-pages/man4/console_codes.4.html

-module(terminal).
-export([start_link/1, handle/2]).

start_link(#{ cmd := Cmd } = Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = open_port({spawn, Cmd}, [use_stdio, binary]),
                maps:put(port, Port, Config)
        end,
        fun ?MODULE:handle/2})};
start_link(_) ->
    start_link(#{ cmd => "/home/tom/exo/ghcid/ghcid.sh" }).


-define(ESC,27).
handle({Port,{data,Data}}=_Msg, #{ port := Port } = State) ->
    _Data1 = 
        case Data of
            %% <<?ESC,$],$0,$;,Rest/binary>> -> Rest;
            _ -> Data
        end,
    log:info("~p~n", [_Msg]),
    %% log:info("~s", [_Data1]),
    State;


handle({Port,_}=_Msg, #{ port := Port } = State) ->
    log:info("~p~n", [_Msg]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).




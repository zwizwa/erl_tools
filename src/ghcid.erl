-module(ghcid).
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


%% We only need to capture ^G (7) BELL to get a notification that
%% something happened, then poll the output file.
handle({Port,_}=_Msg, #{ port := Port } = State) ->
    log:info("~p~n", [_Msg]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).




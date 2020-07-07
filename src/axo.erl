-module(axo).
-export([start_link/1, handle/2]).

%% We are started by axo_hub.  It will provide the correct config to
%% start the axo_connect.elf binary driver.
start_link(Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = 
                    tools:spawn_port(
                      Config,
                      {"axo_connect.elf", []},
                      [use_stdio, binary, exit_status, {packet,4}]),
                State = maps:merge(
                          Config,
                          #{ port => Port }),
                State
        end,
        fun ?MODULE:handle/2})}.



handle({send, Bin}, #{ port := Port}=State) ->
    Port ! {self(), {command, Bin}},
    State;

handle({Port, _}=Msg, #{ port := Port}=State) ->
    log:info("~p~n",[Msg]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).



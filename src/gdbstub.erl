-module(gdbstub).
-export([start/1]).

%% GDB RSP Interface (gdbstub)
%%
%% Have GDB speak RSP to Erlang, allowing Erlang to speak a simplified
%% protocol to firmware.
%%
%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to gdbstub.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


start(Port) ->
    serv:start(
      {handler,
       fun() ->
               serv_tcp:init(
                 [Port],
                 %% Connect init + handler
                 {handler,
                  fun(Sock,_) -> #{sock => Sock} end,
                  fun handle/2},
                 %% Registry message handler & state.
                 fun(Msg,State) -> 
                         tools:info("gdbstub reg: ~p~n", [Msg]),
                         State
                 end, 
                 #{},
                 [binary, {active, true}, {reuseaddr, true}])
       end,
       fun serv_tcp:handle/2}).
                        
handle(Msg,State) ->
    tools:info("gdbstub: ~p~n", [Msg]),
    State.
    

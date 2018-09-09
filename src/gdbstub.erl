%% (c) 2018 Tom Schouten -- see LICENSE file

-module(gdbstub).
-export([start/1,handle/2]).

%% GDB RSP Interface (gdbstub)
%%
%% Have GDB speak RSP to Erlang, allowing Erlang to speak a simplified
%% protocol to firmware.
%%

start(Port) ->
    serv:start(
      {handler,
       fun() ->
               serv_tcp:init(
                 [Port],
                 %% Connect init + handler
                 {handler,
                  fun(_,_) -> "" end,
                  fun gdbstub:handle/2},
                 %% Registry message handler & state.
                 fun(Msg,State) -> 
                         tools:info("gdbstub reg: ~p~n", [Msg]),
                         State
                 end, 
                 #{},
                 [binary, {active, true}, {reuseaddr, true}])
       end,
       fun serv_tcp:handle/2}).

handle({tcp,_Sock,Data},State) ->
    handle_(Data,State).

handle_(<<$+,Data/binary>>, State) ->
    %% Ignore acks
    handle_(Data, State);
                        
handle_(Data, State) ->
   %% See notes in rsp.erl - this is a hack.
   tools:info("gdbstub: ~p~n", [Data]),
   {Rv, NextState} = rsp:update(Data, State),
   case Rv of
       {ok, Msg} ->
           tools:info("rsp: ~p~n", [rsp:unwrap(Msg)]);
       _ ->
           ignore
   end,
   NextState.

    
    

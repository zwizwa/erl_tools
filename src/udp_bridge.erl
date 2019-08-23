-module(udp_bridge).
-export([start/1, handle/2]).

start(PortNo) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                {ok, Sock} = gen_udp:open(PortNo, [binary, {active,true}]),
                #{ sock => Sock}
        end,
        fun ?MODULE:handle/2})}.

handle({udp,Sock,_Host,_Port,Data}=_Udp, State = #{ sock := Sock }) ->
    log:info("~p~n",[_Udp]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).

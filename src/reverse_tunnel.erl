-module(reverse_tunnel).
-export([start/2,singleshot/2,test/1]).

%% The point is to:
%% - Provide the machinery to produce 2 sockets
%% - Set them up so they forward to each other
%% - For the local listener, to accept only one connection

%% The key element here is a single shot connection, which is easier
%% to write a custom routine for as opposed to useing serv_tcp.

%% Implements the equivalent of /etc/inetd.conf:
%% # Reverse ssh tunnel.
%% 22001 stream tcp nowait nobody /usr/bin/socat /usr/bin/socat - tcp-listen:22002,reuseaddr
%%
%% But using a port pool for multiple connections.

info(F,A) -> log:info(F,A).

%% Main server
start(Incoming, {LocalListen,_} = _PortPool) ->

    serv:start(
      {handler,
       fun() ->
               Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
               serv_tcp:init(
                 [Incoming],
                 {handler,
                  %% Initialize after accept
                  fun(TunnelSock, _Port) -> 
                          %% Once an incoming tunnel is set up, we set
                          %% up a corresponding single-shot listening
                          %% socket.
                          Tunnel = self(),
                          singleshot(
                            LocalListen,
                            {handler,
                             fun(LocalSock) ->
                                     %% Once that is set up, both are connected.
                                     Tunnel ! {set_other, self()},
                                     #{ sock => LocalSock,
                                        other => Tunnel }
                             end,
                             fun forward_handle/2}),
                          #{ sock => TunnelSock,
                             other => not_yet_connected }
                  end,
                  fun forward_handle/2},
                 %% Handle other incoming messages: FIXME
                 fun obj:handle/2,
                 %% State for messages
                 #{},
                 Opts)
       end,
       %% Use default handler
       fun serv_tcp:handle/2}).

%% Forwarder, same for both ends.
forward_handle(Msg,State) ->
    log:info("~p~n", [Msg]),
    fw_handle(Msg, State).
fw_handle({tcp, _, Data}=_M, #{ other := Other }=State) ->
    Other ! {send, Data},
    State;
fw_handle({send, Data}, #{ sock := Sock}=State) ->
    gen_tcp:send(Sock, Data),
    State;
fw_handle({set_other, Other}, State) ->
    maps:put(other, Other, State).

    

    



               
singleshot(ListenPort, {handler, Init, Handle}) ->
    Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
    serv:start(
      {body,
       fun() ->
               Listener = self(),
               case gen_tcp:listen(ListenPort, Opts) of
                   {error, Reason} ->
                       info("singleshot listen error: port ~p: ~p~n", [ListenPort,Reason]),
                       exit(Reason);
                   {ok, ListenSock} ->
                       info("singleshot listening: ~p~n", [ListenPort]),
                       serv:start(
                         {handler,
                          fun() ->
                                  case gen_tcp:accept(ListenSock) of
                                      {error, Reason}  ->
                                          info("singleshot accept error: ~p~n", [{ListenPort,Reason}]),
                                          exit(Reason);
                                      {ok, ConnSock} -> 
                                          Listener ! close,
                                          info("singleshot accepted: ~p~n", [{ListenPort,ConnSock}]),
                                          Init(ConnSock)
                                  end
                          end,
                          Handle}),
                       %% Need to wait until socket is accepted
                       receive close -> ok end,
                       info("singleshot closing listener: ~p~n", [ListenPort]),
                       gen_tcp:close(ListenSock),
                       ok
               end
       end}).






test(Port) ->
    singleshot(
      Port,
      {handler,
       fun(Sock) ->
               info("sock: ~p~n",[Sock]),
               #{ sock => Sock }
       end,
       fun(Msg,State) ->
               info("msg: ~p~n",[Msg]),
               State
       end}).

    

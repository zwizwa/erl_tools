-module(reverse_tunnel).
-export([start/2,singleshot/3]).

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

-define(OPTS,[binary, {packet, 0}, {active, true}, {reuseaddr, true}]).

%% Main server
start(Incoming, PortPool) ->
    serv:start(
      {handler,
       fun() ->
               Registry = self(),
               serv_tcp:init(
                 [Incoming],
                 {handler,
                  %% Initialize after accept
                  fun(TunnelSock, _Port) -> 
                          %% Once an incoming tunnel is set up, we set
                          %% up a corresponding single-shot listening
                          %% socket.
                          Registry ! {start_singleshot, self()},
                          #{ sock => TunnelSock,
                             other => not_yet_connected }
                  end,
                  fun forward_handle/2},
                 %% Handle other incoming messages: FIXME
                 fun ({start_singleshot, Tunnel}, State) ->
                         OnError = fun() -> Tunnel ! close end,
                         singleshot(
                           PortPool, OnError,
                           {handler,
                            fun(LocalSock) ->
                                    link(Registry),
                                    %% Once that is set up, both are connected.
                                    Tunnel ! {set_other, self()},
                                    #{ sock => LocalSock,
                                       other => Tunnel }
                            end,
                            fun forward_handle/2}),
                         State;
                     (Msg, State) ->
                         obj:handle(Msg,State) 
                 end,
                 %% State for messages.  Mostly for debugging.
                 #{ port_pool => PortPool },
                 %% Socket options
                 ?OPTS)
       end,
       %% Use default handler
       fun serv_tcp:handle/2}).



%% Forwarder, same for both ends.
forward_handle(Msg,State) ->
    log:info("~p~n", [Msg]),
    fw_handle(Msg, State).
fw_handle({tcp, _, Data}, #{ other := Other }=State) ->
    Other ! {send, Data},
    State;
fw_handle({tcp_closed,_}, #{ other := Other }=State) ->
    Other ! close,
    State;
fw_handle({send, Data}, #{ sock := Sock}=State) ->
    gen_tcp:send(Sock, Data),
    State;
fw_handle(close, #{ sock := Sock}=State) ->
    gen_tcp:close(Sock),
    State;
fw_handle({set_other, Other}, State) ->
    maps:put(other, Other, State).

    

    


until_ok(_, []) -> {error, none_ok};
until_ok(Fun, [E|Es]) -> 
    case Fun(E) of
        {ok,Rv} -> {ok,{E,Rv}};
        Error ->
            log:info("until_ok: ~p~n", [{E,Error}]),
            until_ok(Fun,Es)
    end.
               
singleshot(PortPool, OnError, {handler, Init, Handle}) ->
    serv:start(
      {body,
       fun() ->
               Listener = self(),
               case until_ok(
                      fun(ListenPort) ->
                              RV = gen_tcp:listen(ListenPort, ?OPTS),
                              info("listen: ~p~n", [{ListenPort,RV}]),
                              RV
                      end, 
                      PortPool) of
                   {error, Reason} ->
                       info("singleshot listen error: port ~p: ~p~n", [PortPool,Reason]),
                       OnError();
                   {ok, {ListenPort, ListenSock}} = OK ->
                       info("singleshot listening: ~p~n", [ListenPort]),
                       serv:start(
                         {handler,
                          fun() ->
                                  case gen_tcp:accept(ListenSock) of
                                      {error, Reason}  ->
                                          info("singleshot accept error: ~p~n", [{ListenPort,Reason}]),
                                          exit(Reason);
                                      {ok, ConnSock} -> 
                                          %% Need to wait until socket is accepted
                                          Listener ! close,
                                          info("singleshot accepted: ~p~n", [{ListenPort,ConnSock}]),
                                          Init(ConnSock)
                                  end
                          end,
                          Handle}),
                       receive close -> ok end,
                       info("singleshot closing listener: ~p~n", [ListenPort]),
                       gen_tcp:close(ListenSock),
                       OK
               end
       end}).




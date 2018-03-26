-module(reverse_tunnel).
-export([start/2,singleshot/2,test/1]).

%% Implements the equivalent of /etc/inetd.conf:
%% # Reverse ssh tunnel.
%% 22001 stream tcp nowait nobody /usr/bin/socat /usr/bin/socat - tcp-listen:22002,reuseaddr
%%
%% But using a port pool for multiple connections.

info(F,A) -> log:info(F,A).
start(_,_) ->     
    ok.

%% %% Main server
%% start(Incoming, {LocalListen,_} = _PortPool) ->
%%     start_simple(
%%       Incoming,
%%       %% Tunnel setup connection is accepted.  Spawn a
%%       %% corresponding listener.
%%       fun (TunnelSock) ->
%%               Tunnel = self(),
%%               listener(Tunnel, LocalListen),
%%               #{ sock := TunnelSock,
%%                  other := listener(Tunnel, LocalListen) }


%%               info("tunnel connect: ~p~n", [TunnelSock]),
%%               Tunnel = self(),
%%               #{ tunnel_sock => TunnelSock,
%%                  listener => listener(Tunnel, LocalListen) }
%%       end,
%%       %% All data arriving is forwarded to the listener.
%%       fun (eof, #{ sock := Sock}=State) ->
%%               _ = gen_tcp:close(Sock), State;
%%           ({data, Data}, #{ sock := _Sock,
%%                             listener := _Listener } = State) ->
%%               log:info("tunnel incoming: ~p~n",[Data]),
%%               State;
%%           ({forward, Data}, #{ sock := Sock } = State) ->
%%               gen_tcp:send(Sock, Data),
%%               State;
%%       end).


%% %% Message handlers are the same on both the tunnel socket and the
%% %% local socket.
%% forward_handler(_Tag) ->
%%     fun(eof, #{ sock := Sock} = State) ->
%%             _ = gen_tcp:close(Sock),
%%             State;
%%        ({data, Data}=_M, #{ other := Other } = State) ->
%%             info("~p: ~p~n", [{_Tag,_M}]),
%%             Other ! {send, Data},
%%             State;
%%        ({send, Data}, #{ sock := Sock } = State) ->
%%             info("~p: ~p~n", [{_Tag,_M}]),
%%             gen_tcp:send(Sock, Data),
%%             State;
%%        end.


%% %% Per tunnel listener
%% listener(Tunn, LocalListen) ->
%%     start_simple(
%%       LocalListen,
%%       %% Local connection is accepted.
%%       fun (LocalSock) ->
%%               info("local connect: ~p~n", [LocalSock]),
%%               #{ local_sock => LocalSock }
%%       end,
%%       %% All data arriving is forwarded to the listener.
%%       fun (eof, #{ sock := Sock}=State) ->
%%               _ = gen_tcp:close(Sock), State;
%%           ({data,Data}, State) ->
              
%%               log:info("local incoming: ~p~n",[Data]),
%%               gen_tcp:send(TunnelSock, Data),
%%               State
%%       end).

%% %% API is a little weird.  We only need this basic template.
%% start_simple(Port,Init,Handle) ->
%%     serv:start(
%%     {handler,
%%      fun() ->
%%              serv_tcp:init(
%%                [Port],
%%                {handler,
%%                 %% Initialize after accept
%%                 fun(Sock,_Port) -> Init(Sock) end,
%%                 %% Handle incoming data
%%                 Handle},
%%                %% Handle other incoming messages
%%                fun obj:handle/2,
%%                %% State for messages
%%                #{})
%%      end,
%%      %% Use default handler
%%      fun serv_tcp:handle/2}).
                
                        
             
               

%% %% This is hard to express without an abstraction: a single-shot
%% %% server.

%% singleshot(Port, Connect) ->
%%     start_simple(
%%       Port,
%%       fun(
%%       Connect,
      
      
%%       fun(Socket) ->
              

                                   



%% The point is to:
%% - Provide the machinery to produce 2 sockets
%% - Set them up so they forward to each other
%% - For the local listener, to accept only one connection

%% The key element here is a single shot connection, which is easier
%% to write a custom routine for as opposed to useing serv_tcp.
               
singleshot(ListenPort, {handler, Init, Handle}) ->
    Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
    serv:start(
      {body,
       fun() ->
               Listener = self(),
               case gen_tcp:listen(ListenPort, Opts) of
                   {error, Reason} ->
                       info("listen error: port ~p: ~p~n", [ListenPort,Reason]),
                       exit(Reason);
                   {ok, ListenSock} ->
                       info("listening: ~p~n", [ListenPort]),
                       serv:start(
                         {handler,
                          fun() ->
                                  case gen_tcp:accept(ListenSock) of
                                      {error, Reason}  ->
                                          info("error: ~p~n", [{ListenPort,Reason}]),
                                          exit(Reason);
                                      {ok, ConnSock} -> 
                                          Listener ! accepted,
                                          gen_tcp:send(ConnSock, "Hello\n"),
                                          info("accepted: ~p~n", [{ListenPort,ConnSock}]),
                                          Init(ConnSock)
                                          
                                  end
                          end,
                          Handle}),
                       receive
                           Msg -> info("listener: ~p~n",[Msg])
                       end,
                       info("closing: ~p~n", [ListenPort]),
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

    

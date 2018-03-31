-module(reverse_tunnel).
-export([start/2,info/1,forward_handle/2]).

%% Create a reverse TCP tunnel.

%% This was originally written to be able to forward SSH connections
%% without an ssh client on the target.  If you have an ssh client and
%% key infrastructure setup, it is probably better to use that
%% instead.

%% The server implemented here  on the other end of a client:
%% socat tcp-connect:localhost:22 tcp-connect:$SERVER_IP:22001
%%
%% Note the 2 'tcp-connect' ends.  At the server end we need to
%% connect two listening sockets.
%%
%% A way to do that with socat is to put in /etc/inetd.conf:
%% 22001 stream tcp nowait nobody /usr/bin/socat /usr/bin/socat - tcp-listen:22002,reuseaddr
%%
%% This way, when a client connects to 22001, a new listeneing socket
%% is created on 22002, which can then be used to log into the
%% client's forwarded port 22.
%%
%% The socat solution only allows one connection.  The code below is a
%% generalization to multiple connections.



%% A key element in the implementation is to provide the mechanism of
%% a single-shot listening socket.  There are some subtleties in
%% getting that to work right.
%%
%% For the main incoming server the client's socat will connect to we
%% just use a serv_tcp instance.


info(F,A) -> log:info(F,A).

-define(OPTS,[binary, {packet, 0}, {active, true}, {reuseaddr, true}]).

%% Main TCP server.  Clients will connect here to set up reverse tunnel.
start(Incoming, PortRange) ->
    serv:start(
      {handler,
       fun() ->
               Registry = self(),
               serv_tcp:init(
                 [Incoming],
                 {handler,
                  fun(TunnelSock, _) -> forward_init(PortRange, TunnelSock, Registry) end,
                  fun reverse_tunnel:forward_handle/2},
                 %% Handle other incoming messages
                 fun ({'EXIT',_Pid,_Reason}=Msg, State) ->
                         log:info("~p~n",[Msg]),
                         State;
                     (Msg, State) ->
                         obj:handle(Msg,State) 
                 end,
                 %% State for messages.  Mostly for debugging.
                 #{ port_pool => PortRange },
                 %% Socket options
                 ?OPTS)
       end,
       %% Use default handler
       fun serv_tcp:handle/2}).



forward_init(PortRange, TunnelSock, Registry) ->
    %% TunnelSock is an accepted incoming connection.  Create a new
    %% single-shot listening socket.
    Tunnel = self(),
    log:info("incoming: ~p~n",[inet:peername(TunnelSock)]),
    SingleShot = 
        singleshot(

          %% The code that sets up the corresponding listener picks a
          %% port from the port range that is not busy.
          PortRange,

          %% Once the listening port is opened, this gets called so we
          %% can save the port number for later use by info/1.
          fun(ListenPort) -> obj:set(Tunnel, state, {port, ListenPort}) end,

          %% When anything goes wrong, we close up.
          fun() -> Tunnel ! close end,

          %% Once the local listening port is connected to, the
          %% processes associated to the two sockets are associated
          %% such that they can forward TCP traffic between them.
          {handler,
           fun(LocalSock) ->
                   link(Registry),
                   Tunnel ! {set_connected, self()},
                   obj:set(Tunnel, state, {connected, {Tunnel, self()}}),
                   #{ sock => LocalSock,
                      buffer => [],
                      other => {connected, Tunnel} }
           end,
           fun reverse_tunnel:forward_handle/2}),
    
    #{ sock => TunnelSock,
       buffer => [],
       other => {waiting, SingleShot}
     }.


%% A tunnel conists of two processes, one for each end.  Each of those
%% processes is associated to an accepted incoming socket: one comes
%% from the main incoming TCP. server, the other is from the local
%% one-shot TCP server.  Once the two processes are associated,
%% behavior is symmetric, so they use the same message handling method.

forward_handle(Msg, State) ->
    %% Debugging tap
    %% log:info("~p~n", [Msg]),
    fw_handle(Msg, State).

fw_handle({tcp, _, Data}, State) ->
    queue(Data, State);

fw_handle({send, Data}, #{ sock := Sock}=State) ->
    ok = gen_tcp:send(Sock, Data),
    State;

fw_handle({tcp_closed,_}, #{ other := {waiting, SingleShot} }) ->
    SingleShot ! close,
    exit(self(), normal);

fw_handle({tcp_closed,_}, #{ other := {connected, Other} }) ->
    Other ! close,
    exit(self(), normal);

fw_handle(close, #{ sock := Sock}) ->
    gen_tcp:close(Sock),
    exit(self(), normal);

fw_handle({set_connected, Other}, State) ->
    %% Once other end is there, flush dump the buffer (e.g. contains SSH banner).
    State1 = maps:put(other, {connected, Other}, State),
    flush(State1);

fw_handle(Msg, State) ->
    obj:handle(Msg, State).


queue(Data, #{ buffer := Buffer } = State) ->
    State1 = maps:put(buffer, [Data|Buffer], State),
    flush(State1).

flush(#{ other := {waiting, _}, buffer := _Buffer } = State) ->
    log:info("buffer: ~p~n",[_Buffer]),
    State;
flush(#{ other := {connected, Other}, buffer := Buffer} = State) ->
    lists:foreach(
      fun(Data) -> Other ! {send, Data} end,
      lists:reverse(Buffer)),
    maps:put(buffer, [], State).
    

    
%% socat tcp-connect:localhost:22 tcp-connect:10.1.3.29:22000

    

%% Try Fun until {ok,_}, given an integer range.  This is used
%% together with gen_tcp:listen/2 to find an available listening port.
until_ok(Fun, {Start, Endx}) ->
    case Start >= Endx of
        true ->
            {error, none_ok};
        false ->
            case Fun(Start) of
                {ok, Rv} -> {ok, {Start, Rv}};
                _Error ->
                    %% log:info("until_ok: ~p~n", [{Start,_Error}]),
                    until_ok(Fun, {Start+1,Endx})
            end
    end.

%% Listen, then stop listening once a connection is established.               
singleshot(PortRange,
           HavePort,
           OnError,
           {handler, Init, Handle}) ->
    serv:start(
      {body,
       fun() ->
               Listener = self(),
               case until_ok(
                      fun(ListenPort) ->
                              RV = gen_tcp:listen(ListenPort, ?OPTS),
                              %% info("listen: ~p~n", [{ListenPort,RV}]),
                              RV
                      end, 
                      PortRange) of
                   {error, none_ok} ->
                       info("no free ports: ~p~n", [PortRange]),
                       OnError();
                   {ok, {ListenPort, ListenSock}} = OK ->
                       info("listening: ~p~n", [ListenPort]),
                       HavePort(ListenPort),
                       serv:start(
                         {handler,
                          fun() ->
                                  case gen_tcp:accept(ListenSock) of
                                      {error, Reason}  ->
                                          info("accept error: ~p~n", [{ListenPort,Reason}]),
                                          exit(Reason);
                                      {ok, ConnSock} -> 
                                          %% Need to wait until socket is accepted
                                          Listener ! close,
                                          info("accepted: ~p~n", [{ListenPort,ConnSock}]),
                                          Init(ConnSock)
                                  end
                          end,
                          Handle}),
                       receive close -> ok end,
                       info("closing listener: ~p~n", [ListenPort]),
                       gen_tcp:close(ListenSock),
                       OK
               end
       end}).


info(Server) ->
    Here = self(),
    Ref = make_ref(),
    Server ! {apply, fun(Pids) -> Here ! {Ref, Pids} end},
    receive
        {Ref, Pids} ->
            [begin
                 {obj:get(P, state),              %% {listening,Port} or {connected,_}
                  inet:peername(obj:get(P, sock)),%% Incoming tunnel connection
                  obj:get(P, buffer)}             %% Forwarded server's TCP banner
             end || P <- Pids]
    end.

    


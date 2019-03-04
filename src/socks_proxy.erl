%% Proof of concept socks5 proxy.

-module(socks_proxy).
-export([start_serv/1, on_accept/1, handle/2]).
start_serv(#{ port := _ } = Spec) ->
    serv_tcp:start_link(
      maps:merge(
        Spec,
        #{ opts      => [binary,
                         {packet, raw},
                         {active, false},
                         {reuseaddr, true}],
           headers   => #{},
           on_accept => fun ?MODULE:on_accept/1,
           handle    => fun ?MODULE:handle/2 })).

on_accept(#{ sock := Sock} = State) ->
    {ok, PN} = inet:peername(Sock),
    log:set_info_name({socks,PN}),
    Recv = fun(N) -> {ok, D} = gen_tcp:recv(Sock, N), log:info("R: ~p~n",[D]), D end,
    Send = fun(D) -> log:info("S: ~p~n",[D]), gen_tcp:send(Sock, D) end,

    %% Handshake
    %% https://en.wikipedia.org/wiki/SOCKS
    %% Not checking _Auths.  Assume no auth is ok.    
    <<5, NbAuths>> = Recv(2),
    _Auths = Recv(NbAuths),
    Send(<<5, 0>>),
    %% FIXME: Assume only IPv4 here.
    <<5,1,0,1,A,B,C,D,Port:16>> = Recv(10),
    IP = {A,B,C,D},
    {ok, DstSock} = 
       gen_tcp:connect(
            IP,Port, [{active,true},{packet,raw},binary]),
    Send(<<5,0,0,1,127,0,0,1,100,0>>),
    inet:setopts(Sock, [{active, true}]),
    maps:put(dst_sock, DstSock, State).

handle({tcp,Sock,Data},
       #{ sock := Src, dst_sock := Dst } = State) ->
    case Sock of
        Src -> gen_tcp:send(Dst, Data);
        Dst -> gen_tcp:send(Src, Data)
    end,
    State;

handle({tcp_closed, Sock},
       #{ sock := Src, dst_sock := Dst } = _State) ->
    case Sock of
        Src ->
            log:info("src closed~n"),
            gen_tcp:close(Dst);
        Dst ->
            log:info("dst closed~n"),
            gen_tcp:close(Src)
    end,
    exit(normal);


handle(Msg,State) ->
   obj:handle(Msg,State).
    
%% curl --socks5 10.1.3.29:1080 http://google.com


-module(serv_ws).
-export([start_link/1, handle/2, on_accept/1]).

%% Stand-alone minimalistic websocket server.
%% FIXME: Just proof of concept.

%% ws = new WebSocket("ws://10.1.3.29:8123");
%% ws.send("123");


start_link(#{ port := _} = Spec) ->
    serv_tcp:start_link(
      maps:merge(
        Spec,
        #{ opts      => [binary,
                         {packet, http},
                         {active, once},
                         {reuseaddr, true}],
           headers   => #{},
           on_accept => fun ?MODULE:on_accept/1,
           handle    => fun ?MODULE:handle/2 })).

on_accept(#{ sock := _Sock} = State) ->
    State.

handle({http,Sock,{http_request,'GET',{abs_path,"/"},{1,1}}},
       #{ sock := Sock } = State) ->
    Headers = http:recv_headers(Sock),
    log:info("~p~n",[Headers]),
    Key64 = proplists:get_value("Sec-Websocket-Key", Headers),
    %% Key = base64:decode(Key64), log:info("~p~n",[Key]),
    Magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    KeyOut = base64:encode(crypto:hash(sha, [Key64,Magic])),
    Resp =
        [<<"HTTP/1.1 101 Switching Protocols\r\n",
           "Upgrade: websocket\r\n",
           "Connection: Upgrade\r\n",
           "Sec-WebSocket-Accept: ">>, KeyOut, <<"\r\n">>,
         <<"\r\n">>],
    log:info("resp:~n~s", [Resp]),
    inet:setopts(Sock, [{packet, raw}, {active, true}]),
    ok = gen_tcp:send(Sock, Resp),
    maps:put(headers, Headers, State);

handle({tcp,Sock,Data}, #{ sock := Sock}=State) ->
    log:info("~p~n", [Data]),    
    case parse1(Data) of
        #{ mask := 1, len := Len } when Len < 127  ->
            #{ key := Key, rest := Rest } = parse2(Data),
            Unmasked = xorkey(Key, 0, Rest),
            log:info("~p~n", [Unmasked])
    end,
    State;

handle({send, Data}, #{ sock := Sock}=State) when is_binary(Data)->
    Len = size(Data),
    true = Len < 127,
    Fin = 1, Opcode = 1, Mask = 1,
    <<Key32:32,_/binary>> = crypto:hash(sha, term_to_binary(erlang:timestamp())),
    Key = <<Key32:32>>,
    Encoded =
        [<<Fin:1,0:3,Opcode:4,Mask:1,Len:7>>,Key,
         xorkey(Key,0,Data)],
    log:info("~p~n", [Encoded]),
    gen_tcp:send(Sock, Encoded),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).


%% Several variants: FIXME: not complete
parse1(<<_Fin:1,_Res1:1,_Res2:1,_Res3:1,
         Opcode:4, Mask:1,
         Len:7, _/binary>>) ->
    #{
        opcode => Opcode,
        mask => Mask,
        len => Len
    }.
parse2(<<_Fin:1,_Res1:1,_Res2:1,_Res3:1, 
         Opcode:4, 1:1, Len:7, 
         Key:32,
         Rest/binary>>) ->
    #{
       opcode => Opcode,
       mask => 1,
       len => Len,
       key => <<Key:32>>,
       rest => Rest
    }.    

xorkey(M,N,Bin) when is_binary(Bin) ->
    xorkey(M,N,binary_to_list(Bin));
xorkey(_,_,[]) -> [];
xorkey(Mask,N,[B|Bs]) ->
    M = binary:at(Mask, N rem 4),
    [B bxor M | xorkey(Mask, N+1, Bs)].
    

-module(serv_ws).
-export([start_link/1, defaults/0, handle/2, on_accept/1]).

%% Stand-alone minimalistic websocket server.
%% FIXME: Not complete.  Modified as needed.

%% ws = new WebSocket("ws://10.1.3.29:8123");
%% ws.send("123");

defaults() ->
    #{ opts      => [binary,
                     {packet, http},
                     {active, once},
                     {reuseaddr, true}],
       headers   => #{},
       on_accept => fun ?MODULE:on_accept/1,
       handle    => fun ?MODULE:handle/2 }.
    

start_link(#{ port := _} = Spec) ->
    serv_tcp:start_link(
      maps:merge(defaults(), Spec)).


on_accept(State) ->
    %% Incoming state is a fork of the listener state.  Remove
    maps:filter(
      fun(Key,_Val) when is_pid(Key) -> false;
         (Key,_Val) ->
              not lists:member(
                    Key,
                    [listen_sock, on_accept, opts, port])
      end,
      State).
                         

%% Start the websocket
handle({http,Sock,{http_request,'GET',{abs_path,"/ws"}=_Path,{1,1}}},
       #{ sock := Sock } = State) ->

    %% log:info("~p~n", [{inet:peername(Sock),_Path}]),

    Headers = http:recv_headers(Sock),
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
    %% log:info("resp:~n~s", [Resp]),
    inet:setopts(Sock, [{packet, raw}, {active, true}]),
    ok = gen_tcp:send(Sock, Resp),


    WsUp = maps:get(ws_up, State, fun(S) -> S end),

    %% Remove unnecessary members.
    WsUp(
      maps:filter(
        fun(K,_) ->
                not lists:member(
                      K, [ws_up, headers, req])
        end,
        State));


%% Any other page is delegated to plugin.
handle({http,Sock,{http_request,'GET',Path,{1,1}}},
       #{ sock := Sock } = State) ->
    %% log:info("~p~n", [{inet:peername(Sock), Path}]),
    Headers = http:recv_headers(Sock),
    Resp =
        case maps:find(req, State) of
            {ok, Req} ->
                {ok, HttpResp} = Req({Path,Headers}, State),
                %% log:info("HttpResp: ~p~n", [HttpResp]),
                HttpResp;
            _ ->
                [<<"HTTP/1.1 404 Not Found\r\n">>]
        end,
    ok = gen_tcp:send(Sock, Resp),
    gen_tcp:close(Sock),
    exit(normal);
    %% inet:setopts(Sock, [{packet, http},{active,once}]),
    %% State;

%% FIXME: Handle partial frames.
handle({tcp,Sock,Data}=_Msg, #{ sock := Sock}=State) ->
    %% log:info("~p~n", [_Msg]),    
    case parse(Data) of
        {Decoded = #{ 
           mask   := 1,
           opcode := Opcode,
           data   := _
          }, Rest} ->
            case Opcode of
                8 ->
                    exit({ws_connection_close, Opcode});
                _ ->
                    self() ! {data, Decoded},
                    case Rest of
                        <<>> -> State;
                        _ -> handle({tcp,Sock,Rest}, State)
                    end
                end;
        Error ->
            log:info("serv_ws: can't parse~n~p~n~p~n", [Data,Error]),
            State
    end;

handle({send, Data}, #{ sock := Sock}=State) when is_binary(Data)->
    Len = size(Data),
    %% According to chrome, server must not mask any frames that it
    %% sends to the client.

    %% <<Key32:32,_/binary>> =
    %%    crypto:hash(sha, term_to_binary(erlang:timestamp())),
    %% Key = <<Key32:32>>,

    Fin = 1, Opcode = 1, Mask = 0,
    {LenCode,ExtraLen} =
        if Len < 126   -> {Len,[]};
           Len < 65535 -> {126,<<Len:16>>};
           true        -> {127,<<Len:64>>}
        end,
    Encoded = [<<Fin:1,0:3,Opcode:4,Mask:1,LenCode:7>>,
               ExtraLen, 
               %% Key, xorkey(Key,0,Data)
               Data
              ],
    gen_tcp:send(Sock, Encoded),
    State;

handle({tcp_closed,_Sock}=Msg, _State) ->
    exit(Msg);

%% For super
handle({req,_}=_Msg, State) ->
    log:info("serv_ws:handle: ~p~n",[_Msg]),
    State;
handle({data,_}, State) ->
    State;

handle({_,dump} = Msg, State) ->
    obj:handle(Msg, State);

handle(Msg, _State) ->
    E = {bad_request, Msg},
    log:info("~p~n", [E]),
    exit(E).



%% Several variants: FIXME: not complete

%% https://tools.ietf.org/html/rfc6455

%% Large size message (untested)
parse(<<Fin:1,_Res1:1,_Res2:1,_Res3:1, 
        Opcode:4, Mask:1, 127:7,
        Len:64,
        Key:32,
        Bin/binary>>) ->
    Masked  = binary:part(Bin, 0, Len),
    Rest    = binary:part(Bin, Len, size(Bin)-Len),
    Unmasked = xorkey(<<Key:32>>, 0, Masked),

    {#{ fin    => Fin,
        opcode => Opcode,
        mask   => Mask,
        len    => Len,
        data   => Unmasked
      }, Rest};

%% Medium size message
parse(<<Fin:1,_Res1:1,_Res2:1,_Res3:1, 
        Opcode:4, Mask:1, 126:7,
        Len:16,
        Key:32,
        Bin/binary>>) ->
    Masked  = binary:part(Bin, 0, Len),
    Rest    = binary:part(Bin, Len, size(Bin)-Len),
    Unmasked = xorkey(<<Key:32>>, 0, Masked),

    {#{ fin    => Fin,
        opcode => Opcode,
        mask   => Mask,
        len    => Len,
        data   => Unmasked
      }, Rest};

%% Remainder case: small message (Len < 126)
parse(<<Fin:1,_Res1:1,_Res2:1,_Res3:1, 
        Opcode:4, Mask:1, Len:7, 
        Key:32,
        Bin/binary>>) ->
    Masked  = binary:part(Bin, 0, Len),
    Rest    = binary:part(Bin, Len, size(Bin)-Len),
    Unmasked = xorkey(<<Key:32>>, 0, Masked),

    {#{ fin    => Fin,
        opcode => Opcode,
        mask   => Mask,
        len    => Len,
        data   => Unmasked
      }, Rest};

parse(Data) ->
    {ws_parse_error, Data}.


xorkey(M,N,Bin) when is_binary(Bin) ->
    xorkey(M,N,binary_to_list(Bin));
xorkey(_,_,[]) -> [];
xorkey(Mask,N,[B|Bs]) ->
    M = binary:at(Mask, N rem 4),
    [B bxor M | xorkey(Mask, N+1, Bs)].
    

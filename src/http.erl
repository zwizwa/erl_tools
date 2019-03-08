-module(http).

%% Some misc wrappers for gen_tcp

-export([recv_headers_and_data/1,
         recv_headers/1,
         recv_data/2,
         recv_rest/1,
         fmt_request/1,
         fmt_response/1,
         fmt_headers/1,
         get/1, post/2
         ]).

recv_headers_and_data(Sock) ->
    Headers = recv_headers(Sock),
    Data = recv_data(Sock, Headers),
    {Headers,Data}.

recv_headers(Sock) ->
    recv_headers(Sock, []).
recv_headers(Sock, Headers) ->
    case gen_tcp:recv(Sock, 0) of
        {ok,{http_header,_,K,_,V}} ->
            recv_headers(Sock, [{K,V}|Headers]);
        {ok,http_eoh} ->
            lists:reverse(Headers)
    end.

recv_data(Sock, Len) when is_number(Len) ->
    inet:setopts(Sock, [{packet, raw}]),
    case gen_tcp:recv(Sock, Len) of
        {ok, Data} ->
            %% log:info("Data: ~p~n", [Data]),
            inet:setopts(Sock, [{packet, http}, {active, once}]),
            Data;
        {error, closed} ->
            ""
    end;
recv_data(Sock, Headers) ->
    case proplists:get_value('Content-Length', Headers) of
        undefined ->
            throw({no_content_length, Headers});
        ContentLength ->
            %% log:info("Len: ~p~n", [Len]),
            recv_data(Sock, list_to_integer(ContentLength))
    end.



recv_rest(Sock) -> recv_rest(Sock,[]).
recv_rest(Sock,Stack) ->
    Rv = gen_tcp:recv(Sock, 0),
    %% log:info("Rv: ~p~n", [Rv]),
    case Rv of
        {ok, Data} ->
            recv_rest(Sock, [Data|Stack]);
        {error, closed} ->
            lists:reverse(Stack)
    end.
            

fmt_request({http_request,Method,{abs_path,Path},{V1,V2}}) ->
    io_lib:format("~s ~s HTTP/~p.~p\r\n", [Method, Path, V1, V2]).

fmt_response({http_response,{V1,V2},Status,Reason}) ->
    io_lib:format("HTTP/~p.~p ~p ~s\r\n",[V1,V2,Status,Reason]).

fmt_headers(Headers) ->
    [[io_lib:format("~s: ~s\r\n", [K,V]) || {K,V} <- Headers],"\r\n"].
     


get({Host,Port,Path}) ->
    Opts = [{active,once},{packet,http},binary],
    {ok, Sock} = gen_tcp:connect(Host,Port,Opts),
    Req =
        [fmt_request({http_request,'GET',{abs_path,Path},{1,1}}),
         fmt_headers(
           [{<<"User-Agent">>,<<"http:get">>},
            {<<"Host">>,Host},
            {<<"Accept">>,<<"*/*">>},
            {<<"Connection">>,<<"keepalive">>}
           ])],
    %% log:info("~n~s",[Req]),
    gen_tcp:send(Sock, Req),
    Rv =
        receive 
            {http, Sock, {http_response,{1,1},Status,_}} ->
                case Status of
                    200 ->
                        Headers = recv_headers(Sock),
                        %% log:info("response:~n~p~n", [Headers]),
                        recv_data(Sock, Headers);
                    _ ->
                        throw({status, Status})
                end
        end,
    gen_tcp:close(Sock),
    Rv.


post({Host,Port,Path},Bin) when is_binary(Bin)->
    Opts = [{active,once},{packet,http},binary],
    {ok, Sock} = gen_tcp:connect(Host,Port,Opts),
    Req =
        [fmt_request({http_request,'POST',{abs_path,Path},{1,1}}),
         fmt_headers(
           [{<<"User-Agent">>,<<"http:get">>},
            {<<"Host">>,Host},
            {<<"Accept">>,<<"*/*">>},
            {<<"Content-Type">>,<<"application/binary">>},
            {<<"Content-Length">>,integer_to_binary(size(Bin))},
            {<<"Connection">>,<<"keepalive">>}
           ]),
         Bin],
    %% log:info("~n~s",[Req]),
    gen_tcp:send(Sock, Req),
    Rv =
        receive 
            {http, Sock, {http_response,{1,1},Status,_}} ->
                case Status of
                    200 ->
                        Headers = recv_headers(Sock),
                        %% log:info("response:~n~p~n", [Headers]),
                        recv_data(Sock, Headers);
                    _ ->
                        throw({status, Status})
                end
        end,
    gen_tcp:close(Sock),
    Rv;
    
%% A simple ad-hoc Erlang term call through http post.
post(Dst, {rpc, Req}) ->
    BResp = post(Dst, term_to_binary(Req)),
    {ok, Resp} = erlang:binary_to_term(BResp, [safe]),
    Resp.

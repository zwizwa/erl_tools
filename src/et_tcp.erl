-module(et_tcp).

%% Some misc wrappers for gen_tcp

-export([recv_http_headers_and_data/1,
         recv_http_headers/1,
         recv_http_data/2,
         recv_rest/1,
         fmt_request/1,
         fmt_response/1,
         fmt_headers/1
         ]).

recv_http_headers_and_data(Sock) ->
    Headers = recv_http_headers(Sock),
    Len = list_to_integer(proplists:get_value('Content-Length', Headers)),
    %% log:info("Len: ~p~n", [Len]),
    Data = recv_http_data(Sock, Len),
    {Headers,Data}.

recv_http_headers(Sock) ->
    recv_http_headers(Sock, []).
recv_http_headers(Sock, Headers) ->
    case gen_tcp:recv(Sock, 0) of
        {ok,{http_header,_,K,_,V}} ->
            recv_http_headers(Sock, [{K,V}|Headers]);
        {ok,http_eoh} ->
            lists:reverse(Headers)
    end.

recv_http_data(Sock, Len) ->
    inet:setopts(Sock, [{packet, raw}]),
    case gen_tcp:recv(Sock, Len) of
        {ok, Data} ->
            %% log:info("Data: ~p~n", [Data]),
            inet:setopts(Sock, [{packet, http}, {active, once}]),
            Data;
        {error, closed} ->
            ""
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
     

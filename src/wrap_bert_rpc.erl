-module(wrap_bert_rpc).
-export([start_link/1, handle/2]).

%% Wrapper / monitor for BERT RPC servers.
%% Notes:
%% - The other end sends {packet,4}, but why do we send raw ETF?

start_link(#{ spec := {_Host, _Port}} = Info) ->
    {ok, 
     serv:start(
       {handler,
        fun() -> self() ! connect, Info end,
        fun wrap_bert_rpc:handle/2})}.

handle(connect, State) ->
    Retries = maps:get(retries, State, 1),
    State1 = maps:put(sock, connect(State, Retries), State),
    OnConnect = maps:get(on_connect, State1, fun(S) -> S end),
    OnConnect(State1);

handle({Pid, {M,F,A}}, #{ sock := Sock } = State) ->
    gen_tcp:send(Sock, term_to_binary({call,M,F,A})),
    receive
        {tcp,Sock,Data} ->
            case binary_to_term(Data) of
                {reply, Term} ->
                    obj:reply(Pid, {ok, Term})
            end
    end,
    State;

handle({tcp_closed,Sock}=_Msg, #{ sock := Sock }=State) ->
    %% log:info("~p~n", [_Msg]),
    handle(connect, State);
                    
handle(Msg,State) ->
    obj:handle(Msg,State).

connect(State,0) ->
    exit({error_connect, State});
connect(State=#{spec := {Host,Port}}, Tries) ->
    case gen_tcp:connect(
           Host,Port,
           [{active,true},{packet,4},binary]) of
        {ok, Sock} ->
            log:info("connected: ~p~n", [{Host,Port}]),
            Sock;
        _Error = {error,econnrefused} ->
            %% log:info("error: ~p~n", [_Error]),
            Start = maps:get(start, State, fun() -> ok end),
            Start(),
            Ms = maps:get(delay, State, 1000),
            %% blog:info("waiting ~p ms to reconnect~n", [Ms]),
            timer:sleep(Ms),
            connect(State, Tries-1)
    end.

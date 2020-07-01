-module(bert_rpc).
-export([start_link/1, handle/2, call/5, call/6]).

%% Ad-hoc tools for interacting with BERT RPC servers.
%% Notes:
%% - The other end sends {packet,4}, but why do we send raw ETF?


%% Server ineterface.
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

handle({Pid, {M,F,A}}=Msg, State) ->
    case maps:find(sock, State) of
        {ok, Sock} ->
            %% log:info("bert_rpc: ~p~n", [Msg]),
            gen_tcp:send(Sock, term_to_binary({call,M,F,A})),
            receive
                {tcp,Sock,Data} ->
                    case binary_to_term(Data) of
                        {reply, Term} ->
                            obj:reply(Pid, {ok, Term});
                        {error,_}=Err ->
                            obj:reply(Pid, Err)
                    end
            end,
            State;
        error ->
            %% Autoconnect.  Maybe make this optional?
            State1 = handle(connect, State),
            %% Assert sock is there before performing recursive call
            #{ sock := _ } = State1,
            handle(Msg, State1)
    end;

handle({tcp_closed,Sock}=_Msg, #{ sock := Sock }=State) ->
    %% log:info("~p~n", [_Msg]),
    handle(connect, State);
                    
handle({_,dump} = Msg,State) ->
    obj:handle(Msg,State);

%% Makes it easier to use handle/2 as a delegate.
handle(Msg, _State) ->
    throw({?MODULE, {error, Msg}}).


connect(State,0) ->
    exit({error_connect, State});
connect(State=#{spec := {Host,Port}}, Tries) ->
    log:info("bert_rpc: connect ~s:~p~n", [Host,Port]),
    case gen_tcp:connect(
           Host,Port,
           [{active,true},{packet,4},binary]) of
        {ok, Sock} ->
            log:info("bert_rpc: connected: ~p~n", [{Host,Port}]),
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



%% Perform just a call
call(Host,Port,M,F,A) ->
    call(Host,Port,M,F,A,3000).
call(Host,Port,M,F,A,Timeout) when is_atom(M) and is_atom(F) ->
    case 
        gen_tcp:connect(
          Host, Port,
          [binary, {packet, 4}, {active, false}]) of
        {ok, Sock} -> 
            ok = gen_tcp:send(
                   Sock, term_to_binary({call,M,F,A})),
            Resp = gen_tcp:recv(Sock, 0, Timeout),
            %% Close before parsing.
            ok = gen_tcp:close(Sock),
            {ok, Bin} = Resp,
            {reply, Rv} = binary_to_term(Bin),
            {ok, Rv};
        {error, _}=E ->
            E
    end.

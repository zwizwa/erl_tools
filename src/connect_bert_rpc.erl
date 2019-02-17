-module(connect_bert_rpc).
-export([start_link/1, handle/2]).

%% Wrapper / monitor for BERT RPC servers.

start_link(#{ spec := {_Host, _Port},
              start := _Start} = Info) ->
    {ok, 
     serv:start(
       {handler,
        fun() -> self() ! connect, Info end,
        fun connect_bert_rpc:handle/2})}.

handle(connect, #{ spec := Spec, start := Start } = State) ->
    maps:put(sock, connect(Spec,Start,3), State);

handle({Pid, {M,F,A}}, #{ sock := Sock } = State) ->
    gen_tcp:send(Sock, term_to_binary({call,M,F,A})),
    receive
        {tcp,Sock,Data} ->
            case  binary_to_term(Data) of
                {reply, Term} ->
                    obj:reply(Pid, {ok, Term})
            end
    end,
    State;

handle({tcp_closed,Sock}=_Msg, #{ sock := Sock }=State) ->
    log:inco("~p~n", [_Msg]),
    handle(connect, State);
                    
handle(Msg,State) ->
    obj:handle(Msg,State).

connect(Spec,_,0) ->
    exit({error_connect, Spec});
connect({Host,Port}=Spec,Start,Tries) ->
    case gen_tcp:connect(
           Host,Port,
           [{active,true},{packet,4},binary]) of
        {ok, Sock} ->
            log:info("connected: ~p~n", [Spec]),
            Sock;
        Error ->
            log:info("error: ~p~n", [Error]),
            Start(),
            Ms = 1000,
            log:info("waiting ~p ms to reconnect~n", [Ms]),
            timer:sleep(Ms),
            connect(Spec, Start, Tries-1)
    end.
            
         

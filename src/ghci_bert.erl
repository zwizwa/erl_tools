%% Wrappers for running a BERT server inside ghci, with code reload
%% and server restart.  The BERT server responds to control:stop() to
%% fall back into the ghci prompt.

%% FIXME: Remove hardcoded bits.


-module(ghci_bert).
-export([reload/1, ghci_init/0]).

%% Pid is a ghci wrapper process from ghci.erl
reload(Pid) ->
    #{ tcp_host := Host,
       tcp_port := Port } = obj:dump(Pid),
    Call =
        fun(M,F,A) ->
                bert_rpc:call(Host,Port,M,F,A,1000)
        end,
    %% Explicit clear.  Only the rpc mechanism performs implicit
    %% clear, and we're not using that beacause "start" will block.
    Pid ! clear,
    %% Enqueue before stopping.  If reload fails, start will be undefined.
    Pid ! {cmds, [":reload","start"]},
    _ = Call(control,stop,[]),
    %% Make sure it is up.
    case wait_bert_up(20, Call) of
        ok -> {ok,{Host,Port}};
        E -> E
    end.

%% There is no start synchronization, so just poll until the other end
%% replies..
wait_bert_up(0,_) -> error;
wait_bert_up(N,Call) -> 
    case Call(control,pid,[]) of
        {ok,_} -> ok;
        _Error={error,econnrefused} ->
            %% log:info("wait_bert_up: ~p~n", [_Error]),
            timer:sleep(500),
            wait_bert_up(N-1,Call)
    end.
            
ghci_init() ->
    {cmds, ["start"]}.
    

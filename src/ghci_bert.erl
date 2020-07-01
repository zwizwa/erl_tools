%% Wrappers for running a BERT server inside ghci, with restarts.
%% The BERT server responds to control:stop() to fall back into the ghci prompt.

%% FIXME: Remove hardcoded bits.


-module(ghci_bert).
-export([reload/1, ghci_init/0]).

%% Pid is a ghci wrapper process from ghci.erl
reload(Pid) ->
    Host = "localhost",
    Port = 7890, 
    Call =
        fun(M,F,A) ->
                bert_rpc:call(Host,Port,M,F,A,1000)
        end,
    %% Enqueue before stopping.
    Pid ! {cmds, [":reload","start"]},
    Call(control,stop,[]),
    %% Make sure it is up.
    case wait_bert_up(300, Call) of
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
    

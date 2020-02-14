-module(rigol).
-export([start_link/1, handle/2, test/1,
        rpc/1, rpc/2]).

%% https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf

%% A nice goal would be to move data from the rigol into gtkwave.

start_link(#{ tcp := {_Host,_Port} } = Spec) ->
    {ok,
     serv:start(
       {handler,
        fun() -> Spec end,
        fun ?MODULE:handle/2})}.

handle(disconnect, State) ->
    case maps:find(sock, State) of
        error ->
            log:info("not connected~n"),
            State;
        {ok, Sock} ->
            gen_tcp:close(Sock),
            maps:remove(sock, State)
    end;
handle(connect, State) ->
    case maps:find(sock, State) of
        {ok, _} ->
            log:info("already connected~n"),
            State;
        error ->
            Retries = maps:get(retries, State, 1),
            State1 = maps:put(sock, connect(State, Retries), State),
            OnConnect = maps:get(on_connect, State1, fun(S) -> S end),
            OnConnect(State1)
    end;

%% Single ended messages.
handle({cmd, Cmd}, #{ sock := Sock} = State) ->
    gen_tcp:send(Sock, Cmd), State;
handle({tcp, Sock, Data}, #{ sock := Sock } = State) ->
    case maps:find(cont, State) of
        {ok, Cont} ->
            Cont(Data, maps:remove(cont, State));
        error ->
            log:info("no cont: ~p~n", [Data]),
            State
    end;

%% RPCs.  Note that some SCPI commands do not produce a reply.  What
%% we call an RPC is a bunch of such commands bundled up, followed
%% with one command that does produce a reply.
handle({Pid, {rpc, RPCSpec}}, State) ->
    case rpc_scpi(RPCSpec) of
        {error,_} = Error ->
            obj:reply(Pid, Error),
            State;
        {ok, Cmd} ->
            Cont = fun(D,S) -> obj:reply(Pid, {ok, D}), S end,
            handle(
              {cmd, Cmd},
              maps:put(cont, Cont, State))
    end;

%% Misc
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    log:info("WARNING: ~p~n", [Msg]),
    State.

%% RPC specs
rpc_scpi(idn) -> {ok, "*IDN?\n"};
rpc_scpi(RPC) -> {error, {bad_rpc, RPC}}.
     

connect(State,0) ->
    exit({error_connect, State});
connect(State=#{tcp := {Host,Port}}, Tries) ->
    log:info("connect ~s:~p~n", [Host,Port]),
    case gen_tcp:connect(
           Host,Port,
           [{active,true},{packet,raw},binary]) of
        {ok, Sock} ->
            log:info("connected: ~p~n", [{Host,Port}]),
            Sock;
        _Error = {error,econnrefused} ->
            %% log:info("error: ~p~n", [_Error]),
            Start = maps:get(start, State, fun() -> ok end),
            Start(),
            Ms = maps:get(delay, State, 5000),
            log:info("waiting ~p ms to reconnect~n", [Ms]),
            timer:sleep(Ms),
            connect(State, Tries-1)
    end.

rpc(Spec) ->
    rpc(?MODULE, Spec).
rpc(Pid, Spec) ->
    obj:call(Pid, {rpc, Spec}, 3000).


test(Spec) ->
    throw({?MODULE,bad_test_spec,Spec}).


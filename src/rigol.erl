-module(rigol).
-export([start_link/1, handle/2,
         test/1,
         cmd/2]).

%% https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf

%% A nice goal would be to move data from the rigol into gtkwave.

%% Notes:

%% - Work in passive mode.  Operation is mostly RPC with some ad-hoc
%%   encoding that is easier to do as read_header -> read_data
%%   dependencies.
%%
%% - Delays are added for commands that do not have an ack.  How to do
%%   this properly?


start_link(#{ tcp := {_Host,_Port} } = Spec) ->
    {ok,
     serv:start(
       {handler,
        fun() -> self() ! connect, Spec end,
        fun ?MODULE:handle/2})}.

handle(connect, State) ->
    case maps:find(sock, State) of
        {ok, _} ->
            State;
        error ->
            Retries = maps:get(retries, State, 1),
            State1 = maps:put(sock, connect(State, Retries), State),
            OnConnect = maps:get(on_connect, State1, fun(S) -> S end),
            OnConnect(State1)
    end;

%% Transaction: run a user-specified program against the socket.
handle({Pid, {run, Spec}}, #{ sock := Sock} = State) ->
    DelayMS = 200,  %% FIXME: Is there proper flow control?
    Write = fun(IOList) ->
                    gen_tcp:send(Sock, IOList),
                    timer:sleep(DelayMS)
            end,
    Read  = fun(N) -> read(Sock,N) end,
    Rv = program(Write, Read, Spec),
    obj:reply(Pid, Rv),
    State;

%% Misc
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    log:info("WARNING: ~p~n", [Msg]),
    State.

%% Programs that communication with the device through write and read
%% operations.
program(W,R,idn) ->
    W("*IDN?\n"), {ok, R(line)};
program(W,R,disp_data) ->
    W(":DISP:DATA?\n"), 
    Data = R(tmc),
    _ = R(line),
    {ok, Data};
program(W,R,wav_pre) ->
    W(":WAV:PRE?\n"), 
    {ok,
     lists:zip(
       [format,type,points,count,
        xincrement,xorigin,xreference,
        yincrement,yorigin,yreference],
       lists:map(
         fun to_number/1,
         re:split(R(line),",")))};
program(W,R,{wav_data,Chan,Start,Stop}) ->
    W([":WAV:SOUR CHAN",p(Chan),"\n"]),
    W([":WAV:MODE RAW\n"]),
    W([":WAV:FORM BYTE\n"]),
    W([":WAV:STAR ",p(Start),"\n"]),
    W([":WAV:STOP ",p(Stop),"\n"]),
    W("WAV:DATA?\n"), 
    Data = R(tmc),
    _ = R(line),
    {ok, Data};
program(_,_,Spec) ->
    {error, Spec}.


%% user API
cmd(Pid, Spec) ->
    obj:call(Pid, {run, Spec}, 3000).


%% connect with retry            
connect(State,0) ->
    exit({error_connect, State});
connect(State=#{tcp := {Host,Port}}, Tries) ->
    log:info("connect ~s:~p~n", [Host,Port]),
    case gen_tcp:connect(
           Host,Port,
           [{active,false},{packet,raw},binary]) of
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

%% Receiver has some special purpose parsers.  Read errors translate
%% to pattern matching errors.
read(Sock, line) ->
    case read(Sock, 1) of
        <<"\n">> -> [];
        <<Char>> -> [Char | read(Sock, line)]
    end;
read(Sock, tmc_blockheader) ->
    <<"#",CNumSize>> = read(Sock, 2),
    Num = CNumSize - $0,
    SNumData = read(Sock, Num),
    binary_to_integer(SNumData);
read(Sock, tmc) ->
    NumData = read(Sock, tmc_blockheader),
    read(Sock, NumData);
read(_Sock, 0) ->
    <<>>;
read(Sock, flush) ->
    {ok, Data} = gen_tcp:recv(Sock, 0),
    Data;
read(Sock, N) when is_integer(N) ->
    {ok, Data} = gen_tcp:recv(Sock, N),
    Data.

%% miac tools
p(T) ->
    io_lib:format("~p",[T]).
to_number(Bin) ->
    try binary_to_float(Bin)
    catch _:_ -> binary_to_integer(Bin) end.

%% tests            
test(Spec) ->
    test(exo:need(rigol), Spec).
test(P,{size,Spec}) ->
    Data = cmd(P,Spec),
    size(Data);
test(P,snapshot) ->
    {ok, Data} = cmd(P,disp_data),
    file:write_file("/tmp/scope.bmp", Data);
test(_,Spec) ->
    throw({?MODULE,bad_test_spec,Spec}).


-module(rigol).
-export([start_link/1, handle/2,
         test/1,
         cmd/3]).

%% https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf

%% A nice goal would be to move data from the rigol into gtkwave.

%% Notes:

%% - Work in passive mode.  Operation is mostly RPC with some ad-hoc
%%   encoding that is easier to do as read_header -> read_data
%%   dependencies.
%%
%% - Delays are added for commands that do not have an ack.  How to do
%%   this properly?

%% Sampling parameters.  The ms/div can be set.  The rest is set by scope:
%% 500ms/div => 2MHz, 12Mpts, 6000ms


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
            connect(State)
    end;

%% Transaction: run a user-specified program against the socket.
handle({Pid, {run, Spec}}, State) ->
    {Rv, State1} = retry_program(Spec, State, 5),
    obj:reply(Pid, Rv),
    State1;

%% Misc
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    log:info("WARNING: ~p~n", [Msg]),
    State.

%% This thing is buggy.  Keep retrying on error.
retry_program(Spec, _, 0) ->
    throw({rigol_retry_program, Spec});
retry_program(Spec, State=#{sock := Sock}, Retry) ->
    DelayMS = 200,  %% FIXME: Is there proper flow control?
    Write = fun(IOList) ->
                    gen_tcp:send(Sock, IOList),
                    timer:sleep(DelayMS)
            end,
    Read  = fun(N) -> read(Sock,N) end,
    try 
        Rv = program(Write, Read, Spec),
        {Rv, State}
    catch _C:_E ->
            %% log:info("errror: ~p~n",[{_C,_E}]),
            %% gen_tcp:close(Sock),
            %% timer:sleep(100),
            %% retry_program(Spec, connect(State))
            Read(flush),
            retry_program(Spec, State, Retry-1)
    end.
    


%% Programs that communication with the device through write and read
%% operations.
program(W,R,idn) ->
    W("*IDN?\n"), {ok, R(line)};
program(W,R,disp_data) ->
    W(":DISP:DATA?\n"), 
    Data = R(tmc),
    _ = R(line),
    {ok, Data};
program(W,R,wav_stat) ->
    W(":WAV:STAT?\n"),
    {ok,R(line)};
program(W,R,wav_pre) ->
    W(":WAV:PRE?\n"), 
    {ok,
     maps:from_list(
       lists:zip(
         [format,type,points,count,
          xincrement,xorigin,xreference,
          yincrement,yorigin,yreference],
         lists:map(
           fun to_number/1,
           re:split(R(line),","))))};
program(W,R,{wav_data,Chan,Start,Stop}=P) ->
    log:info("~p~n",[P]),
    W([":STOP"]),
    W([":WAV:SOUR CHAN",p(Chan),"\n"]),
    W([":WAV:MODE RAW\n"]),
    W([":WAV:FORM BYTE\n"]),
    W([":WAV:STAR ",p(Start),"\n"]),
    W([":WAV:STOP ",p(Stop),"\n"]),
    W("WAV:DATA?\n"), 
    Data = R(tmc),
    _ = R(line),
    {ok, Data};
%% Download in chunks of 240k points.
%% This means there are 10 chunks in total.

program(W,R,{wav_data_chunks,Chan,NbChunks}) ->
    {ok,
     lists:map(
       fun(N) ->
               log:info("chunk ~p/~p~n",[N,NbChunks]),
               {ok, Bin} = program(W,R,{wav_data_chunk, Chan, N}),
               Bin
       end,
       lists:seq(1,NbChunks))};
program(W,R,{wav_data_chunk,Chan,ChunkNb}) ->
    ChunkSize = 240000,
    Start = (ChunkNb-1) * ChunkSize + 1,
    End   = Start-1+ChunkSize,
    program(W,R,{wav_data,Chan,Start,End});
program(W,R,{wav_data,Chan}) ->
    {ok, #{ points := Points }} = program(W,R,wav_pre),
    program(W,R,{wav_data,Chan,1,Points});
program(W,R,acq_mdep) ->
    W([":ACQ:MDEP?"]),
    {ok, R(line)};
program(W,R,acq_srat) ->
    W([":ACQ:SRAT?"]),
    {ok, to_number(R(line))};
program(W,_,{acq_mdep,N}) when is_number(N) ->
    W(tools:format(":ACQ:MDEP ~p~n",[N]));
program(W,_,run) ->
    W(":RUN");
program(W,_,stop) ->
    W(":STOP");

%% DOESNT WORK
%% program(W,R,trig_pos) ->
%%     W([":TRIG:POS?"]),
%%     {ok, R(line)};
program(W,R,tim_main_scal) ->
    W([":TIM:MAIN:SCAL?"]),
    {ok, to_number(R(line))};
program(W,_,{tim_main_scal,PerDiv}) ->
    W(tools:format(":TIM:MAIN:SCAL ~p",[PerDiv]));
    
program(_,_,Spec) ->
    {error, Spec}.


%% user API
cmd(Pid, Spec, Timeout) ->
    obj:call(Pid, {run, Spec}, Timeout).


%% connect with retry            
connect(State=#{tcp := {Host,Port}}) ->
    log:info("connect ~s:~p~n", [Host,Port]),
    case gen_tcp:connect(
           Host,Port,
           [{active,false},{packet,raw},binary]) of
        {ok, Sock} ->
            log:info("connected: ~p~n", [{Host,Port}]),
            maps:put(sock, Sock, State);
        Error ->
            throw(Error)
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
to_number(IOList) ->
    Bin = iolist_to_binary(IOList),
    try binary_to_float(Bin)
    catch _:_ -> binary_to_integer(Bin) end.

%% tests            
test(Spec) ->
    test(exo:need(rigol), Spec).
test(P,{size,Spec}) ->
    Data = cmd(P,Spec,3000),
    size(Data);
test(P,snapshot) ->
    {ok, Data} = cmd(P,disp_data,infinity),
    file:write_file("/tmp/scope.bmp", Data);
test(_,Spec) ->
    throw({?MODULE,bad_test_spec,Spec}).


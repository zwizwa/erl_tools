-module(rigol).
-export([start_link/1, handle/2,
         run_prim/3,
         test/1,
         cmd/3]).

%% https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf

%% Notes:

%% - Use TCP in passive mode.  Operation is mostly RPC with some
%%   ad-hoc encoding that is easier to do sequentially.
%%
%% - The SCPI implementation doesn't seem to be very robust, or at
%%   least I have no idea how to do proper flow control, so:
%%
%%   - Delays are added for commands that do not have an ack.
%%
%%   - Retries and consistency checks are added as the scope seems to
%%     just abort a command without any explanation.
%%
%% - Sampling rate can't be set directly.  It is set indirectly by
%%   setting the time div:
%%
%%   500ms/div => 2MHz, 12Mpts, 6000ms


start_link(#{ tcp := {_Host,_Port} } = Spec) ->
    {ok,
     serv:start(
       {handler,
        fun() -> self() ! connect, Spec end,
        fun ?MODULE:handle/2})}.

handle(connect, State=#{tcp := {Host,Port}}) ->
    case maps:find(sock, State) of
        {ok, _} ->
            State;
        error ->
            log:info("connect ~s:~p~n", [Host,Port]),
            case gen_tcp:connect(
                   Host,Port,
                   [{active,false},{packet,raw},binary]) of
                {ok, Sock} ->
                    log:info("connected: ~p~n", [{Host,Port}]),
                    maps:put(sock, Sock, State);
                Error ->
                    throw(Error)
            end
    end;
handle({Pid, {run, Spec}}, State) ->
    {Rv, State1} = ?MODULE:run_prim(Spec, State, 5),
    obj:reply(Pid, Rv),
    State1;
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    log:info("WARNING: ~p~n", [Msg]),
    State.

%% This thing is buggy.  Keep retrying on error.
run_prim(Spec, _, 0) ->
    throw({rigol_run_prim, Spec});
run_prim(Spec, State=#{sock := Sock}, Retry) ->
    Write =
        fun(IOList) ->
                gen_tcp:send(Sock, IOList),
                %% FIXME: Is there proper flow control?
                DelayMS = 200,  
                timer:sleep(DelayMS)
        end,
    Read =
        fun(N) ->
                read(Sock,N) 
        end,
    try 
        log:info("~p~n", [Spec]),
        Rv = prim(Write, Read, Spec),
        {Rv, State}
    catch _C:_E ->
            log:info("errror: ~p~n",[{_C,_E}]),
            Read(flush),
            run_prim(Spec, State, Retry-1)
    end.
    
%% Primitive commands communicate with the device through write and
%% read operations on the socket.  These primitives are treated as
%% transaction, and get retried on error.  It seems simplest to keep
%% them generic, based on the replies that are expected.

%% Generic operations.
prim(W,R,{tmc,Query}) ->
    W([Query,"\n"]),
    R(tmc);
prim(W,R,{tmc_line,Query}) ->
    W([Query,"\n"]),
    Data = R(tmc),
    _ = R(line),
    Data;
prim(W,R,{tmc,Query,ExpectedSize}) ->
    Data = prim(W,R,{tmc,Query}),
    true = size(Data) == ExpectedSize,
    Data;
prim(W,R,{line,Query}) ->
    W([Query,"\n"]),
    R(line);
prim(W,R,{number,Query}) ->
    W([Query,"\n"]),
    to_number(R(line));
prim(W,_,Cmd) ->
    W([Cmd,"\n"]).


%% user API

%% Useful commands:

%% *IDN?
%% *IOPC
%% *RST
%% *:ACQ:MDEP?
%% :WAV:STAT?
%% :ACQ:MDEP?
%% :ACQ:SRAT?
%% :RUN
%% :STOP
%% :TRIG:MODE?
%% :TRIG::EDG:LEV?
%% :TRIG::EDG:SLOP?
%% :TIM:MAIN:SCAL?


%% Composite commands should be defined outside of the retry loop,
%% e.g. loading several chunks.
cmd(Pid, {save_wav_data,Chan,NbPoints,FileName}, TO) ->
    Cmd = fun(C) -> cmd(Pid, C, TO) end,
    Cmd([":STOP"]),
    Cmd([":WAV:SOUR CHAN",p(Chan)]),
    Cmd([":WAV:MODE RAW"]),
    Cmd([":WAV:FORM BYTE"]),
    %% ChunkSize = 240000,
    ChunkSize = 1000000,
    Chunks =
        lists:map(
          fun({Start,N}) ->
                  %% Cmd([":STOP"]),
                  Cmd([":WAV:STAR ",p(Start+1)]),
                  Cmd([":WAV:STOP ",p(Start+N)]),
                  Cmd({tmc,"WAV:DATA?",N})
          end,
          tools:nchunks(0,NbPoints,ChunkSize)),
    file:write_file(FileName, Chunks);

cmd(Pid, wav_pre, TO) ->
    Reply = cmd(Pid, {line, ":WAV:PRE?"}, TO),
    maps:from_list(
      lists:zip(
        [format,type,points,count,
         xincrement,xorigin,xreference,
         yincrement,yorigin,yreference],
        lists:map(
          fun to_number/1,
          re:split(Reply,","))));

cmd(Pid, c8_test, TO) ->
    lists:foreach(
      fun(Cmd) -> cmd(Pid, Cmd, TO) end,
      [[":STOP"],
       [":ACQ:TYPE AVER"],
       [":TIM:MAIN:SCAL ",p(0.0005)],
       [":TRIG:MODE ","EDGE"],
       [":TRIG:EDG:SLOP ","NEG"],
       [":TRIG:EDG:LEV ",p(1.5)],
       [":TRIG:EDG:SOUR ","CHAN1"],
       [":RUN"]]);

cmd(Pid, disp_data, TO) ->
    cmd(Pid, {tmc_line, ":DISP:DATA?"}, TO);

cmd(Pid, {save_disp_data, FileName}, TO) ->
    Data = cmd(Pid, disp_data, TO),
    file:write_file(FileName, Data);

%% Primitive operation.
cmd(Pid, Spec, Timeout) ->
    obj:call(Pid, {run, Spec}, Timeout).



%% Receiver has some special purpose parsers.  Read errors translate
%% to pattern matching errors.
read(Sock, line) ->
    case read(Sock, 1) of
        <<"\n">> -> [];
        <<Char>> -> [Char | read(Sock, line)]
    end;
read(Sock, tmc_blockheader) ->
    case read(Sock, 1) of
        <<"#">> ->
            <<CNumSize>> = read(Sock, 1), 
            Num = CNumSize - $0,
            SNumData = read(Sock, Num),
            binary_to_integer(SNumData);
        _ ->
            %% Skip junk
            read(Sock, tmc_blockheader)
    end;
read(Sock, tmc) ->
    NumData = read(Sock, tmc_blockheader),
    log:info("tmc ~p~n", [NumData]),
    read(Sock, NumData);
read(_Sock, 0) ->
    <<>>;
read(Sock, flush) ->
    {ok, Data} = gen_tcp:recv(Sock, 0),
    Data;
read(Sock, N) when is_integer(N) ->
    {ok, Data} = gen_tcp:recv(Sock, N),
    Data.



%% misc tools
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


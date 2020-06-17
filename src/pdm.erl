-module(pdm).
-export([init/2, handle/2, measure/2, measure_range/4, find_freq/2]).

%% Driver for uc_tools/gdb/pdm.c
init(Pid, _PacketProto) ->
    log:info("pdm:init/2~n"),
    Pid ! {set_forward, fun ?MODULE:handle/2},
    ok.

%% Be careful for cycles here.
super(Msg,State) ->
    gdbstub_hub:dev_handle(Msg, State).

handle(info, State) ->
    super({send_u32,[103]}, State);

handle({setpoint, Channel, FloatVal}, State) ->
    IntVal = round(FloatVal * 16#100000000),
    Clipped = 
        if
            IntVal >= 16#100000000 -> 16#FFFFFFFF;
            IntVal < 0 -> 0;
            true  -> IntVal
        end,
    super({send_u32,[101,Channel,Clipped]}, State);

handle({epid_send,Dst,Data}=Msg, State) ->
    case Dst of
        midi ->
            lists:foldl(
              fun(Midi,S) ->
                      case Midi of
                          {cc,0,P,V} ->
                              midi_cc(P,V,S);
                          _->
                              S
                      end
              end,
              State,
              Data);
        _ ->
            log:info("pdm: unknown: ~p~n",[Msg]),
            State
    end;

%% FIXME: First I want tag_u32 with binary payload and abstract
%% continuation.  Put that in the library.

handle({Pid, {measure, LogMax}}, State) ->
    %% FIrmware places continuation in a queue to reply when the
    %% measurement is done.
    super({Pid, {call_u32, [102, LogMax]}}, State);

handle(Msg, State) ->
    %% log:info("pdm: passing on: ~p~n",[Msg]),
    gdbstub_hub:default_handle_packet(Msg, State).



midi_cc(P,V,State) ->
    log:info("~p~n",[{P,V}]),
    case P of
        22 ->
            Frac = 0.5 - 0.0019 * (V - 64.0),
            handle({setpoint, 0, Frac}, State);
        _ ->
            ok
    end,
    State.


%% The low level interface is a little quirky.  The routine below

%% Specify the log of the max number of CPU cycles to measure.  The
%% result has the decimal point shifted by that amount.  1<<26 CPU
%% cycles is about a second.
measure(Pid,LogMax) ->
    true = LogMax < 32,
    true = LogMax >= 0,
    FracBits = 32 - LogMax,

    %% Such that Period / Div is in seconds, Div / Period is in Hz.
    Div = 72000000.0 * (1 bsl FracBits),
    
    case obj:call(Pid, {measure,LogMax}, 3000) of
        <<Period:32,_Count:32>> -> Div / Period
    end.


%% Crashes when period becomes too high, probably not triggering any
%% measurements.
%% pdm:measure_range(pdm, 0,5, 0,0001, 3).
measure_range(Pid, Start, Inc, N) ->
    LogMax = 24,  %% 0.25sec
    lists:map(
      fun(I) ->
              Frac = Start + Inc * I,
              Pid ! {setpoint, 0, Frac},
              _ = measure(Pid, LogMax), %% Throw away transition
              Rv = {Frac,measure(Pid, LogMax)},
              log:info("~p\n", [Rv]),
              Rv
      end,
      lists:seq(0,N-1)).


%% Find the set point that gives a particular frequency using
%% successive linear approximation of the setpoint -> log(freq) curve.
%%
%% pdm:find_freq(pdm, 110, 2).

ln(X) -> math:log(X).


%% Two or three iterations seems to be enough for most frequencies.
%% Set it to 4 for an extra step.  Note that the initial two points
%% are very important: too low and the frequency measurement might
%% time out.  Pick them very close to the mid range of the converter.

find_freq(Pid, Hz) ->
    N = 4,
    Inits = [0.49, 0.51],
    LogMax = 24, %% 0.25 sec
    find_freq(Pid, Hz, LogMax, Inits, N).

find_freq(Pid, Hz, LogMax, Inits, N) ->
    Measure =
        fun(Frac) ->
                Pid ! {setpoint, 0, Frac},
                %% Throw away transition
                _ = measure(Pid, LogMax),
                M = measure(Pid, LogMax),
                {Frac, ln(M), M}
        end,
    [XYA,XYB] = lists:map(Measure, Inits),
    Approx = find_freq_it(Pid, Measure, ln(Hz), XYA, XYB, N),
    {XE,YE,EYE} = lists:last(Approx),
    #{setpoint => XE, 
      log_hz => YE,
      hz => EYE, 
      approx => Approx}.
    

find_freq_it(_, _, _, XYA, XYB, 0) -> [XYA, XYB];              
find_freq_it(Pid, Measure, YT, {XA,YA,_}=XYA, {XB,YB,_}=XYB, N) ->
    XC = XA + (YT-YA) * ((XB-XA) / (YB-YA)),
    XYC = Measure(XC),
    [XYA | find_freq_it(Pid, Measure, YT, XYB, XYC, N-1)].
    
    

    

-module(slip).
-export([encode/1, decode/1,
         start/2, handle/2,
         ping/1
        ]).

%% Simplified gdbstub/slip uC interface.

%% Packetization for Erlang is usually done using size prefixes.  This
%% works well when the byte stream can't get out of sync.

%% Serial port connections do not really have a clear starting point,
%% so SLIP is used as a means to be able to recover from an unknown
%% receiver state.

%% At the system level we already need a couple of different packets.
%% The convention is to use the high range for system packets, and the
%% low range for applciation.

-include("slip.hrl").

start(Host,TTY) ->
    serv:start(
      {handler,
       fun() ->
               log:info("tty_start: ~s:~s~n",[Host,TTY]),
               Port = exo:open_ssh_port(Host, "gdbstub_connect", TTY, []),
               #{ port => Port, buf => <<>> }
       end,
       fun ?MODULE:handle/2}).
handle({send, Data}, #{ port := Port} = State) ->
    log:info("slip: send: ~p~n", [Data]),
    port_command(Port, Data),
    State;
handle({send_packet, Packet}, State) ->
    Enc = encode(Packet),
    handle({send, Enc}, State);
handle({Port, {data, Msg}}, #{ port := Port, buf := Buf } = State) ->
    Data = iolist_to_binary([Msg,Buf]),
    case decode(Data) of
        {ok, Dec, Buf1} ->
            State1 = maps:put(buf, Buf1,  State), 
            Dispatch = maps:get(dispatch, State, fun dispatch/2),
            State2 = Dispatch(Dec, State1),
            handle({Port, {data, <<>>}}, State2);
        {more,_} ->
            maps:put(buf, Data, State)
    end;
%% FIXME: Clean this up, but general idea would be great to have.
handle({Pid, {call, Packet}}, State) ->
    {Wait, State1} = wait(Pid, State),
    %% log:info("wait: ~p~n",[{Wait,Pid}]),
    Ack = term_to_binary(Wait),
    Enc = encode([Packet,size(Ack),Ack]),
    handle({send, Enc}, State1);
    
handle(Msg, State) ->
    obj:handle(Msg, State).


dispatch(<<?TAG_REPLY:16,L,Ack/binary>>=Msg, State) ->
    log:info("ack: ~p~n",[Ack]),
    try
        Wait = binary_to_term(Ack, [safe]),
        {Pid, State1} = unwait(Wait, State),
        Rpl = binary:part(Ack, L, size(Ack)-L),
        obj:reply(Pid, Rpl),
        State1
    catch _:_ ->
            %% This case is for acks that are generated outside of
            %% the {call,Packet} mechanism above.
            log:info("bad ack in TAG_REPLY message: ~p~n",[Msg]),
            State
    end;
dispatch(<<Tag:16,Data/binary>>, State) ->
    %% log:info("unknown: Tag=~p Data=~p~n",[Tag,Data]),
    %% Keep track of things that are not handled here.
    maps:put({last,Tag}, Data, State);

dispatch(<<>>, State) ->
    %% Ignore empty messages.  This is a consequence of dual-ended
    %% SLIP encoding.
    State;

dispatch(Msg, State) ->
    log:info("malformed: ~p~n",[Msg]),
    State.

%% Simple registry for pending requests.
wait(Term, State) ->
    wait(Term, State, 0).
wait(Term, State, N) ->
    case maps:find({wait, N}, State) of
        {ok,_} -> wait(Term, State, N+1);
        _ -> {N, maps:put({wait, N}, Term, State)}
    end.
unwait(N, State) ->
    {maps:get({wait, N}, State),
     maps:remove({wait, N}, State)}.
            




%% Export encode/decode as well.

encode(IOList) ->
    Bin = iolist_to_binary(IOList),     %% log:info("Bin ~p~n",[Bin]),
    Lst = binary_to_list(Bin),          %% log:info("List ~p~n",[Lst]),
    IOList1 = [192,slip_body(Lst),192], %% log:info("IOList1 ~p~n",[IOList1]),
    Bin1 = iolist_to_binary(IOList1),   %% log:info("Bin1 ~p~n",[Bin1]),
    Bin1.

slip_body([]) -> [];
slip_body([192|Tail])  -> [219,220|slip_body(Tail)];
slip_body([219|Tail])  -> [219,221|slip_body(Tail)];
slip_body([Head|Tail]) -> [Head|slip_body(Tail)].
    
                
decode(Bin) ->
    slip_decode(binary_to_list(Bin),[]).
slip_decode([192|Rest],    Stack) ->
    {ok, list_to_binary(lists:reverse(Stack)), list_to_binary(Rest)};
slip_decode([219,220|Rest],Stack) -> slip_decode(Rest, [192|Stack]);
slip_decode([219,221|Rest],Stack) -> slip_decode(Rest, [219|Stack]);
slip_decode([219,_|_],_)          -> error(slip_decode);
slip_decode([219],         _)     -> {more, undefined};
slip_decode([],            _)     -> {more, undefined};
slip_decode([Char|Rest],   Stack) -> slip_decode(Rest, [Char|Stack]).



%% High level calls
ping(Pid) -> <<>> = obj:call(Pid, {call,<<16#FFFC:16>>}, 3000).

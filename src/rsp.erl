-module(rsp).
-export([wrap/1,
         unwrap/1,
         qRcmd/1,
         send/2,
         hex_csv/3,
         assemble/1,
         recv/1,
         watch/2,
         gather/2
        ]).

-import(tools,[info/1, info/2, unhex/1, hex/1]).

%% GDB RSP protocol tools

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to rsp.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


%% FIX: We're not sending acks.  Stub ignores those.

unesc([])          -> [];
unesc([$}|[H| T]]) -> [H|unesc(T)];
unesc([H|T])       -> [H|unesc(T)].

esc([])       -> [];
esc([$# | T]) -> "}#" ++ esc(T);
esc([$$ | T]) -> "}$" ++ esc(T);
esc([$} | T]) -> "}}" ++ esc(T);
esc([$* | T]) -> "}*" ++ esc(T);
esc([H  | T]) -> [ H | esc(T) ].

chk(L) -> chk(L,0) band 255.
chk([],A)    -> A;
chk([H|T],A) -> chk(T,A+H).

wrap(L) ->
    W = esc(L),
    "+$" ++ W ++ "#" ++ hex([chk(W)]).

chop_end([$#|[_|_]]) -> []; %% ignore checksum - this went through USB CRC.
chop_end([H|T])      -> [H|chop_end(T)].

chop([$+|T]) -> chop(T);    %% ignore acks
chop([$-|T]) -> chop(T);    %% fail on nacks FIXME
chop([$$|T]) -> chop_end(T).

unwrap(L) ->
    %% info("unwrap:~p~n",[L]),
    unesc(chop(L)).




% Return true if packet ends in delimiter.
%% delim(Bin) when is_binary(Bin) ->
%%     N = size(Bin),
%%     if
%%         binary_at(Bin, N-3) == $# -> true;  %% "*#??"
%%         binary_at(Bin, N-1) == $+ -> true;  %% "*+"
%%         true -> false
%%     end.


% Return true if packet ends in delimiter.
delim ("+") -> true;
%%delim ("-") -> true;  %% ???
delim ([$# | [_ | [ _ ]]]) -> true;
delim ([_ | T]) -> delim(T);
delim (_) -> false.

% Is it a remote command?  If so, unpack it.
qRcmd("+") -> false;
qRcmd(Request) ->
    case rsp:unwrap(Request) of
        [$q,$R,$c,$m,$d,$, | Hex] -> unhex(Hex);
        _ -> false
    end.



comma([]) -> "";
comma([A|B]) -> A ++ "," ++ comma(B).

%% Create a RSP command, separating arguments by commas.  This is not
%% strictly the GDB RSP protocol as some commands use different
%% separators, but smstub is liberal in what it accepts: any non-hex
%% character works.
hex_csv(Code, Args, Payload) ->
    HexArgs = [integer_to_list(A,16) || A <- Args],
    rsp:wrap(Code ++ comma(HexArgs) ++ Payload).







%% The code below is a little convoluted, using several interfaces to
%% do essentially the same thing: assemble RSP packets and send them
%% on.

%% Concatenate chunks until a full packet is received.
%% Core for assemble/1, and recv/1

%% This is a hack, relies on correct TCP packet borders which seems to
%% works in practice but will break if borders are not respected.
gather(Data, Accu) ->
    NextAccu = Accu++tools:as_list(Data),
    case delim(NextAccu) of
        true -> {{ok, NextAccu}, ""};
        _ -> {busy, NextAccu}
    end.
    
singleshot({In, Out}=Env, Accu) ->
    Data = In(),
    {Result,NextAccu} = gather(Data, Accu),
    case Result of
        {ok, Msg} -> Out(Msg);
        _ -> singleshot(Env, NextAccu)
    end.

singleshot(Env) ->
    singleshot(Env, "").

loop({In,Out}=Env) ->
    singleshot({In,fun(Msg) -> Out(Msg), loop(Env) end}).


%% Process body for separate assembler task.
assemble(Receiver) ->
    loop({fun() -> receive {rsp_chunk, Data} -> Data end end,
          fun(Data) -> Receiver ! {rsp_recv, Data} end}).




%% Synchronous send/receive.
send(Sock, Request) ->
    case gen_tcp:send(Sock, Request) of
        ok -> ok;
        {error, Reason} -> exit(Reason)
    end.
recv(Sock) ->
    singleshot(
      {fun() -> case gen_tcp:recv(Sock, 0) of
                    {ok, Data} -> binary_to_list(Data);
                    {error, Error} -> exit(Error)
                end
       end,
       fun(Data) -> Data end}).

%% Run this in a linked process.  It blocks in read, to also trap
%% connection close and terminating the device process tree.
watch(Dev, Sock) ->
    Reply = rsp:recv(Sock),
    Dev ! {rsp_recv, Reply},
    watch(Dev, Sock).




    




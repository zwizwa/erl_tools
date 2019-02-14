%% (c) 2018 Tom Schouten -- see LICENSE file

-module(rsp).
-export([wrap/1,
         unwrap/1,
         qRcmd/1,
         send/2,
         hex_csv/3,
         assemble/1,
         recv/1, recv_port/2,
         watch/2,
         gather/2,
         update/2,
         esc/1, unesc/1
        ]).

-import(tools,[info/1, info/2, unhex/1, hex/1]).

%% GDB RSP protocol tools

%% FIX: We're not sending acks.  Stub ignores those.

unesc([])          -> [];
unesc([$}|[H| T]]) -> [H|unesc(T)];
unesc([H|T])       -> [H|unesc(T)].


esc([H|T]) ->
    case lists:member(H,"#}$*") of
        true -> [$}, 16#20 bxor H | esc(T)];
        false -> [H | esc(T)]
    end;
esc([]) -> [].

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
%% work in practice but will break if borders are not respected.
gather(Data, Accu) ->
    NextAccu = Accu++tools:as_list(Data),
    case delim(NextAccu) of
        true -> {{ok, NextAccu}, ""};
        _ -> {busy, NextAccu}
    end.
    
singleshot({GetChunk, Out}=Env, Accu) ->
    Data = GetChunk(),
    {Result,NextAccu} = gather(Data, Accu),
    case Result of
        {ok, Msg} -> Out(Msg);
        busy -> singleshot(Env, NextAccu)
    end.

singleshot(Env) ->
    singleshot(Env, "").

loop({GetChunk,Out}=Env) ->
    singleshot(
      {GetChunk,
       fun(Msg) -> 
               {_,_} = Out(Msg),
               loop(Env)
       end}).

                 
        

%% Receive chunks from current mailbox and send them out to a separate
%% process.
assemble(Receiver) ->
    loop(
      {fun() -> receive {rsp_chunk, Data} -> Data end end,
       fun(Data) -> Receiver ! {rsp_recv, Data} end}).




%% Synchronous send/receive to socket.
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

%% Similar, but for a port.
recv_port(Port, Timeout) ->
    singleshot(
      {fun() -> 
               receive {Port, {data, Data}} -> Data
               after Timeout -> exit({timeout, Timeout})
               end 
       end,
       fun(Data) -> Data end}).
                 
       


%% Run this in a linked process.  It blocks in read, to also trap
%% connection close and terminating the device process tree.
watch(Dev, Sock) ->
    Reply = rsp:recv(Sock),
    Dev ! {rsp_recv, Reply},
    watch(Dev, Sock).



-spec update(_,_) -> no_return().
update(_,_) -> exit(rsp_update_stub).
    




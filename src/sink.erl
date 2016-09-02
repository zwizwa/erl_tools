%% Sink - parameterized Generators

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to sink.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


-module(sink).
-export([gen_to_list/1,gen_tcp/1,print/1,map/2,
         buffer/0, buffer/2, no_eof/1, file/1,

         %% Misc processors
         line_assembler/3
        ]).


%% Sinks are callbacks that do not return any value.

%% A Generator is a function that abstracts a sequence by iterating
%% over the elements and passing them to a function named Sink:
%%
%% - Sink({data,Data}) for each element of the stream
%% - Sink(eof)         at the end of the stream

%% This can be thought of as complimentary to fold.erl 
%% The combination of both, and processes, yields input/output behavior.

%% note that sink-parameterized generators and folds are equivalent in
%% that they can be converted automatically from one into the other.
%% The decision to use either is purely one of arbitrary convenience
%% in representing the sequence at
%%
%% - the sending end:   the sink is abstracts "!"
%% - the receiving end: the fold abstracts a loop over "receive"


map(Fun, Sink) -> fun(Msg) -> Sink(map_msg(Msg, Fun)) end.
map_msg({data, Data}, Fun) -> {data, Fun(Data)};
map_msg(eof,_) -> eof.
    
                          
    


%% Note that a combination of fold:generator and fold:to_list can't be
%% used if the generator makes calls to a port process, as that can
%% only be done in-proces.  Here we call the generator in-process.
gather(Pid, Stack) ->
    receive
        eof          -> Pid ! {self(), lists:reverse(Stack)};
        {data, Data} -> gather(Pid, [Data|Stack]);
        Msg          -> exit({sink_gather,Msg})
    end.
gen_to_list(Gen) ->
    Self = self(),
    Pid = spawn_link(fun () -> gather(Self, []) end),
    Gen(fun(Msg) -> Pid ! Msg end),  
    receive {Pid, List} -> List end.



%% Similar to gather, but not synchronized to eof.  Keeps running
%% after being flushed.
buffer() ->
    Pid = spawn_link(fun() -> buffer_loop([]) end),
    {Pid, fun(Msg) -> Pid ! Msg end}.
buffer(Pid,list) ->
    lists:reverse(buffer(Pid,stack));
buffer(Name,Cmd) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> exit({undefined,Name});
        Pid -> buffer(Pid,Cmd)
    end;
buffer(Pid,Cmd) when is_pid(Pid) ->
    Self = self(),
    Pid ! {Self, Cmd},
    receive {Pid, List} -> List
    after 1000 -> timeout
    end.
buffer_loop(Stack) ->
    receive
        eof          -> buffer_loop(Stack);
        {data, Data} -> buffer_loop([Data|Stack]);
        {Pid, stack} -> Pid ! {self(), Stack}, buffer_loop(Stack);
        {Pid, flush} -> Pid ! {self(), ok},    buffer_loop([]);
        Msg          -> exit({unknown,Msg})
    end.
    

%% Data sinks.
gen_tcp(Sock) ->
    fun(Msg) -> 
            case Msg of
                {data, Data} -> gen_tcp:send(Sock, Data);
                _ -> ok
            end
    end.

print(eof) -> ok;
print({data,Data}) -> tools:info("~p~n",[Data]).


file(Filename) ->
    fun(Msg) ->
            case Msg of
                {data, Data} -> file:write_file(Filename, Data, [append]);
                _ -> ok
            end
    end.


%% Sink-aparameterized processors are functions oparating on (Input,
%% State), and passing data to Sink.

%% This abstraction is useful when input and output "clocks" are not
%% the same: 0 or 1 inputs may yield 0 or more output events.


%% Feed it binary chunks.  Complete lines are passed to Sink.
line_assembler(BinInput, Last, Sink) ->
    [First | Rest] = binary:split(BinInput, <<"\n">>, [global]),
    {Lines, Next} = tools:pop_tail([[Last, First] | Rest]),
    lists:foreach(fun(Line) -> Sink(Line) end, Lines),
    Next.
    


%% When stitching together sink-parameterized generators, make sure to
%% strip the eof messages.  The convention is that a single generator
%% will produce a single eof at the end.
no_eof(_, eof) -> ignore;
no_eof(S, M) -> S(M).
no_eof(S) -> fun(M) -> no_eof(S, M) end.


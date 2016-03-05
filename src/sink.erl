%% Sink - parameterized Generators

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to sink.hrl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


-module(sink).
-export([gen_to_list/1,gen_tcp/1,print/1,map/2,
         buffer/0, buffer/2]).


%% Sinks are callbacks that do not return any value.

%% A Generator is a function that abstracts a sequence by iterating
%% over the elements and passing them to a function named Sink:
%%
%% - Sink({data,Data}) for each element of the stream
%% - Sink(eof)         at the end of the stream

%% This can be thought of as complimentary to fold.erl The combination
%% of both, and processes, yields input/output behavior.

%% Use sink.erl when more control is needed over the time at which an
%% action is taken.  Use folds when the idea is to construct sequences
%% and later evaluate them.


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
        Msg          -> exit({unknown,Msg})
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
buffer(Pid,Cmd) ->
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

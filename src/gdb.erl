-module(gdb).
-export([open/5         %% Start GDB through TCP GDB RSP, providing a Port handler
        ,open_os_pid/4  %% Start GDB attaching to host Pid
        ,upload/5       %% Start GDB, upload file, exit
        ,cmd_sink/3     %% Send command, forwared responses to sink. 
        ,cmd/3          %% Send command, return formatted response. 
        ,set/4          %% Set a variable or struct member
        ,send/2, sync/2 %% For blocking interaction
        ]).

         
%% GDB Machine Interface (MI).
%%
%% Interact with GDB from erlang.
%%
%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to gdb.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


open_os_pid(GdbMi, OsPid, Elf, Sink) ->
    P = open_port({spawn, GdbMi}, [{line, 1024}, use_stdio]),
    sync(P, Sink),
    cmd_sink(P, tools:format("file ~s", [Elf]), Sink),
    cmd_sink(P, tools:format("attach ~p", [OsPid]), Sink),
    P.

open(GdbMi, TargetHost, TargetPort, Elf, Sink) ->
    P = open_port({spawn, GdbMi}, [{line, 1024}, use_stdio]),
    sync(P, Sink),
    cmd_sink(P, tools:format("file ~s", [Elf]), Sink),
    cmd_sink(P, tools:format("target remote ~s:~p", [TargetHost, TargetPort]), Sink),
    P.

%% Ask connected dev node to push image here.
%% Assumes host name is set up correctly so dev node can find us, as
%% GDB uses plain TCP to connect.
upload({pull, DevNode}, Gdb, TargetPort, Elf, Sink) ->
    {ok, TargetHost} = inet:gethostname(),
    rpc:call(DevNode, gdb, upload, [TargetHost, Gdb, TargetPort, Elf, Sink]);

%% Upload ELF through gdb -i=mi
upload(TargetHost, Gdb, TargetPort, Elf, Sink) ->
    P = open(Gdb, TargetHost, TargetPort, Elf, Sink),
    Rv = cmd_sink(P, "load", Sink),
    send(P, "quit"),
    Rv.

%% Send command, send results to sink, and wait for (gdb) prompt.
cmd_sink(Port, Cmd, Sink) ->
    send(Port, Cmd),
    sync(Port, Sink),
    ok.

send(Port, Cmd) ->
    %%tools:info("send: ~s~n", [Cmd]),
    Port ! {self(), {command, Cmd++"\n"}}.

sync(Port,Sink) ->
    receive
        {Port, {data, {eol, [$(,$g,$d,$b,$)|_]}}} ->
            Sink(eof);
        {Port, {data, {eol, Line}}} ->
            Sink({data, handle(Line)}),
            sync(Port,Sink);
        {Port, Anything} -> 
            Sink({error, Anything}),
            Sink(eof)
    after
        2000 -> {error, timeout}
    end.

%% https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html#GDB_002fMI-Output-Syntax
handle(Line) ->
    case Line of
        [$^|S] -> {result,S};
        [$+|S] -> {status,S};
        [$&|S] -> {log,S};
        [$*|S] -> {exec,S};
        [$=|S] -> {notify,S};
        [$~|S] -> {console,S};
        Other ->  {other, Other}
    end.


%% non-sink version of cmd_sink/3.
cmd(Port, Cmd, list) ->  
    sink:gen_to_list(fun(Sink) -> gdb:cmd_sink(Port, Cmd, Sink) end);
cmd(Port, Cmd, console) ->
    cmd_console(Port, Cmd);
cmd(Port, Cmd, result) ->
    hd(tools:filter_tag(result,cmd(Port, Cmd, list))).





%% GDB mi access is useful for poking at firmware images from Erlang,
%% without having to expose a specific interface to do so.  Note that
%% getting information out of the image is best done with direct GDB
%% RSP calls; this here is only one-directionl.  I.e. this only
%% "sends" a message.  Rely on firmware mechanism to produce a response.

%% Note: A previous attempt to parse the expressions printed by gdb
%% failed due to lack of simple structure in gdb's output (i.e. it is
%% real work to make that work properly..).


%% Path is a list of atoms, and is mapped to a (nested) structure
%% member reference address or a single value.
set(P, Path, Val, Sink) when is_list(Path) ->
    Dotted = string:join(lists:map(fun atom_to_list/1, Path),"."),
    Cmd = tools:format("set ~s = ~p", [Dotted, Val]),
    %%tools:info("gdb:set ~p~n", [Cmd]),
    cmd(P,Cmd,Sink);
set(P, Var, Val, Sink) when is_atom(Var) ->
    set(P, [Var], Val, Sink).



%% Run command, return IO list of console output.  Note that using the
%% erlang scanner is a dirty hack, but seems to work.  Don't use this
%% in production code!
cmd_console(P, Cmd) ->
    tools:filter_tag(ok,[console_clean(Msg) || Msg<-gdb:cmd(P,Cmd,list)]).
console_clean({console, QuotedString}) ->
    {ok,[{string,_,String}],_} = erl_scan:string(QuotedString), {ok, String};
console_clean(Other) -> {error, Other}.

            





  

%% Very Ad-hoc.  Works fine for integers, but doesn't work for
%% multi-line results.
%% unpack_binding(Str) ->
%%     case erl_scan:string(Str) of
%%         {ok,[{string,1,[$$|Binding]}],1} ->
%%             case re:split(Binding, <<" = ">>) of
%%                 [Var, Val] -> {ok, Var, Val};
%%                 _ -> error
%%             end;
%%         _ -> error
%%     end.
            

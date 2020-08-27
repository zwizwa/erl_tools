%% (c) 2018 Tom Schouten -- see LICENSE file

-module(gdb).
-export([open/5         %% Start GDB through TCP GDB RSP, providing a Port handler
        ,open_os_pid/4  %% Start GDB attaching to host Pid
        ,upload/5       %% Start GDB, upload file, exit
        ,cmd_sink/3     %% Send command, forwared responses to sink. 
        ,cmd/3          %% Send command, return formatted response. 
        ,set/4          %% Set a variable or struct member
        ,send/2, sync/2 %% For blocking interaction
        ,msg_get/2      %% Retreive values from status messages (ad-hoc)
        ,msg_proplist/1 %% Hack, gathers all bindings
        ,msg_parse/1    %% Proper parser
        ,open_mi/1      %% Low level
        ]).

         
%% GDB Machine Interface (MI).
%%
%% Interact with GDB from erlang.

%% open_port/2 with proper arguments.  Expects script that starts gdb
%% with -i=mi argument.

open_mi(GdbMi) ->
    open_port({spawn, GdbMi}, [{line, 1024}, use_stdio]).

open_os_pid(GdbMi, OsPid, Elf, Sink) ->
    P = open_mi(GdbMi),
    sync(P, Sink),
    cmd_sink(P, tools:format("file ~s", [Elf]), Sink),
    cmd_sink(P, tools:format("attach ~p", [OsPid]), Sink),
    P.

open(GdbMi, TargetHost, TargetPort, Elf, Sink) ->
    P = open_mi(GdbMi),
    sync(P, Sink),
    cmd_sink_file(P, Elf, Sink),
    cmd_sink(P, tools:format("target remote ~s:~p", [TargetHost, TargetPort]), Sink),
    P.

cmd_sink_file(_, none, _)   -> ok;
cmd_sink_file(P, Elf, Sink) -> cmd_sink(P, tools:format("file ~s", [Elf]), Sink).
    

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
    _Rv = cmd_sink(P, "compare-sections", Sink),
    quit(P, Sink),
    Rv.

%% Quit gdb, ensuring it has actually quit.
quit(P, Sink) ->
    Ref = erlang:monitor(port, P),
    ok = send(P, "quit"),
    sync(P, Sink),
    receive 
        {'DOWN',Ref,port,_,_}=_Msg ->
            %% log:info("~p~n",[_Msg]),
            ok
    end,
    ok.


%% Send command, send results to sink, and wait for (gdb) prompt.
cmd_sink(Port, Cmd, Sink) ->
    ok = send(Port, Cmd),
    sync(Port, Sink),
    ok.

send(Port, Cmd) ->
    %%tools:info("send: ~s~n", [Cmd]),
    Port ! {self(), {command, Cmd++"\n"}}, ok.

sync(Port,Sink) ->
    receive
        {Port, {data, {eol, [$(,$g,$d,$b,$)|_]}}} ->
            Sink(eof);
        {Port, {data, {eol, Line}}} ->
            Sink({data, untag(Line)}),
            sync(Port,Sink);
        {Port, Anything} -> 
            Sink({error, Anything}),
            Sink(eof)
    after
        2000 -> {error, timeout}
    end.

%% https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html#GDB_002fMI-Output-Syntax
untag(Line) ->
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

            
%% There is currently no real parser, but since the structures are
%% fixed it is possible to use regular expressions to fish out what is
%% needed.
msg_get({status,Str},Tag) when is_binary(Tag) ->
    Re = tools:format("download.*~s=\"(.*?)\"",[Tag]),
    case re:run(Str, Re, [{capture,all_but_first,binary}]) of
        {match,[Val]} -> {ok, Val};
        Other -> {error, Other}
    end.

%% (gw@10.1.1.81)15> re:run(S,"download.*section-sent=\"(.*?)\"",[{capture,all,list}]).

  

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
            


test() -> "download,{section=\".text\",section-sent=\"1440\",section-size=\"34032\",total-sent=\"1676\",total-size=\"625300\"}".
    
tok_fold(Str) ->
    parse:bimodal_tokenize(
      #{ %% Used by tokenizer
         $"  => quote,
         $\\ => escape,
         %% Left in output stream
         ${  => open,
         $}  => close,
         $,  => comma,
         $=  => equal,
         %% Escaped characters
         {escape, $r} => 13,
         {escape, $n} => 10
       },
      source:from_list(Str)).
    
msg_proplist(test) ->
    msg_proplist(test());

%% A simple hack to avoid a real parser, using just a bimodal
%% tokenizer.  We don't care about the nesting of the data structure,
%% but only want the key,value bindings, so just scan for
%% [{atom,K},equal,{atom,V}] in the token stream.
msg_proplist(Str) ->
    Fold = tok_fold(Str),
    T = fun(X) -> list_to_atom(X) end,
    {_,_,Rv} =
        Fold(
          fun({atom,V},{equal,{atom,K},L}) -> {x,x,[{T(K),V}|L]};
             (C,{B,_,L})                   -> {C,B,L} %% shift delay line
          end,
          {x,x,[]}),
    Rv.


%% But a good enough parser isn't actually that difficult.  Work with
%% concrete lists as lazy lists are hard to express in Erlang.
msg_parse(test) ->
    msg_parse(test());
msg_parse({tok,Tokens}) ->
    p(Tokens, [], []);
msg_parse(Str) ->
    msg_parse({tok,fold:to_list(tok_fold(Str))}).


r(Q) -> lists:reverse(Q).
     
    
%% I: input
%% Q: current queue
%% S: stack of queues
p([{atom,""}|I], Q,          S)               -> p(I, Q,              S);  %% (1)
p([open     |I], Q,          S)               -> p(I, [],             [Q|S]);
p([close    |I], Q1,         [[{eq,K}|Q2]|S]) -> p(I, [{K,r(Q1)}|Q2], S);
p([close    |I], Q1,         [Q2|S])          -> p(I, [r(Q1)|Q2],     S);
p([{atom,V} |I], [{eq,K}|Q], S)               -> p(I, [{K,V}|Q],      S);
p([{atom,A} |I], Q,          S)               -> p(I, [A|Q],          S);
p([equal    |I], [K|Q],      S)               -> p(I, [{eq,K}|Q],     S);
p([comma    |I], Q,          S)               -> p(I, Q,              S);  %% (2)
p([],            Q,          [])              -> r(Q);
    
p(Input, Queue, Stack) -> error({parse,Input,Queue,Stack}).

%% (1) Tokenizer leaves empty atoms inbetween other tokens.  This is
%% convenient for some things, and easy to filter out here.

%% (2) FIXME: shortcut. this will not catch some bad syntax but is
%% good enough in case we know the input is well-formed.  We only need
%% it during tokenization.

%% Note that {Q,S} is a representation of the continuation.  S is
%% always a stack of Qs, but there are two kinds of Qs:
%% - list     the hole at the end of a list
%% - {eq,K}   the hole in the second slot of the pair



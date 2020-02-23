-module(run).
-export([%% interactive
         session/2, script/2,
         %% batch
         script_line/1,
         script_print/2,
         port_print/2,
         port_cons/2,
         fold_port/4,
         fold_script/5,
         script_lines/2,
         script_output/2,
         script_xml/2, 

         to_script/2,
         bash/3,

         %% runner
         runner_start/1,
         runner_handle/2,
         default_log/1,

         %% For reloads
         session_receive/2
]).

%% Running external (interactive) command line tools using a CPS
%% interface.  Waits for prompt.
session(Cmd,Prompt) ->
    Port = open_port({spawn, Cmd}, [{line, 1024}, use_stdio]),
    Env = #{ port => Port,
             tag  => Cmd,
             prompt => Prompt },
    fun(Data) ->
            port_command(Port, Data),
            run:session_receive(Env, [])
    end.


%% Empty prompt means to ignore all replies.  Log them to console.
session_receive(Env=#{prompt := "", tag := Tag}, Stack) ->
    receive
        _Msg ->
            log:info("~s: ~p~n",[Tag, _Msg]),
            run:session_receive(Env, Stack)
    after
        0 -> 
             {ok, ""}
    end;

session_receive(Env=#{port := Port, prompt := Prompt, tag := Tag}, Stack) ->
    receive
        {Port, {data, {eol, Prompt}}} ->
            %% Return iolist of response and a continuation.
            {ok, lists:reverse(Stack)};
        {Port, {data, {eol, Data}}} ->
            log:info("~s: ~p~n", [Tag, Data]),
            run:session_receive(Env, [Data|Stack]);
        {Port, Anything} ->
            {error, Anything}
    after
        2000 -> {error, timeout}
    end.


%% Use run_session interface to do one-shot.
script(Cmd,Prompt) ->
    Continue = session(Cmd, Prompt),
    Continue([]).







%% Running non-interactive scripts with more control than os:cmd.


script_line(Cmd) ->
    Port = open_port({spawn, Cmd},
                     [{line, 1024}, use_stdio, exit_status]),
    receive
        {Port, {data, {eol, Elf}}} -> Elf
    end.

%% For fold_script
port_print({data, {eol, Data}},_) -> io:format("~s~n", [Data]), {cont, none};
port_print({exit_status, Stat},_) -> io:format("exit ~p~n",[Stat]), {done, Stat}.

port_cons({data, {eol, Data}}, L) -> {cont, [Data|L]}; %% line mode, e.g. opts=(#(line 1024))
port_cons({data, Data},        L) -> {cont, [Data|L]}; %% chunk/packet mode, e.g. opts=()
port_cons({exit_status, 0},    L) -> {done, L};
port_cons({exit_status, _}=E,  L) -> {error, {E, L}}.

fold_port(Port, Fun, State, Timeout) ->
    receive
        {Port, Data} ->
            {Cmd, Arg} = Fun(Data, State),
            case Cmd of
                cont -> fold_port(Port, Fun, Arg, Timeout);
                done -> {ok, Arg};
                error -> {error, Arg}
            end
    after 
        Timeout -> {error, timeout}
    end.

fold_script(Cmd, Fun, State, Timeout, Opts) ->
    DefaultOpts = [exit_status, use_stdio],
    Port = open_port({spawn, Cmd}, DefaultOpts ++ Opts),
    link(Port),
    fold_port(Port, Fun, State, Timeout).

%% List of lines
script_lines(Cmd, Timeout) ->
    case fold_script(Cmd, fun port_cons/2, [], Timeout, [{line, 1024}]) of
        {ok, List} -> {ok, lists:reverse(List)};
        {error, {E, List}} -> {error, {E, lists:reverse(List)}}
    end.
%% Full output string, flattened.
script_output(Cmd, Timeout) ->
    case fold_script(Cmd, fun port_cons/2, [], Timeout, []) of
        {ok, List} -> {ok, lists:flatten(lists:reverse(List))};
        {error, {E, List}} -> {error, {E, lists:flatten(lists:reverse(List))}}
    end.

%% Full output as parsed xml
script_xml(Cmd, Timeout) ->
    case script_output(Cmd,Timeout) of
        {ok, String} -> {ok, xmerl_scan:string(String)};
        E -> E
    end.

%% Print output
script_print(Cmd, Timeout) ->
    fold_script(Cmd, fun port_print/2, none, Timeout, [{line, 1024}]).




%% Print to output
to_script(Cmd, Out) ->
    Port = open_port({spawn,Cmd},[]),
    port_command(Port, Out),
    port_close(Port).


%% See redo.erl
bash(Dir, Cmds, Log) ->
    %% log:info("run: bash: ~s~n", [Cmds]),
    Log(clear),
    Log({line,Cmds}),
    run:fold_script(
      tools:format("bash -c 'cd ~s ; ~s'", [Dir, Cmds]), 
      fun({data,{eol,Line}}, Lines) ->
              Log({line,Line}),
              {cont, [Line ++ "\n"|Lines]};
         ({exit_status, ExitCode},Lines) ->
              {done, {ExitCode, lists:flatten(lists:reverse(Lines))}}
      end,
      [],
      infinity,
      [{line, 1024}]).


%% A place to run script sequentially.
runner_start(Spec) ->
    {ok, serv:start(
           {handler,
            fun() ->
                    Spec 
            end,
            fun ?MODULE:runner_handle/2})}.
runner_handle(Msg, State) ->
    case Msg of
        {bash, Dir, Cmds, Log} ->
            _ = bash(Dir, Cmds, Log),
            State;
        {bash, Dir, Cmds} ->
            _ = bash(Dir, Cmds, fun ?MODULE:default_log/1),
            State
    end.

default_log({line,Line}) ->
    log:info("~s~n", [Line]);
default_log(_) ->
    ok.

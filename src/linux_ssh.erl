-module(linux_ssh).
-export([start_link/1, handle/2, putx_cmd/2]).

%% Given ssh access, this code takes care of:
%%
%% - Event monitoring by watching the messages log
%% - Remote port program execution
%% - Binary code deployment
%%
%% Useful when it's not practically possible to run a remote Erlang
%% node.

start_link(#{ host := Host }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                %% Note Host can be user@host also
                log:set_info_name({?MODULE, Host}),
                Cmd = tools:format(
                        "ssh ~s tail -n0 -f /tmp/messages",
                        [Host]),
                log:info("Cmd = ~s~n", [Cmd]),
                Opts = [use_stdio, exit_status, binary, {line, 1024}],
                LogPort = open_port({spawn, Cmd}, Opts),
                maps:merge(
                  Config,
                  #{ log => LogPort})
        end,
        fun ?MODULE:handle/2})}.
handle(Msg, State = #{log := LogPort, host := Host}) ->
    case Msg of
        %% Parse log message
        {LogPort, PMsg} ->
            case PMsg of
                {data, {eol, Line}} -> 
                    %% log:info("log: ~p~n", [Line]),
                    case gdbstub_hub:parse_syslog_ttyACM(Line) of
                        {ok, TTY} -> log:info("TTY ~p~n", [TTY]);
                        _ -> ok
                    end,
                    State
                end;
        %% Ack from one-shot commands
        {CmdPort, PMsg} when is_port(CmdPort) ->
            case {PMsg, maps:find({cmd, CmdPort}, State)} of
                {closed, {ok, Pid}} ->
                    obj:reply(Pid, ok),
                    State
            end;
        %% Upload a binary
        {Pid, {putx, Path, Bin}} ->
            Opts = [use_stdio, exit_status, binary],
            Cmd = putx_cmd(Host,Path),
            log:info("Cmd=~s",[Cmd]),
            Port = open_port({spawn, Cmd}, Opts),
            Port ! {self(), {command, Bin}},
            Port ! {self(), close},
            maps:put({cmd,Port}, Pid, State);
        %% Start a remote port program.
        {Pid, {open_port, {spawn, Cmd0}, Opts}} ->
            Cmd = tools:format("ssh ~p '~p'",[Cmd0]),
            Port = open_port({sapwn, Cmd}, Opts),
            Port ! {self(), {connect, Pid}},
            obj:reply(Pid, {ok, Port}),
            State;
        {_, dump} ->
            obj:handle(Msg, State);
        _ ->
            log:info("WARNING: ~p~n", [Msg])
    end.

putx_cmd(Host, Path) ->
    Cmd1 = tools:format(
             "P=~p;"
             "mkdir -p $(dirname \"$P\");"
             "rm -f \"$P\";"
             "cat >\"$P\";"
             "chmod +x \"$P\"",
             [Path]),
    Cmd = tools:format("ssh ~s ~s", [Host, run:quote(Cmd1)]),
    Cmd.

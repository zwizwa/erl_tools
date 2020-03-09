-module(ssh_leaf).
-export([start_link/1, handle/2, putx_cmd/2]).

%% Given ssh access, this code takes care of:
%%
%% - read:  Event monitoring by watching the messages log
%% - exec:  Remote port program execution
%% - write: Binary code deployment
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
    Notify = maps:get(notify, State, fun(_)->ok end),
    case Msg of
        %% Parse log message
        {LogPort, PMsg} ->
            case PMsg of
                {data, {eol, Line}} -> 
                    %% log:info("log: ~p~n", [Line]),
                    case gdbstub_hub:parse_syslog_ttyACM(Line) of
                        {ok, {USB, ACM}} -> 
                            SpawnSpec = #{
                              proxy => self(),
                              host => Host %% only for logging
                             },
                            Notify({tty,SpawnSpec,USB,ACM});
                        _ ->
                            ok
                    end,
                    State
                end;
        %% Ack from one-shot commands
        {CmdPort, PMsg} when is_port(CmdPort) ->
            case {PMsg, maps:find({cmd, CmdPort}, State)} of
                %% OK if we receive closed before exit_status
                {closed, {ok, {Pid, Info}}} ->
                    log:info("done: ~p~n", [Info]),
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
            Info = {putx, Path},
            maps:put({cmd,Port}, {Pid, Info}, State);
        %% Start a remote port program.
        {Pid, {spawn_port, #{ cmd := Cmd, args := Args, opts := Opts }=_SpawnSpecs}} ->
            log:info("linux_ssh:spawn_port ~p~n", [_SpawnSpecs]),
            %% Use canonical location and extension.
            Elf = tools:format("bin/~s.elf",[Cmd]),
            ShellCommand = run:shell_command(Elf, Args),
            SshCmd = tools:format("ssh ~s '~s'", [Host, ShellCommand]),
            log:info("SshCmd = ~s~n", [SshCmd]),
            Port = open_port({spawn, SshCmd}, Opts),
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
             "P=~s;"
             "mkdir -p $(dirname \"$P\");"
             "rm -f \"$P\";"
             "cat >\"$P\";"
             "chmod +x \"$P\"",
             [Path]),
    Cmd = tools:format("ssh ~s ~s", [Host, run:shell_quote(Cmd1)]),
    Cmd.

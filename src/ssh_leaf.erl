-module(ssh_leaf).
-export([start_link/1, handle/2, cmd/2]).

%% Given ssh access, this code takes care of:
%%
%% - read:  Event monitoring by watching the messages log
%% - exec:  Remote port program execution
%% - write: Binary code deployment, rsync
%%
%% Useful when it's not practically possible to run a remote Erlang
%% node.

%% EDIT: This is turning out to be quite useful, and compared to
%% previous convoluted approaches, it seems to abstract in the correct
%% way: keep it simple and just call 'ssh'.  Let any authentication be
%% handled in ssh config on a per host basis.  Any sharing can then
%% still be implemented at that config level.


start_link(#{ host := Host }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                %% Note Host can be user@host also
                log:set_info_name({?MODULE, Host}),
                Cmd = cmd(Config, {ssh, "tail -n0 -f /tmp/messages"}),
                %% log:info("Cmd = ~s~n", [Cmd]),
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
            Cmd = cmd(State,{putx,Path}),
            %% log:info("Cmd=~s",[Cmd]),
            Port = open_port({spawn, Cmd}, Opts),
            Port ! {self(), {command, Bin}},
            Port ! {self(), close},
            Info = {putx, Path},
            maps:put({cmd,Port}, {Pid, Info}, State);

        %% Rsync a tree.  The SrcPath is on the local node.
        {Pid, {rsync, SrcPath, DstPath}=Info} ->
            Opts = [use_stdio, exit_status, binary],
            Cmd = cmd(State,{rsync,SrcPath,DstPath}),
            log:info("Cmd=~s",[Cmd]),
            Port = open_port({spawn, Cmd}, Opts),
            Port ! {self(), close},
            maps:put({cmd,Port}, {Pid, Info}, State);

        %% Start a remote port program using the spawn_port spec
        %% format.  Note that transferring ownership seems to not
        %% always work. The issue is sidestepped: whe have the
        %% knowledge about how to open a port, and will just pass that
        %% on so the caller can call open_port/2 instead.
        {Pid, {open_port_cmd, #{ cmd := Cmd, args := Args }=_SpawnSpecs}} ->
            %% log:info("ssh_leaf:spawn_port ~p~n", [_SpawnSpecs]),
            Elf =
                case maps:find(cmd_to_elf, State) of
                    {ok, ElfFun} ->
                        %% Allow user to override
                        ElfFun(Cmd);
                    error ->
                        %% Use canonical location and extension.
                        tools:format("bin/~s.elf", [Cmd])
                end,
            ShellCommand = run:shell_command(Elf, Args),
            SshCmd = tools:format("ssh ~s '~s'", [Host, ShellCommand]),
            obj:reply(Pid, {ok, SshCmd}),
            State;

        {_, dump} ->
            obj:handle(Msg, State);

        _ ->
            log:info("WARNING: ~p~n", [Msg])
    end.

%% Command formatter
cmd(#{host := Host}, {ssh, Cmd}) ->
    tools:format("ssh ~s ~s", [Host, q(Cmd)]);
cmd(Env, {putx, Path}) ->
    cmd(
      Env,
      {ssh,
       tools:format(
         "P=~s;"
         "mkdir -p $(dirname \"$P\");"
         "rm -f \"$P\";"
         "cat >\"$P\";"
         "chmod +x \"$P\"",
         [Path])});
cmd(#{host := Host}, {rsync, SrcPath, DstPath}) ->
    Cmd = tools:format(
            "rsync "
            "--archive "
            "--delete "
            "--one-file-system "
            "--rsh=ssh "
            "~s/ ~s:~s/",
            [q(SrcPath), Host, q(DstPath)]),
    Cmd;
cmd(Env, Spec) ->
    throw({?MODULE,cmd,Env,Spec}).

q(IOList) ->
    run:shell_quote(IOList).

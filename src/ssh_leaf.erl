-module(ssh_leaf).
-export([start_link/1, handle/2, cmd/2, handle_log_reply/2]).

%% TODO:
%% - Allow on-demand with off state.

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
                self() ! connect,
                Config
        end,
        fun ?MODULE:handle/2})}.

%% All the other handlers can go in a single case.    
handle(Msg, State = #{host := Host}) ->
    case Msg of
        %% Connect log port
        connect ->
            log:info("connecting~n"),
            %% Note: This will NOT close when stdin closes!
            %% CmdLine = "exec tail -n0 -f /tmp/messages",
            CmdLine = "exec socat - EXEC:'tail -n0 -f /tmp/messages'",
            Cmd = cmd(#{host => Host}, {ssh, CmdLine}),
            log:info("Cmd = ~s~n", [Cmd]),
            Opts = [use_stdio, exit_status, binary, {line, 1024}],
            Port = open_port({spawn, Cmd}, Opts),
            maps:put({log,Port}, #{handle => fun ?MODULE:handle_log_reply/2}, State);

        %% Port replies
        {Port, PMsg} when is_port(Port) ->
            case maps:find({log, Port}, State) of
                %% Logs have abstract handlers.
                {ok, #{handle := Handle}} ->
                    Handle(Msg, State);
                %% Ack from one-shot commands
                error ->
                    case {PMsg, maps:find({cmd, Port}, State)} of
                        {{exit_status, Status}, {ok, {Pid, Info}}} ->
                            %% log:info("done: ~p~n", [{PMsg,Info}]),
                            log:info("done: ~999p~n", [Info]),
                            Rv = case Status of
                                     0 -> ok;
                                     _ -> {error, {status, Status}}
                                 end,
                            obj:reply(Pid, Rv),
                            State;
                        _ ->
                            log:info("unexpeced port message: ~999p~n", [Msg]),
                            State
                    end
                end;
                
        %% Rsync a tree.  The SrcPath is on the local node.

        %% FIXME: Increase banner timeout?  Or find a way to retry?
        %% Having trouble on trans-atlantic vpn link...  Now catching
        %% the error so it doesn't fail silently.
        {Pid, {rsync, SrcPath, DstPath}=Info} ->
            Opts = [use_stdio, exit_status, binary],
            Cmd = cmd(State,{rsync,SrcPath,DstPath}),
            %% log:info("Cmd=~s",[Cmd]),
            Port = open_port({spawn, Cmd}, Opts),
            maps:put({cmd,Port}, {Pid, Info}, State);

        %% Ensure a remote directory exists.  This should be part of
        %% rsync, but I can't immediately see how to cover all cases.
        {Pid, {mkdirp, Dir}=Info} ->
            Opts = [use_stdio, exit_status, binary],
            Cmd = cmd(State,{mkdirp,Dir}),
            %% log:info("Cmd=~s",[Cmd]),
            Port = open_port({spawn, Cmd}, Opts),
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
            log:info("WARNING: ~p~n", [Msg]),
            State
    end.

handle_log_reply(Msg={Port, PMsg}, State = #{host := Host}) ->
    %% Parse log message.
    case PMsg of
        {data, {eol, Line}} -> 
            %% log:info("log: ~999p~n", [Line]),
            case gdbstub_hub:parse_syslog_ttyACM(Line) of
                {ok, {USB, ACM}} -> 
                    SpawnSpec = #{
                      proxy => self(),
                      host => Host %% only for logging
                     },
                    Notify = maps:get(notify, State, fun(_)->ok end),
                    Notify({tty,SpawnSpec,USB,ACM});
                _ ->
                    ok
            end,
            State;
        {exit_status,_} ->
            log:info("~p~n",[Msg]),
            self() ! connect,
            maps:remove(log, State)
    end.

%% FIXME: This needs to handle logrotate.




%% Command formatter
cmd(#{host := Host}, {ssh, Cmd}) ->
    tools:format("ssh ~s ~s", [Host, q(Cmd)]);
cmd(#{host := Host}, {rsync, SrcPath, DstPath}) ->
    Cmd = tools:format(
            "rsync "
            "--archive "
            "--delete "
            "--one-file-system "
            "--rsh=ssh "
            "~s ~s:~s",
            [q(SrcPath), Host, q(DstPath)]),
    Cmd;
cmd(#{host := Host}, {mkdirp, Dir}) ->
    Cmd = tools:format(
            "ssh ~s mkdir -p ~s",
            [Host, q(Dir)]),
    Cmd;
cmd(Env, Spec) ->
    throw({?MODULE,cmd,Env,Spec}).

q(IOList) ->
    run:shell_quote(IOList).

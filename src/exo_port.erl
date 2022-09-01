%% EXO-SPECIFIC MECHANISM

%% Dead code removed before moving exo -> erl_tools after exo
%% dbce63b2607070abb919346e22c6c1912b7466fb

%% It is hard to find a good abstraction here.  So let's hold off on
%% standardizing it.  For now this has a number of special cases in
%% spawn_port/1, and a "resolve" method that produces the Env.
%%
%% For future API, these are a couple of simplifications.  There are
%% essentially only 2 cases:
%%
%% 1. If the remote is not an Erlang node, a port program runs through
%%    ssh.  In this case, we do not manage authentication in exo.  The
%%    ssh_config is used to set up authentication such that we can
%%    just use a hostname.  It is probably best to use ssh_leaf.erl in
%%    this case.
%%
%% 2. A program runs on an Erlang node.  In this case, the erlang
%%    driver process should always run on the remote node directly.
%%
%% In both these cases it is probably best to standardize the build
%% and deployment paths to /i/exo.  Incremental development makes it
%% difficult to use "packaged" target trees.  Using a partial mirror
%% of the build tree, it is still possible to copy or use NFS exports,
%% or use the CAS.
%%
%%
%% The two old cases are not good defaults:
%%
%% - The ~/bin should really just be a partial mirror of the build
%%   directory, i.e. /i/exo/ There is no point in adding an
%%   indirection here.
%%
%% - The exo ssh mechanism is too hard to configure.  Use a broader
%%   ssh account with full access.  The old mechanism is documented in
%%   exo/erl/apps/exo/c_src/port_test.c
%%
%%
%%
%% GENERIC CODE AND NAME RESOLUTION
%%
%% There are not many decisions that need to be made, but it is
%% important to render them explicit, and they should be made by the
%% frameworl (exo) such that the underlying code can be kept simple.
%%
%% The goal is to start a driver + port program combo, and abstract
%% the process such that the default can be "get binary from priv
%% dir".  I really do not use the latter, but let's include it for
%% compatibility with the rest of the ecosystem.
%%
%% The allocator determines:
%%
%% 1. where the port process will run
%% 2. where the driver will run
%% 3. what their relationship is
%%
%% It follows a these rules:
%%
%% - Port processes will always run on the (Linux) machine that has
%%   the hardware.
%%
%% - The Erlang driver runs on the same machines as the port driver if
%%   possible, otherwise it will run on some other (arbitrary) node,
%%   connected with SSH.
%%
%% Additionally, it will map the basename of an executable to its full
%% name (path + extension), and probably should allow some wrapper
%% around the startup, e.g. to add environment variables.
%%
%% So what does resolve do?  It takes a high level specification, and
%% turns it into a closure that needs to be called to open the port.
%% The most flexible interface we currently have is the proxy call
%% in spawn_port/1.
%%
%% This already separates the two concerns:
%%
%% 1. RESOLVE: turn a spec into a concrete command
%% 2. INSTANTIATE: call spawn_port/1 to instantiate the port.
%%
%% The way we implement this is to explicitly define the strategy for
%% every port command in the system, and then gradually standardize
%% from there.
%%



-module(exo_port).
-export([spawn_port/3, spawn_port/2, spawn_port/1, resolve/1]).


%% resolve/1 translates user spec to something that is specific to
%% exo.  Hence we can dispatch on "cmd".  The caller only needs to
%% provide opts, cmd, and args, and can assume that we take care of
%% the rest.

resolve(EnvSpec) ->

    %% Assert minimal config
    #{ opts := _, cmd := CmdIOL } = EnvSpec,
    Cmd = iolist_to_binary(CmdIOL),

    %% Dispatch on command first.  These variables will override.
    ExtraEnv = 
        case Cmd of
            <<"logan">> ->
                #{ dir => "/i/exo/logan" };
            _ ->
                #{}
        end,
    
    %% Compose
    maps:merge(
      %% Defaults will not override
      #{ env => [], args => [],
         app => exo, user => exo, host => localhost },
      maps:merge(
        EnvSpec,
        %% These will override EnvSpec
        ExtraEnv)).



%% Backwards compatibility: FIXME: Remove
spawn_port(Env0, {Cmd, ArgList}=Spec, Opts) ->
    log:info("FIXME: deprecated: spawn_port/3: ~p~n", [{Env0,Spec,Opts}]),
    spawn_port(
      maps:merge(
        Env0,
        #{ cmd => Cmd,
           args => ArgList,
           opts => Opts })).

%% For reloadable closure.
spawn_port(Env0, Env1) ->
    spawn_port(maps:merge(Env0, Env1)).


%% Main entry point.
spawn_port(Env0) ->

    %% First perform some name resolution, as clients are not aware of
    %% "framework" details.

    Env = resolve(Env0),

    %% Then dispatch on a number of special cases.  This needs to be
    %% cleaned up.

    case Env of

        %% If a proxy is specified, we delegate the resolution to an
        %% external process and then locally spawn the port.
        #{ proxy := Pid,
           opts  := Opts } ->
            {ok, SpawnCmd} = obj:call(Pid, {open_port_cmd, Env}),
            log:info("SpawnCmd = ~s~n", [SpawnCmd]),
            Port = open_port_wrap({spawn, SpawnCmd}, Opts),
            Port;

        %% If directory is specified, the command is actual and we
        %% will spawn a local port.
        #{ dir  := Dir,
           cmd  := Cmd,
           opts := Opts,
           args := ArgList } ->
            CmdLine = tools:format("~s/~s", [Dir, run:shell_command(Cmd, ArgList)]),
            log:info("CmdLine: ~s~n", [CmdLine]),
            open_port_wrap({spawn, CmdLine}, Opts);

        %% The exo_ssh mechanism, e.g. one SSH key per program for
        %% finegrained access control.  This should probably be phased
        %% out.  It is very quirky.
        #{ user := exo=User,
           host := Host0,
           cmd  := Cmd,
           args := Args,
           opts := Opts
         } when is_atom(User) ->
            Host = resolve_host(Host0),
            %% FIXME: if there is an Erlang connection, use that?
            UserAtHost = tools:format("~p@~s", [User, Host]),
            CmdLine = exo_ssh_cmd(UserAtHost, Cmd, Args),
            log:info("CmdLine: ~s~n", [CmdLine]),
            open_port_wrap({spawn, CmdLine}, Opts);

        %% Ordinary ssh.
        #{ user := User,
           host := Host0,
           cmd  := Cmd,
           args := Args,
           opts := Opts
         } when is_atom(User) ->
            ShellCommand = run:shell_command(Cmd, Args),
            SshCmd = tools:format("ssh ~s '~s'", [Host0, ShellCommand]),
            log:info("SshCmd: ~s~n", [SshCmd]),
            open_port_wrap({spawn, SshCmd}, Opts);

        %% If app is specified, the directory is taken from the app's priv dir.
        #{ app  := App,
           cmd  := Cmd,
           args := Args,
           opts := Opts
           } ->
            Dir = code:priv_dir(App),
            CmdLine = tools:format("~s/~s", [Dir, run:shell_command(Cmd, Args)]),
            log:info("CmdLine: ~s~n", [CmdLine]),
            open_port_wrap({spawn, CmdLine}, Opts)

    end.

%% FIXME: We should not be called by anythong other than a hostname.
%% At this time ther are still callers that do their own resolution.
resolve_host(Host) when is_atom(Host) ->
    tools:format("~p", [Host]);
resolve_host(Host) ->
    Host.

exo_ssh_cmd(UserAtHost, Service, ArgList) ->
    ExoSsh = code:priv_dir(exo) ++ "/ssh/exo_ssh.sh",
    tools:format("~s ~s ~s", [ExoSsh, UserAtHost,
                              run:shell_command(Service, ArgList)]).


%% Wrapper around Erlang's open_port/2, mainly for logging
open_port_wrap(PortName, Opts) ->
    log:info("exo_port: open_port(~999p,~999p)~n", [PortName, Opts]),
    open_port(PortName, Opts).





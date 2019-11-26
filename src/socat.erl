-module(socat). %% FIXME: This outgrew its name.

-export([pty/1,tcp_listen/1,
         erlcat/2, erlcat_port/1, erlcat_port_handle/2,
         test/0]).

%% 1) PROCESS SPEC

%% A server process isn't necessary.  Just provide open_port shortcuts.
%% This flattening is not really necessary.
pty(Spec)        -> open_spec({pty,Spec}).
tcp_listen(Spec) -> open_spec({tcp_listen,Spec}).


open_spec(Spec) ->
    Cmd = cmd(Spec),
    log:info("cmd: ~s~n",[Cmd]),
    open_port({spawn, Cmd}, [use_stdio, binary, exit_status]).

%% One-ended socat mfa
socat(Fmt,Args) ->
    fmt("socat - " ++ Fmt, Args).
fmt(Fmt, Args) ->
    tools:format(Fmt, Args).

%% Allow for a specification language to name some typical setyps.
%% This doesn't need to be all socat, but it is necessary that
%% programs exit when stdin closes, which is not always the case with
%% everything!
cmd(Spec) ->
    case Spec of
        {pty, Link}          -> socat("PTY,link=~s,echo=0,raw", [Link]);
        {tcp_listen, Port}   -> socat("TCP-LISTEN:~p,reuseaddr", [Port]);
        {read_file, File}    -> fmt("cat '~s'",[File]);
        {write_file, File}   -> fmt("cat >'~s'",[File]);
        {dd_if, File}        -> fmt("dd 'if=~s' 2>/dev/null",[File]);
        {dd_of, File}        -> fmt("dd 'of=~s' 2>/dev/null",[File]);
        {pipe, [S]}          -> cmd(S);
        {pipe, [S|Ss]}       -> fmt("~s | ~s", [cmd(S), cmd({pipe, Ss})]);
        {cmd, Cmd}           -> Cmd
    end.


%% 2) INTER-NODE BRIDGE     

%% Bridge two bi-directional ports on separate nodes.  Run until one
%% of them exits.  Basically, socat, but transporting between Erlang
%% processes.
erlcat(SrcSpec, DstSpec) ->
    Open =
        fun({Node, Spec}) ->
                case rpc:call(
                       Node, ?MODULE, erlcat_port,
                       [#{ spec => Spec }]) of
                    Pid when is_pid(Pid) ->
                        {Pid, erlang:monitor(process, Pid)};
                    Error ->
                        throw({error, Error})
                end
        end,
    try
        {Src, MonSrc} = Open(SrcSpec),
        {Dst, MonDst} = Open(DstSpec),
        Src ! {connect, Dst},
        Dst ! {connect, Src},

        %% Wait until both are finished.  They send messages to each
        %% other to ensure teardown.
        receive {'DOWN', _, _, Src, SrcReason} -> ok end,
        receive {'DOWN', _, _, Dst, DstReason} -> ok end,
        erlang:demonitor(MonSrc),
        erlang:demonitor(MonDst),
        case Reasons = {SrcReason,DstReason} of
            {normal, normal} -> ok;
            _ -> throw({error, Reasons})
        end
    catch C:E ->
            {error, {C,E}}
    end.
        
            
%% It appears that the Pid associated to a port needs to be a local
%% process.  I don't find this in the manual, but I do get 'badarg'
%% errors for remote pids on any port operations.  So just sidestep
%% the problem and put all the logic in a wrapper process, one for
%% each port.

erlcat_port(#{ spec := _ } = Init) ->
    serv:start(
      {handler,
       fun() -> Init end,
       fun ?MODULE:erlcat_port_handle/2}).

erlcat_port_handle({connect, Other}, State = #{spec := Spec} ) ->
    %% Hold off opening port until we have something to forward to.
    Port = open_spec(Spec),
    maps:merge(
      State,
      #{ other => Other,
         port  => Port });

erlcat_port_handle(Msg, State = #{port := Port, other := Other}) ->
    case Msg of
        {_,dump} ->
            obj:handle(Msg,State);
        {to_port, PortMsg} ->
            Port ! {self(), PortMsg},
            State;
        {Port, PortMsg} when is_port(Port) ->
            case PortMsg of
                {data, Data} ->
                    Other ! {to_port, {command, Data}},
                    State;
                {exit_status, Status}=E ->
                    Other ! {to_port, close},
                    case Status of
                        0 -> exit(normal);
                        _ -> exit(E)
                    end;
                closed ->
                    exit(normal)
                end;
        _ ->
            exit({erlcat_port_handle,Msg})
    end.


test() ->    
    erlcat(
      {'exo@10.1.3.29', {read_file, "/tmp/test"}},
      {'exo@10.1.3.20', {pipe, [{cmd,"lz4c"}, {write_file, "/tmp/test"}]}}).



%% 3) EPID

%% Note that this looks almost like epids, but the proxy processes
%% here are nodes.

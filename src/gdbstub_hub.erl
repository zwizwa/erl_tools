-module(gdbstub_hub).
-export([start_link/0,send/1,call/1,
         dev/1,
         %% Some high level calls
         info/1,
         %% Attach gdb-mi
         gdb_mi/1,

         %% Internal
         dev_start/1,
         hub_handle/2, dev_handle/2, devpath_port/1
]).

%% This module is a hub for uc_tools gdbstub-based devices.  See also
%% gdbstub.erl

%% Singal flow:
%% - gdbstub_hub board gets enumerated on some host
%% - hosts's udev config connects to exo_notify
%% - gdbstub_hub hub gets an 'add' message
%% - a process is started for the particular device
%% - this process starts a GDB server process for GDBRSP over TCP
%% - the server supports multiple connections

%% See /etc/net/udev/notify-tty.sh which currently delegates to
%% zoe:/etc/net/udev/tty/zoe_usb_9-2.sh

%% The script sends a line to the exo_notify daemon:
%% bluepoll add zoe /dev/ttyACM1 /devices/pci0000:00/0000:00:16.0/usb9/9-2/9-2.4/9-2.4:1.0/tty/ttyACM1

%% The host name + devpath is enough to uniquely identify the location
%% of the device.

start_link() ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                process_flag(trap_exit, true),
                register(gdbstub_hub, self()),
                #{}
        end,
        fun gdbstub_hub:hub_handle/2})}.

%% Udev events will eventuall propagate to here.

%% DevPath is used to uniquely identify the device, based on the
%% physical USB port location.
hub_handle({add,Host,TTYDev,DevPath}=_Msg, State)
  when is_atom(Host) and is_binary(TTYDev) ->
    log:info("~p~n", [_Msg]),
    {ok, UsbPort} = devpath_port(DevPath),
    ID = {Host,UsbPort},
    case maps:find(ID, State) of
        {ok, Pid} ->
            log:info("already have ~p~n", [{ID,Pid}]),
            State;
        _ ->
            %% Easier to decouple GDB communication if there is a
            %% dedicated process per device.
            Hub = self(),
            Pid = gdbstub_hub:dev_start(
                    #{ hub => Hub,
                       %% log => fun(_) -> ok end,
                       log => fun(Msg) -> log:info("~p~n",[Msg]) end,
                       host => Host,
                       tty => TTYDev,
                       devpath => DevPath,
                       tcp_port => 1234, %% FIXME: alloc
                       id => ID }),
            log:info("adding ~p~n", [{ID,Pid}]),
            maps:put(ID, Pid, State)
    end;

hub_handle({'EXIT',Pid,_Reason}=_Msg,State) ->
    log:info("~p~n", [_Msg]),
    IState = tools:maps_inverse(State),
    case maps:find(Pid, IState) of
        {ok, ID} ->
            maps:remove(ID, State);
        _ ->
            log:info("Warning: ~p not registered~n", [Pid]),
            State
    end;

hub_handle({Pid, {dev_pid, ID}}, State) ->
    obj:reply(Pid, maps:find(ID, State)),
    State;
                          
hub_handle(Msg, State) ->
    obj:handle(Msg, State).


%% The main purpose of this process is to provide mutually exclusive
%% access to the GDB port.
        
dev_start(#{ tty := Dev, id := {Host, _} } = Init) ->      
    serv:start(
      {handler,
       fun() ->
               Port = exo:open_ssh_port(Host, "gdbstub_connect", Dev, []),
               Gdb = gdb_start(maps:merge(Init, #{ pid => self() })),
               maps:merge(
                 Init,
                 #{ gdb => Gdb, port => Port })
       end,
       fun gdbstub_hub:dev_handle/2}).


dev_handle(Msg={_,dump},State) ->
    obj:handle(Msg, State);

dev_handle({Pid, {rsp_call, Request}}, 
           #{ port := Port } = State) ->
    true = port_command(Port, Request),
    obj:reply(
      Pid,
      case Request of
          "+" -> "";
          _   -> rsp:recv_port(Port, 3000)
      end),
    State;

dev_handle({Port, Msg}, #{ port := Port} = _State) ->
    %% All {data,_} messages should arrive in the receive above.
    log:info("exit or bad protocol: ~p~n",[Msg]),
    exit(Msg).






%% GDB RSP server.

gdb_start(#{ tcp_port := TCPPort } = Init) ->
    serv:start(
      {handler,
       fun() ->
               log:info("GDB remote access on TCP port ~p~n",[TCPPort]),
               serv_tcp:init(
                 [TCPPort], 
                 %% loop/2 uses blocking code (rsp:recv/1)
                 {body, 
                  fun(Sock, _) -> 
                          log:info("new connection~n"),
                          gdb_loop(maps:put(sock, Sock, Init))
                  end})
       end,
       fun serv_tcp:handle/2}).

%% GDB session is coupled to name, not to device instance.  This allows
%% device restarts while keeping gdb conn open.
gdb_loop(State = #{ sock := Sock, log := Log }) ->
    Request = rsp:recv(Sock),
    Log({request,Request}),
    case gdb_dispatch(State, Request) of
        "" -> ignore;
        Reply ->
            Log({reply, Reply}),
            ok = rsp:send(Sock, Reply)
    end,
    gdb_loop(State).

gdb_dispatch(#{ pid := Pid}, Request) ->
    obj:call(Pid, {rsp_call, Request}).

%%devpath_port(test) ->
%%    devpath_port(
%%      <<"/devices/pci0000:00/0000:00:16.0/usb9/9-2/9-2.4/9-2.4:1.0/tty/ttyACM1\n">>);
devpath_port(Bin) ->
    case lists:reverse(re:split(Bin,"/")) of
        [_ttyACMx,<<"tty">>,_,UsbPort|_] ->
            case re:split(UsbPort,"-") of
                [Interface,Chain] ->
                    ChainList = re:split(Chain,"\\."),
                    {ok, [binary_to_integer(C) || C <- [Interface | ChainList]]};
                _ -> error
            end;
        _ -> error
    end.

%% It might be convenient. But maybe best not expose a naked Erlang
%% console on a TCP port without any form of authentication.

%% gdb_dispatch(#{ pid := Pid}, Request) ->
%%     %% By default, Send the the GDB command to the device.
%%     Forward = fun() -> obj:call(Pid, {rsp_call, Request}) end,

%%     %% Except when it is a monitor command...
%%     case rsp:qRcmd(Request) of
%%         false -> Forward();
%%         "" -> Forward();
%%         Cmd ->
%%             case lists:last(Cmd) of
%%                 46 ->
%%                     %% ... and it ends with a dot.  Then interpret it
%%                     %% as an erlang command where 'Dev' variable is bound.
%%                     Env = [{'Dev',Pid}],
%%                     Reply = tools:read_eval_print(Cmd, Env),
%%                     rsp:wrap(tools:hex(lists:flatten(Reply)));
%%                 _ ->
%%                     Forward()
%%             end
%%     end.


%% FIXME: Resolution isn't done very well.
send(Msg) -> gdbstub_hub ! Msg.
call(Msg) -> obj:call(gdbstub_hub, Msg).

dev(Pid) when is_pid(Pid) -> Pid;
dev(ID) -> {ok, Pid} = call({dev_pid, ID}), Pid.

info(ID) ->
    case call({dev_pid,ID}) of
        {ok, Pid} -> obj:dump(Pid);
        E -> E
    end.
            

%% Attach gdb-mi

gdb_mi(_ID) ->
    %% FIXME: hardcoded
    GdbMi = "/usr/local/bin/arm-eabi-gdb-7.8.1",
    TargetHost = "10.1.3.29",
    TargetPort = 1234,
    Elf = "/home/tom/exo/deps/uc_tools/gdb/relay_board.x8.elf",
    Sink = fun sink:print/1,
    gdb:open(GdbMi, TargetHost, TargetPort, Elf, Sink),
    ok.

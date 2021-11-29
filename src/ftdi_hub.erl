-module(ftdi_hub).
-export([start_link/1, handle/2, pids/1]).

start_link(#{ spawn_port := _ }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() -> Config end,
        fun ?MODULE:handle/2})}.

%% Event propagation:
%% - udev sends message to exo_notify
%% - that adds up here in handle({add_dev,....})
%% - a daemon gets started for each board: ftdi:start_link/1
%% - the up/2 task is started which does:
%% - upload fpga firmware via ice40 protocol
%% - upload RAM image using SPI


handle({add_dev,BHost,UsbDev,DevPath}, State = #{spawn_port := SpawnPort}) ->
    %% FIXME: Use UsbDev address to distinguish between different FTDI
    %% boards, e.g. <<"/dev/bus/usb/002/048">>
    log:info("~p~n", [{add_dev,BHost,UsbDev,DevPath}]),

    %% FIXME: Currently tied to exo
    %% FIXME: This only supports erlang nodes for now.  I.e. no exo
    %% ssh yet: build system needs to be updated for that to work.


    %% There is likely only one ftdi_hub instance, probably running on
    %% the dev host.  The ftdi "driver" object is started on the
    %% remote host that is connected to the FTDI board.  It uses the
    %% ~/bin/ftdi_connect.elf program set up locally.
    case
        rpc:call(
          exo:to_node(BHost),
          ftdi, start_link,
          [#{ spawn_port => SpawnPort,
              user => tom,
              dir  => "/home/tom/bin",
              host => localhost }])
    of
        {ok, Pid} ->
            unlink(Pid),
            Hub = self(),
            spawn(
              fun() ->
                      {ok, UsbAddr} = devpath_usb_port(DevPath),
                      log:set_info_name({ftdi_up,BHost,UsbAddr}),
                      up(Hub, Pid)
              end),
            _Ref = monitor(process, Pid),
            maps:put(Pid, {BHost,UsbDev,DevPath}, State)
    end;
        

handle({'DOWN',_Ref,process,Pid,Reason}=_Msg, State) ->
    log:info("removing: ~p: ~p~n", [Pid,Reason]),
    maps:remove(Pid, State);

handle(Msg, State) ->
    obj:handle(Msg, State).


up(_Hub, Pid) ->
    log:info("~p~n",[Pid]),
    log:info("FIXME: Map board to firmware.~n"),

    Dir = "/i/exo/ghci/fpga",

    %% ftdi:push_bin(Pid, Dir, "f_soc.breakout.ice40.bin"),
    ftdi:push_bin(Pid, Dir, "f_soc.breakout.bin"),


    %% FIXME: First time doesn't work, so do it twice.
    ftdi:push_bin(Pid, Dir, "f_soc.prog3.ram.bin"),
    ftdi:push_bin(Pid, Dir, "f_soc.prog3.ram.bin"),
    ok.

%% <<"/devices/pci0000:00/0000:00:14.0/usb2/2-2/2-2.3/2-2.3.2">>
devpath_usb_port(Bin) ->
    case lists:reverse(re:split(Bin,"/")) of
        [UsbPort|_] ->
            case re:split(UsbPort,"-") of
                [Interface,Chain] ->
                    ChainList = re:split(Chain,"\\."),
                    {ok, [binary_to_integer(C) || C <- [Interface | ChainList]]};
                _ -> error
            end;
        _ -> error
    end.


pids(Pid) ->
    maps:keys(obj:dump(Pid)).

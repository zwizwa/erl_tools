-module(ftdi_hub).
-export([start_link/0, handle/2, pids/1]).

start_link() ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                #{}
        end,
        fun ?MODULE:handle/2})}.
handle({add_dev,BHost,UsbDev,DevPath}, State) ->
    %% FIXME: Use UsbDev address to distinguish between different FTDI
    %% boards, e.g. <<"/dev/bus/usb/002/048">>
    log:info("~p~n", [{add_dev,BHost,UsbDev,DevPath}]),

    %% FIXME: Currently tied to exo
    %% FIXME: This only supports erlang nodes for now.  I.e. no exo
    %% ssh yet: build system needs to be updated for that to work.
    {ok, Pid} =
        rpc:call(
          exo:to_node(BHost),
          ftdi, start_link,
          [#{ spawn_port => exo:port_spawner(<<"localhost">>) }]),
    unlink(Pid),
    Hub = self(),
    spawn(
      fun() ->
              {ok, UsbAddr} = devpath_usb_port(DevPath),
              log:set_info_name({ftdi_up,BHost,UsbAddr}),
              up(Hub, Pid)
      end),
    _Ref = monitor(process, Pid),
    maps:put(Pid, {BHost,UsbDev,DevPath}, State);

handle({'DOWN',_,_,Pid,_}=_Msg, State) ->
    log:info("~p~n", [_Msg]),
    maps:remove(Pid, State);

handle(Msg, State) ->
    obj:handle(Msg, State).


up(_Hub, Pid) ->
    log:info("~p~n",[Pid]),
    log:info("FIXME: Map board to firmware.~n"),

    Dir = "/home/tom/exo/ghcid/fpga",
    ftdi:push_bin(Pid, Dir, "f_soc.breakout.ice40.bin"),
    %% FIXME: First time doesn't work.
    ftdi:push_bin(Pid, Dir, "f_soc.prog3.ram.bin"),
    ftdi:push_bin(Pid, Dir, "f_soc.prog3.ram.bin").
    

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

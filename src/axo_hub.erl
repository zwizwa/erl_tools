-module(axo_hub).
-export([start_link/1, handle/2, pids/1]).


%% Start here:
%%
%% - plug in core board in zoo
%% - panda: exo:need(axo_hub) ! {add_dev,<<"zoo">>}.
%% - zoo:   verify that log messages pass by (up asks version)
%%
%% Partial implementation of axo protocol in axo.erl
%%
%% Then to use, see axo:call/1





%% Code is derived from ftdi_hub.erl / ftdi.erl
%% See comments in that file.


%% To use debugger, plug in the board.  First the axoloti board, then
%% the stlink.  /etc/net/udev/openocd/3310_axoloti.sh starts openocd
%% on port 3312 via /etc/udev/rules.d/99-openocd.rules
%%
%% Firmware loading:
%%
%% (gdb) file /i/exo/axrai/firmware/build/axoloti.elf 
%% (gdb) load
%%
%% There is some automated gdb firmware load setup as well, see do.erl
%% target {axo_openocd,Host,Port}.


start_link(#{ spawn_port := _ }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() -> Config end,
        fun ?MODULE:handle/2})}.

handle({add_dev,BHost,UsbDev,DevPath}, State = #{spawn_port := SpawnPort}) ->
    log:info("~p~n", [{add_dev,BHost,UsbDev,DevPath}]),
    {ok, Pid} =
        rpc:call(
          exo:to_node(BHost),
          axo, start_link,
          [#{ spawn_port => SpawnPort,
              user => tom,
              dir  => "/i/exo/bin",
              host => localhost }]),
    %% Why did this unlink before?
    %% unlink(Pid),

    %% When starting through rpc:call, there is no link, so make sure
    %% the proxy and port get killed when we die.
    link(Pid),
    Hub = self(),
    spawn(
      fun() ->
              case devpath_usb_port(DevPath) of
                  {ok, UsbAddr} -> 
                      log:set_info_name({axo_up,BHost,UsbAddr});
                  error ->
                      log:set_info_name({axo_up,BHost})
              end,
              up(Hub, Pid)
      end),
    _Ref = monitor(process, Pid),
    maps:put(Pid, {BHost,UsbDev,DevPath}, State);

%% Shortcut, if there is only one.
handle({add_dev,BHost}, State) ->
    DevPath = <<"?">>,
    UsbDev = <<"?">>,
    handle({add_dev,BHost,UsbDev,DevPath}, State);

handle({'DOWN',_Ref,process,Pid,Reason}=_Msg, State) ->
    log:info("removing: ~p: ~p~n", [Pid,Reason]),
    maps:remove(Pid, State);

handle(Msg, State) ->
    obj:handle(Msg, State).


up(_Hub, Pid) ->
    log:info("axo_hub:up: ~p~n",[Pid]),
    Pid ! {send, <<16#566f7841:32/little>>},
    %%Pid ! {send, <<"AxoV">>},
    ok.

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
    lists:filter(
      fun erlang:is_pid/1,
      maps:keys(obj:dump(Pid))).



%% Convenience.  These are bound to default hub and assume only one
%% device.


load(_Elf) ->
    %% allocate(),
    ok.

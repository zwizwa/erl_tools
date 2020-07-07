-module(axo_hub).
-export([start_link/1, handle/2, pids/1]).

%% Derived from ftdi_hub.erl / ftdi.erl
%% See comments in that file.

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
    unlink(Pid),
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
    log:info("~p~n",[Pid]),
    Pid ! {send, <<16#566f7841:32/little>>},
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
    maps:keys(obj:dump(Pid)).

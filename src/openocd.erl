-module(openocd).
-export([start_link/1, handle/2]).

%% openocd monitor/proxy.
%%
%% This is built on "/etc/net/udev/openocd" infrastructure that is not
%% included here.  The start/stop of openocd is managed elsewhere.
%% This process is configured with TCP host + port for the gdbstub
%% (3333) interface.

%% The main purpose of this service is to provide a somewhat high
%% level plug-and-play interface to gdb / openocd that presents a work
%% flow similar to that based on uc_tools gdbstub written for "blue
%% pill" stm32f103.

%% Example on 29: exo:start({openocd,3310}).  for axo.  relies on
%% manual start for openocd atm.

start_link(Config0) ->
    %% Allow for some more convenient config shortcuts, as we use this
    %% in log messages.
    Config =
        case Config0 of
            _ when is_number(Config0) -> #{ gdb_port => Config0 };
            _ -> Config0
        end,

    Host       = maps:get(host, Config, "zoe.zoo"),
    GdbPort    = maps:get(gdb_port, Config, 3333),
    GdbMi      = maps:get(gdb_mi, Config, "/i/exo/gdb/gdb -i=mi"), %% FIXME
    {ok,
     serv:start(
       {handler, 
        fun() ->
                log:set_info_name({openocd,Config0}),
                self() ! check,
                #{ host        => Host,
                   gdb_port    => GdbPort,
                   gdb_mi      => GdbMi
                 }
        end,
        fun ?MODULE:handle/2})}.

handle(check, State = #{gdb_mi := GdbMi, gdb_port := GdbPort, host := Host }) ->
    %% What do we want to be ok here?  Let's focus on a gdb connection
    %% for now.
    case maps:find(gdb, State) of
        {ok, _} ->
            %% FIXME: ping?
            State;
        error ->
            %% Gdb = gdb:open_mi(GdbMi),
            Sink = fun(Thing) -> log:info("gdb: ~p~n", [Thing]) end,
            Gdb = gdb:open(GdbMi, Host, GdbPort, none, Sink),
            maps:put(gdb, Gdb, State)
    end;

handle({Port, {data,{eol,Line}}}, State = #{gdb := Port}) ->
    log:info("gdb: ~s~n", [Line]),
    State;

handle({_, dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    log:info("unknown: ~p~n", [Msg]),
    State.




-module(openocd).
-export([start_link/1, handle/2]).

%% openocd monitor/proxy.
%%
%% This is built on "/etc/net/udev/openocd" infrastructure that is not
%% included here.  The start/stop of openocd is managed elsewhere.
%% This process is configured with TCP ports fort the gdbstub (3333)
%% and telnet (4444) interfaces.

%% The main purpose of this service is to provide a somewhat high
%% level plug-and-play interface to gdb / openocd that presents a work
%% flow similar to that based on uc_tools gdbstub.

%% Example (zoe)  exo:start({openocd,3310}).  for axo.  needs manual start.

start_link(Config0) ->
    %% Allow for some more convenient config shortcuts, as we use this
    %% in log messages.
    Config =
        case Config0 of
            _ when is_number(Config0) -> #{ gdb_port => Config0 };
            _ -> Config0
        end,

    GdbPort    = maps:get(gdb_port, Config, 3333),
    TelnetPort = maps:get(telnet_port, Config, 4444),
    GdbMi      = maps:get(gdb_mi, Config, "arm-eabi-gdb-7.6 -i=mi"), %% FIXME
    {ok,
     serv:start(
       {handler, 
        fun() ->
                log:set_info_name({openocd,Config0}),
                self() ! check,
                #{ gdb_port    => GdbPort,
                   telnet_port => TelnetPort,
                   gdb_mi      => GdbMi
                 }
        end,
        fun ?MODULE:handle/2})}.

handle(check, State = #{gdb_mi := GdbMi, gdb_port := GdbPort }) ->
    %% What do we want to be ok here?  Let's focus on a gdb connection
    %% for now.
    case maps:find(gdb, State) of
        {ok, _} ->
            %% FIXME: ping?
            State;
        error ->
            %% Gdb = gdb:open_mi(GdbMi),
            Sink = fun(Thing) -> log:info("gdb: ~p~n", [Thing]) end,
            Gdb = gdb:open(GdbMi, "localhost", GdbPort, none, Sink),
            maps:put(gdb, Gdb, State)
    end;

handle({Port, {data,{eol,Line}}}, State = #{gdb := Port}) ->
    log:info("gdb: ~s~n", [Line]),
    State;

handle({_, dump}=Msg, State) ->
    obj:dump(Msg, State);
handle(Msg, State) ->
    log:info("unknown: ~p~n", [Msg]),
    State.




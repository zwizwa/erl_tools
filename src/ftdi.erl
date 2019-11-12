-module(ftdi).
-export([start_link/1, handle/2, push_bin/2, push_bin/3, test/1]).

start_link(Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = 
                    tools:spawn_port(
                      Config,
                      {"ftdi_connect.elf", ["i:0x0403:0x6010"]},
                      %% "ftdi_connect.elf",
                      [use_stdio, binary, exit_status, {packet,4}]),
                maps:merge(
                  Config,
                  #{ port => Port })
        end,
        fun ?MODULE:handle/2})}.

handle({Pid, {send_spi, Bin}}, State = #{port := Port}) ->
    Port ! {self(), {command, [<<1,0,0,0>>, Bin]}},
    wait_ack(Port),
    obj:reply(Pid, ok),
    State;

handle({Pid, {send_ice40, Bin}}, State = #{port := Port}) ->
    Port ! {self(), {command, [<<2,0,0,0>>, Bin]}},
    wait_ack(Port),
    obj:reply(Pid, ok),
    State;

handle({Port, _}=Msg, #{ port := Port}) ->
    exit(Msg);

handle(Msg, State) ->
    obj:handle(Msg, State).


wait_ack(Port) ->
    receive
        {Port, _Msg} ->
            log:info("ack: ~p~n", [_Msg])
    end.
        


%% FIXME: This is a "router" for .ram.bin and .ice40.bin files
%% Change the extensions to reflect only the necessary information.

push_bin(Pid,Path,File) ->
    log:info("ftdi: ~999p~n", [{Path,File}]),
    Load =
        fun() ->
                F = tools:format("~s/~s", [Path, File]),
                {ok, Bin} = file:read_file(F),
                Bin
        end,

    case lists:reverse(re:split(File,"\\.")) of
        [<<"bin">>,<<"ram">>|_] ->
            obj:call(Pid, {send_spi, Load()});
        [<<"bin">>,<<"ice40">>|_] ->
            obj:call(Pid, {send_ice40, Load()});
        Unknown ->
            log:info("ftdi:push_bin: unknown: ~p~n", [Unknown])
    end.


push_bin(Path, File) ->
    %% FIXME: Caller needs to find hub.
    try
        Hub = rpc:call('exo@10.1.3.29',exo,need,[ftdi_hub]),
        [Pid|_] = ftdi_hub:pids(Hub),
        push_bin(Pid, Path, File)
    catch C:E ->
            log:info("ftdi: push_bin: ~p~n",[{Path,File,C,E}])
    end.
        

test(core) ->
    push_bin("/home/tom/exo/ghcid/fpga",
             "f_soc.breakout.ice40.bin");

%% After programming core, the first prog3 fails.  Second is ok.  What
%% also works: core, restart ftdi, prog3.
test(prog3) ->
    push_bin("/home/tom/exo/ghcid/fpga",
             "f_soc.prog3.ram.bin");
test(prog4) ->
    push_bin("/home/tom/exo/ghcid/fpga",
             "f_soc.prog4.ram.bin").


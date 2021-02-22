-module(timeseries).
-export([start_link/1, handle/2]).

start_link(#{}=Init) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                SpawnPort = fun exo_port:spawn_port/1,
                Port =
                    SpawnPort(
                      #{ dir  => "/i/exo/uc_tools/linux",  %% FIXME: hardcoded
                         cmd  => "timeseries.dynamic.host.elf",
                         args => [],
                         opts => [use_stdio, binary, exit_status, {packet,4}] }),
                maps:merge(
                  Init,
                  #{port => Port})
        end,
        fun ?MODULE:handle/2})}.
    
handle({_,dump}=Msg, State) ->    
    obj:dump(Msg, State);

%% Delegate to mixins at tail end.
handle(Msg, State) -> 
    Mixins = [fun epid:mixin/3, fun tag_u32:mixin/3],
    {Handled, State1} = serv:delegate(Mixins, Msg, State),
    case Handled of
        true ->
            State1;
        false ->
            log:info("unknown: ~p~n",[Msg]),
            State1
    end.

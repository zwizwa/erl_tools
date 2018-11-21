-module(gpio_poll).
-export([start/1, parse/1]).
start(ValueNodes) ->

    %% FIXME: remove this hack
    Priv =
        case code:priv_dir(erl_tools) of
            {error,bad_name} -> 
                %% Allow files to be copied into other application's
                %% source tree.  In that case an environment variable
                %% needs to be set to recover the location of the
                %% binary.
                case os:getenv("ERL_TOOLS_APP") of
                    false -> throw(no_env_ERL_TOOLS_APP);
                    AppStr -> code:priv_dir(list_to_atom(AppStr))
                end;
            Dir -> Dir
        end,

    Cmd = iolist_to_binary([Priv, "/gpio_poll.elf", [[" ", VN] || VN <- ValueNodes]]),
    log:info("gpio_poll: ~s~n", [Cmd]),
    open_port({spawn, Cmd}, [use_stdio, {line,10}, exit_status, binary]).

parse(Msg) ->
    %% Any other messages cause pattern errors and kill the process.
    {data,{eol,Line}} = Msg,
    case lists:map(fun binary_to_integer/1, re:split(Line,",")) of
        [-1, -1] -> ping;
        [I,V] -> {ok, {I,V}}
    end.

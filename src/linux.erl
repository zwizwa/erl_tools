-module(linux).
-export([interfaces/0, interfaces_re/1, brctl_addif/2]).

%% Wrappers around linux commands.
%% FIXME: parsers are very ad-hoc.

interfaces() ->
    lists:map(
      fun interface/1,
      skip_odd(re:split(os:cmd("ip link show"),"\n"))).
interface(L) ->
    {match,[_,Iface]} = re:run(L,"\\d+:\\s*(.*?):.*",[{capture,all,binary}]),
    Iface.
skip_odd([H,_|T]) -> [H|skip_odd(T)];
skip_odd(_) -> [].


interfaces_re(Re) ->
    lists:filter(
      fun(I) ->
              case re:run(I,Re) of
                  {match, _} -> true;
                  _ -> false
              end
      end,
      interfaces()).

                          
             
brctl_addif(Bridge, Iface) when is_binary(Bridge) and is_binary(Iface) ->
    Cmd = tools:format("brctl addif ~s ~s", [Bridge, Iface]),
    CmdOut = os:cmd(Cmd),
    {Cmd,CmdOut}.

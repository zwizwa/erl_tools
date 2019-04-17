-module(linux).
-export([interfaces/0, interfaces_re/1, brctl_addif/2, ip_addresses/1, netmask_number/1]).

%% Wrappers around linux commands.
%% FIXME: parsers are very ad-hoc.

%% FIXME: use inet:getifaddrs/0 instead

%% [<<"lo">>,<<"eth0">>,<<"usb0">>,<<"br0">>]
interfaces() ->
    {ok, PL} = inet:getifaddrs(),
    [list_to_binary(Key) || {Key,_} <- PL].

%% interfaces() ->
%%     lists:map(
%%       fun interface/1,
%%       skip_odd(re:split(os:cmd("ip link show"),"\n"))).
%% interface(L) ->
%%     {match,[_,Iface]} = re:run(L,"\\d+:\\s*(.*?):.*",[{capture,all,binary}]),
%%     Iface.
%%
%% skip_odd([H,_|T]) -> [H|skip_odd(T)];
%% skip_odd(_) -> [].


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

ip_addresses(Iface) when is_list(Iface) ->
    {ok, PL} = inet:getifaddrs(),
    Info = proplists:get_value(Iface, PL),
    Addrs    = [A || {addr,A={_,_,_,_}} <- Info],
    NetMasks = [N || {netmask,N={_,_,_,_}} <- Info],
    %% This assumes inet:getifaddrs/0 returns the address, netmask in
    %% the proper order.  That is not documented, but seems to be the
    %% case.
    lists:zipwith(
      fun(A,N) -> {A, netmask_number(N)} end,
      Addrs, NetMasks);

ip_addresses(Iface) ->
    ip_addresses(tools:format("~s",[Iface])).

%% Get the /<numbits> representation of a netmask.
netmask_number({A,B,C,D}) ->
    Bits = [Bit || <<Bit:1>> <= <<A,B,C,D>>],
    {Head,Tail} = lists:splitwith(fun(Bit) -> Bit == 1 end, Bits),
    %% Be good and assert proper format.
    lists:foreach(fun(Bit) -> 0 = Bit end, Tail),
    length(Head).

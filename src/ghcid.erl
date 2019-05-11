%% Simple wrapper for ghcid.

%% - test output is not included in ghcid output file, so maybe just
%%   write it to another file in the hs test.

-module(ghcid).
-export([start_link/1, handle/2, success/1]).



start_link(#{ ghcid_cmd := Cmd } = Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = open_port({spawn, Cmd}, [use_stdio, binary]),
                maps:put(port, Port, Config)
        end,
        fun ?MODULE:handle/2})}.

%% Just capture ^G (7) BELL to get a notification that something
%% happened, then poll the output file.
handle({Port,{data, Data}}, #{ port := Port } = State) ->
    %% io:format("~s~n",[Data]),
    lists:foreach(
      fun(7) -> self() ! bell;
         (_) -> ok end,
      binary_to_list(Data)),
    State;

handle(bell, State = #{notify := {M,F,A}}) ->
    %% log:info("bell~n"),
    %% You probably want this to use success/1
    erlang:apply(M,F,A),
    State;

handle({Port,_}=_Msg, #{ port := Port } = State) ->
    log:info("~p~n", [_Msg]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).


%% Get a pass/fail flag from log output.
success(Logfile) ->
    {ok, F} = file:open(Logfile,[read]),
    {ok, Head} = file:read_line(F),
    %% log:info("~p~n", [Head]),
    file:close(F),
    Ok=tools:re_dispatch(
         Head,
         [{"^All good", fun(_) -> true  end},
          {"",          fun(_) -> false end}]),
    %% log:info("ok: ~p~n", [Ok]),
    Ok.

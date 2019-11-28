%% Simple wrapper for ghcid.

%% - test output is not included in ghcid output file, so maybe just
%%   write it to another file in the hs test.

-module(ghcid).
-export([start_link/1, handle/2, success/1]).



%% There seems to be an issue: multiple instances are started.


start_link(#{ ghcid_cmd := Cmd, notify := {_M,_F,_A} } = Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = open_port({spawn, Cmd}, [use_stdio, binary, exit_status]),
                maps:merge(
                  Config,
                  #{ port => Port,
                     port_info => erlang:port_info(Port) })
        end,
        fun ?MODULE:handle/2})}.

%% The bell mechanism is crude.  A better approach is to send an event
%% from the Haskell side at the end of the test script.

%% Just capture ^G (7) BELL to get a notification that something
%% happened, then poll the output file.
handle({Port,{data, Data}}, #{ port := Port } = State) ->
    %% io:format("~s~n",[Data]),
    lists:foreach(
      fun(7) -> self() ! bell;
         (_) -> ok end,
      binary_to_list(Data)),
    State;

%

handle(bell, State = #{notify := {M,F,A}}) ->
    %% You probably want this to use success/1
    erlang:apply(M,F,A),
    State;
handle(bell, State) ->
    log:info("bell~n"),
    State;

handle({Port,{exit_status,_}}=Msg, #{ port := Port }) ->
    exit(Msg);

handle({Port,_}=_Msg, #{ port := Port } = State) ->
    log:info("~p~n", [_Msg]),
    State;


handle(Msg, State) ->
    obj:handle(Msg, State).


%% Get a pass/fail flag from log output.
success(Logfile) ->
    try
        {ok, F} = file:open(Logfile,[read]),
        {ok, Head} = file:read_line(F),
        %% log:info("~p~n", [Head]),
        file:close(F),
        Ok=tools:re_case(
             Head,
             [{"^All good", fun(_) -> true  end},
              {"",          fun(_) -> false end}]),
        %% log:info("ok: ~p~n", [Ok]),
        Ok
    catch
        %% There seems to be a race condition.
        C:E ->
            log:info("WARNING: ~p~n", [{C,E}]),
            false
    end.

%% Wrapper for ghcid.

%% - test output is not included in ghcid output file, so maybe just
%%   write it to another file in the hs test.

-module(ghcid).
-export([start_link/1, handle/2, success/1, clean/1]).



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

%% BERT service interface.
%%
%% If exp-ghcid.hs runs ExoBERT.start, there will be a BERT interface
%% to the running process.

%% Note there is an issue where the ghc process lingers, which
%% manifests as the folling error:
%% ghcid: *** Exception: Network.Socket.bind: resource busy (Address already in use)~
%%
%% I've enabled console logging, so it should be easy to spot.  The
%% workaround is "killall ghc", upon which Erlang will restart the
%% ghcid process.  Maybe add a monitor on the text output of ghcid to
%% just work around this in a more robust way.  I've tried but I can't
%% figure out how exactly it works on the haskell side.
%%


handle({ReplyPid, {cmd, MFA={_,_,_}}}, State) ->
    %% Instead of creating a separate process, reuse the state
    %% machine.  In many cases, monolithic state machines make error
    %% management a lot simpler.  Composition is alsy quite
    %% straighforward using map nesting, so no actual constraints need
    %% to be imposed on the sub-machine.
    SubState =
        maps:get(
          bert_rpc,
          State,
          #{ spec => {"127.0.0.1", 7890} }),
    maps:put(
      bert_rpc,
      bert_rpc:handle({ReplyPid, MFA}, SubState),
      State);




%% The bell mechanism is crude.  A better approach is to send an event
%% from the Haskell side at the end of the test script.

%% Just capture ^G (7) BELL to get a notification that something
%% happened, then poll the output file.
handle({Port,{data, Data}}, #{ port := Port } = State) ->
    log:info("~s~n",[clean(Data)]),
    lists:foreach(
      fun(7) -> self() ! bell;
         (_) -> ok end,
      binary_to_list(Data)),
    State;



handle(bell, State = #{notify := {M,F,A}}) ->
    %% You probably want this to use success/1
    try erlang:apply(M,F,A)
    catch C:E -> log:info("notify failed: ~p~n", [{C,E}]) end,
    State;
handle(bell, State) ->
    log:info("bell~n"),
    State;

handle({Port,{exit_status,_}}=Msg, #{ port := Port }) ->
    exit(Msg);

handle({Port,_}=_Msg, #{ port := Port } = State) ->
    log:info("~p~n", [_Msg]),
    State;


handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);

handle(Msg, State) ->
    log:info("unknown: ~p~n", Msg),
    throw({?MODULE,unknown_msg,Msg}),
    State.
    



%% Get a pass/fail flag from log output.
%% FIXME: There are issues with this.
success(Logfile) ->
    try
        {ok, F} = file:open(Logfile,[read]),
        {ok, Head} = file:read_line(F),
        %% log:info("~p~n", [Head]),
        _ = file:close(F),
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


clean(Bin) when is_binary(Bin) ->
    clean(binary_to_list(Bin));
clean([]) -> [];
clean([H|T]) -> [clean_char(H)|clean(T)].

clean_char(C) ->
    case (C >= 32) and (C < 127) of
        true  -> C;
        false -> $~
    end.

            
                 

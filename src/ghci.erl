%% Wrapper for ghci.
%% See also ghcid.erl

%% FIXME: Look into ghcide.

%% NIH: Building what I actually need, instead of trying to shoe-horn
%% ghcid into exo.  I already have file change notifications, so just
%% need simple reload and test run.

%% The module is stateful: there is an idea of a "current" module that
%% exposes a "test" function.

%% GHCI output is dumped in the Erlang console log.  This is
%% asynchronous only. Any other synchronization will need to be built
%% externally using Haskell->Erlang message path.

-module(ghci).
-export([start_link/1, handle/2]).

start_link(#{ ghci_cmd := Cmd, module := Module } = Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                %% It is assumed a test module is loaded at all times.
                %% The test itself doesn't need to run at startup.
                handle(
                  {load, Module},
                  maps:put(
                    port,
                    open_port({spawn, Cmd}, [use_stdio, exit_status]),
                    Config))
        end,
        fun ?MODULE:handle/2})}.


handle({cmds, Cmds}, #{ port := Port } = State) ->
    port_command(Port, [[Cmd, "\n"] || Cmd <- Cmds]),
    State;

handle({load, Module}, State) ->
    handle({cmds,[tools:format(":load ~s",[Module])]}, 
           maps:put(module, Module, State));

%% To build ad-hoc two-way communication using in-band log lines, have
%% the caller specify a reference string.  Note that this is subject
%% to some (ill-specified) quoting constraints.  Good enough for
%% simple things.  For anything more elaborate, use a socket.
handle({test,AckString}, State = #{ module := Module }) ->
    LogBuf = maps:get(log_buf, State, fun log_buf/1),
    LogBuf(clear),
    handle(
      {cmds,
       [":reload",
        %% Qualify calls to avoid surprises.
        tools:format(
          "do ~s.test ; Prelude.putStrLn \"\\n~s\"",
          [Module, AckString])]},
      State);

handle(test, State) ->
    AckString = maps:get(ack, State, ""),
    handle({test, AckString}, State);

handle({Port, {data, Data}}, #{ port := Port } = State) ->
    LogBuf = maps:get(log_buf, State, fun log_buf/1),
    Buf = maps:get(buf, State, []),
    Buf1 = log_lines(LogBuf, Data, Buf),
    maps:put(buf, Buf1, State);

handle({Port, {exit_status,_}}=Msg, #{ port := Port }) ->
    exit(Msg);

handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);

handle(Msg, State) ->
    log:info("unknown: ~p~n", Msg),
    throw({?MODULE,unknown_msg,Msg}),
    State.
    

log_buf({line, Line}) -> log:info("~s~n",[Line]);
log_buf(_) -> ok.
    

%% Console logger.
%% FIXME: Send stuff to emacs buffer also?
flat(L) -> lists:flatten(L).
     
log_lines(_, [], Line) ->          flat(Line);
log_lines(B, [$\n|Tail], Line) ->  B({line, flat(Line)}), log_lines(B, Tail,[]);
log_lines(B, [Char|Tail], Line) -> log_lines(B,Tail,[Line,Char]).



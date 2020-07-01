%% FIXME:

%% Special case ExoBERT. Later bootstrap todo:
%%
%% - start ghci
%% - start loop
%% - use bert rpc to get pid
%% - save pid for later control

%% - proper restart even if loop is running:
%%   exo:restart(ghci_bert).
%%
%% - start loop:
%%   exo:need(ghci_bert) ! {cmds,[":reload","start"]}.
%%
%% - stop loop:
%%   bert_rpc:call("localhost",7890,control,stop,[]).
%%
%% - alternatively, get pid via this, then send SIGINT
%%   bert_rpc:call("localhost",7890,control,pid,[]).


%% Wrapper for ghci.
%% See also ghcid.erl

%% Notes

%% - NIH: Building what I actually need, instead of trying to shoe-horn
%%   ghcid into exo.  I already have file change notifications, so just
%%   need simple reload and test run.
%%
%% - The proper way to do this is to move to ghcide.
%%
%% - The module is stateful: there is an idea of a "current" module
%%   that exposes a "test" function.
%%
%% - GHCI output can be redirected to a "buffer", which implements a
%%   'clear' and {'line',Line} 
%%
%% - Ad-hoc synchronization use the '#' character to encode
%%   continuations as hex-encoded binary terms.
%%
%% - Data exchange should go over a side channel.  Since this is used
%%   in redo scripts, it seems simplest to just communicate through
%%   files.  E.g. pass in/out file as parameters, and use the '#'
%%   mechanism to signal.
%%


-module(ghci).
-export([start_link/1, handle/2, call/5]).

start_link(#{ ghci_cmd := Cmd, module := Module } = Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                %% After initial load, send an optional initialization
                %% message, e.g. to start a service inside ghci.
                case maps:find(init_msg, Config) of
                    error -> ok;
                    {ok, Msg} -> self() ! Msg
                end,

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

handle(clear, State) ->
    LogBuf = maps:get(log_buf, State, fun log_buf/1),
    LogBuf(clear),  %% FIXME: this seems to have no effect with exo config
    State;

handle({cmds, Cmds}, #{ port := Port } = State) ->
    port_command(Port, [[Cmd, "\n"] || Cmd <- Cmds]),
    State;

handle({load, Module}, State) ->
    handle({cmds,[tools:format(":load ~s",[Module])]}, 
           maps:put(module, Module, State));

%% This is very ad-hoc, but we do our best to:
%% - Ensure the correct module is loaded
%% - Ensure an ack that the caller can use to sync on
%% This way it is still possible to have multiple calls in flight.
%%
%% Note that the sync is just an empty event.  To determine success
%% programmatically, a side effect needs to be used, e.g. the
%% existence of a file.

handle({run,Module,Function,Arg,SyncString}, State0 = #{module := CurrentModule}) ->
    State =
        case Module == CurrentModule of
            true  -> State0;
            false -> handle({load, Module}, State0)
        end,
    log:info("run: ~p~n", [{Module,Function}]),
    LogBuf = maps:get(log_buf, State, fun log_buf/1),
    LogBuf(clear),  %% FIXME: this seems to have no effect with exo config
    handle(
      {cmds,
       [":reload",
        tools:format(
          "~s.~s ~s",
          [Module, Function, Arg]),
        tools:format(
          "Prelude.putStrLn \"\\n~s\"",
          [SyncString])]},
      State);

%% Run with Erlang continuation encoded in the ack string.
handle({run_cont,Module,Function,Arg,Cont}, State) ->
    SyncString = [$#, tools:hex(erlang:term_to_binary(Cont))],
    handle({run,Module,Function,Arg,SyncString}, State);

%% Data coming from ghci gets chopped into lines and passed to a
%% log_buf.  Mostly modeled after emacs buffer: supports append lines
%% + buffer clear.  Line framing is used to be able to easily encode
%% some in-band data.
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
   

flat(L) -> lists:flatten(L).
     


%% Abstract logger.
log_lines(_, [], Line) ->          flat(Line);
log_lines(B, [$\n|Tail], Line) ->  dispatch_line(B, Line), log_lines(B, Tail,[]);
log_lines(B, [Char|Tail], Line) -> log_lines(B,Tail,[Line,Char]).

dispatch_line(B, Line) ->
    case flat(Line) of
        [$#|Enc]=Flat -> 
            try
                Bin = iolist_to_binary(tools:unhex(Enc)),
                Term = binary_to_term(Bin),
                Term()
            catch C:E ->
                    log:info("WARNING: decoding: ~p~n", [{C,E}]),
                    B({line,Flat})
            end;
        Flat ->
            B({line, Flat})
    end.

%% Synchronous call.  This is very raw, e.g. no return values, but
%% good enough for now when storing values in the file system,
%% e.g. for redo.erl

%% FIXME: It might be enough to store the erlang term in the daemon
%% and use a generic ack marker.  Anyway, all just very ad-hoc code
%% that needs to be cleaned up once the full chain is up.

%% FIXME: At the very least allow for pass/fail without relying on
%% timeouts.  When doing pass/fail it's possible to return anything
%% really, so let it return a string instead.

%% FIXME: Scrape error messages to avoid timeout.  They are fairly
%% uniform, ending in ": error:" Note that that would give
%% multiple acks, so it needs to be stateful.

call(Ghci, Module, Function, Arg, TimeOut) ->
    log:info("ghci:call ~999p~n", [{Module,Function,Arg,TimeOut}]),
    Pid = self(),
    Ref = erlang:make_ref(),
    Ghci ! {run_cont, Module, Function, Arg, fun() -> Pid ! Ref end},
    receive Ref -> ok after TimeOut -> {error, timeout} end.








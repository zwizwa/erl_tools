%% Wrapper for ghci.
%% See also ghcid.erl

%% FIXME: Look into ghcide.

%% This is a NIH run to build what I actually need, instead of trying
%% to shoe-horn ghcid into exo.  It uses straight ghci commands and
%% the following stateful principle:
%%
%% - Load the test module.
%% - Execute "test" in that module.


-module(ghci).
-export([start_link/1, handle/2, cmd/2]).

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

handle(test, State) ->
    handle({cmds,[":reload","test"]}, State);

handle({Port,{data, Data}}, #{ port := Port } = State) ->
    Buf = maps:get(buf, State, []),
    Buf1 = log_lines(Data, Buf),
    maps:put(buf, Buf1, State);

handle({Port,{exit_status,_}}=Msg, #{ port := Port }) ->
    exit(Msg);

handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);

handle({push_change, _File}=Msg, State) ->
    %% FIXME: Maybe make this conditional?  I don't really have
    %% dependencies so it is not clear if this even needs to
    %% propagate.
    log:info("~p~n", [Msg]),
    handle(test, State);

handle(Msg, State) ->
    log:info("unknown: ~p~n", Msg),
    throw({?MODULE,unknown_msg,Msg}),
    State.
    

%% Console logger.
%% FIXME: Send stuff to emacs buffer also?
log_lines([], Line) ->          lists:flatten(Line);
log_lines([$\n|Tail], Line) ->  log:info("~s~n",[Line]), log_lines(Tail,[]);
log_lines([Char|Tail], Line) -> log_lines(Tail,[Line,Char]).

%% There is no synchronous command yet.  The rpc should probably be
%% threaded through the haskell code using some Haskell->Erlang
%% message send option.
cmd(Pid, Cmd) ->
    Pid ! Cmd.



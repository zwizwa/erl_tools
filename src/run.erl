-module(run).
-export([session/2]).

session(Cmd,Arg) ->
    tools:run_session(Cmd,Arg).

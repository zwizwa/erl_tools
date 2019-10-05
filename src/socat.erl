-module(socat).
-export([pty/1]).

%% A server process isn't necessary.  Just provide open_port shortcuts.
pty(Link) ->
    Cmd = tools:format("socat - PTY,link=~s,echo=0,raw", [Link]),
    open_port({spawn, Cmd}, [use_stdio, binary, exit_status]).

-module(socat).
-export([pty/1,tcp_listen/1]).

%% A server process isn't necessary.  Just provide open_port shortcuts.
pty(Link) ->
    Cmd = tools:format("socat - PTY,link=~s,echo=0,raw", [Link]),
    open_port({spawn, Cmd}, [use_stdio, binary, exit_status]).

tcp_listen(Port) when is_integer(Port) ->
    Cmd = tools:format("socat - TCP-LISTEN:~p,reuseaddr", [Port]),
    open_port({spawn, Cmd}, [use_stdio, binary, exit_status]).
    

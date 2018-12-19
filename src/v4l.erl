-module(v4l).
-export([obj/1, port/1, handle/2]).

%% User needs to provide command to launch (remote) binary.  The
%% function here only sets the protocol used over the link.

%% Note that this has been designed in the scenario where the remote
%% machine is not running an Erlang instance.  It will only run the
%% port process.  In the application, ssh's authorized_keys mechanism
%% is used to securely connect to the node.

port(Cmd) ->
    open_port({spawn, Cmd}, [use_stdio, {packet,4}, exit_status, binary]).

next(Port) ->
    port_command(Port, <<>>).

obj(Cmd) ->
    serv:start(
      {handler,
       fun() -> 
               Port = port(Cmd),
               next(Port),
               #{ port => Port } 
       end,
       fun v4l:handle/2}).
handle({Port, Msg}, #{ port := Port } = State) ->
    case Msg of
        {data, _} ->
            log:info("frame~n"),
            next(Port),
            State;
        {exit_status, _}=E ->
            exit(E)
    end;
handle(Msg, State) ->
    obj:handle(Msg, State).
               

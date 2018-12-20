-module(v4l).
-export([obj/1, port/1, handle/2]).

%% User needs to provide command to launch (remote) binary.  The
%% function here only sets the protocol used over the link.  See
%% c_src/v4l.c

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
               #{ port => Port, bc => serv:bc_start() } 
       end,
       fun v4l:handle/2}).
handle({Port, Msg}, #{ port := Port, bc := BC } = State) ->
    case Msg of
        {data, Data} ->
            %% log:info("frame: ~p bytes~n", [size(Data)]),
            BC ! {broadcast, {v4l, self(), {jpeg, Data}}},
            next(Port),
            State;
        {exit_status, _}=E ->
            exit(E)
    end;
handle({subscribe, _}=Msg, State = #{ bc := BC }) ->
    BC ! Msg, State;
handle(Msg, State) ->
    obj:handle(Msg, State).
               

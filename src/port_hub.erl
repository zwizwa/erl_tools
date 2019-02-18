%% Generic device hub = dynamic port collection
%% FIXME: For midi_raw, gdbstub_hub, ...
%% FIXME: Is it worth doing this generically?

-module(port_hub).
-export([start_link/1, hub_handle/2,
         start_dev/1, dev_handle/2]).


start_link(_) ->
    {ok,
     serv:start(
       {handler,
        fun() -> #{} end,
        fun ?MODULE:hub_handle/2})}.

hub_handle({Pid, {add, #{ dev := ID }=Info}},
           State = #{ open_port := OpenPort }) ->
    StartDev = maps:get(start_dev, State, fun ?MODULE:start_dev/1),

    %% Assume Host is an erlang node, and start a process that
    %% accesses the file.
    try 
        DevPid = StartDev(maps:put(open_port, OpenPort, Info)),
        _Ref = erlang:monitor(process, DevPid),
        obj:reply(Pid, {ok, {ID, DevPid}}),
        maps:put(ID, DevPid, State)
    catch C:E ->
            obj:reply(Pid, {error, {C,E}}),
            State
    end;

hub_handle({'DOWN',_Ref,process,Pid,_Reason}, State) ->
    IState = tools:maps_inverse(State),
    ID = maps:get(Pid, IState),
    maps:remove(ID, State).

start_dev(#{ open_port := OpenPort }=Info) ->                
   serv:start(
     {handler,
      fun() -> maps:put(port, OpenPort(Info), Info) end,
      fun ?MODULE:dev_handle/2}).

dev_handle({Port,{exit_status,_}=E}, #{ port := Port}) ->
    log:info("~p~n",[E]),
    exit(E);
dev_handle({Port,{data,Data}}, #{ port := Port, hub := Hub}=State) ->
    Hub ! {dev_data, self(), Data},
    State;
dev_handle(Msg, State) ->
    obj:handle(Msg, State).

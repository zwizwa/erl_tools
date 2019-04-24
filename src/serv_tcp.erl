%% (c) 2018 Tom Schouten -- see LICENSE file

-module(serv_tcp).
-export([
         %% V1 API, for legacy support
         init/2, init/4, init/5, handle/2, accept_loop/3,
         %% V2 API, use this for new code
         start_link/1, accept/1, listener_handle/2, pids/1
]).

%% TCP server with client registry.



%% `Connect': a serv:start prototype that handles a single client
%% connection, taking the socket as argument of the init module.

%% `Handle',`State': function that handles additional messages sent to
%% the server registry process, with its corresponding initial state.



init(Ports, Connect, Handle, State, Opts) ->
    %% This runs in the same process as the handler.
    process_flag(trap_exit, true),
    Registry = self(),  
    %% Create listening sockets and acceptors.
    LSocks =
        [case listen(Port, Opts) of
             {error, Reason} ->
                 tools:info("listen error: port ~p: ~p~n", [Port,Reason]),
                 exit(Reason);
             {ok, LSock} ->
                 self() ! {spawn_acceptor, LSock, Port},
                 LSock
         end || Port <- Ports],
    {#{registry => Registry,
       connect  => Connect,
       handle   => Handle,
       lsocks   => LSocks},
     [], %% Connection Pids
     State}.
init(P,C,D,S) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    init(P,C,D,S,Opts).
init(P,C) ->
    Handle = fun(_, State) -> State end,
    init(P,C,Handle,[]).


%% Allow Port and {IP,Port} specs for listening port.
listen(Port, Opts) when is_number(Port) ->
    gen_tcp:listen(Port, Opts);
listen({{_,_,_,_}=IP,Port},Opts) ->
    listen(Port, Opts ++ [{ip,IP}]).
    

    

%% FIXME: make accept loop reloadable as well.
accept_loop(LSock, Port,
            #{connect  := Connect,
              registry := Registry
             }=Env) ->
    case gen_tcp:accept(LSock) of
        {error, timeout} -> serv_tcp:accept_loop(LSock, Port, Env);
        {error, Reason}  -> exit(Reason);
        {ok, Sock} ->
            %% Notify registry, then enter server loop.
            Registry ! {register_connect, self()},
            Registry ! {spawn_acceptor, LSock, Port},
            case Connect of
                {handler, {Init, ContextArgs}, Handle}
                  when is_function(Init) and is_list(ContextArgs) ->
                    %% Labda-lifted version of the case below where
                    %% the function is typically a fun M:F/n
                    %% reference.  This case was added because
                    %% anonymous functions interfere with reloads.
                    Args = ContextArgs ++ [Sock,Port],
                    serv:enter({handler, fun() -> apply(Init, Args) end, Handle});
                {handler, Init, Handle}
                  when is_function(Init) ->
                    serv:enter({handler, fun() -> Init(Sock, Port) end, Handle});
                {body, Body} ->
                    serv:enter({body, fun() -> Body(Sock, Port) end})
            end
    end.


handle(Msg, {Env,_,_}=S) ->
    {Pids, State} = handle_(Msg, S),
    {Env, Pids, State}.

handle_({spawn_acceptor, LSock, Port}, {Env, Pids, State}) ->
    serv:start({body, fun() -> accept_loop(LSock, Port, Env) end}),
    {Pids, State};

handle_({register_connect, Pid}, {_Env, Pids, State}) ->
    {[Pid | Pids], State};

handle_({'EXIT', FromPid, Reason}, {#{handle := Handle}=_Env, Pids, State}) ->
    case lists:member(FromPid, Pids) of
        true -> 
            tools:info("child exit(~p,~p)~n", [FromPid, Reason]),
            NextState = Handle({'EXIT', FromPid, Reason}, State),
            {Pids -- [FromPid], NextState};
        false -> 
            case Reason of
                normal ->
                    ignore;
                _ ->
                    %% Since child will have a full error message,
                    %% this doesn't need one.
                    %% tools:info("linked exit(~p,~p), shutting down~n", [FromPid, Reason]),
                    tools:info("linked exit(~p), shutting down~n", [FromPid]),
                    self() ! shutdown
            end,
            {Pids, State}
    end;

handle_({apply, Fun}, {_Env, Pids, State}) ->
    Fun(Pids),
    {Pids, State};

handle_(shutdown, {#{lsocks := LSocks}=_Env, _Pids, _State}) ->
    [ gen_tcp:close(LSock) || LSock <- LSocks ],
    process_flag(trap_exit, false),
    exit(shutdown);

handle_(Msg, {#{handle := Handle}=_Env, Pids, State}) ->
    {Pids, Handle(Msg, State)}.
 





%% A simpler version 2.

%% Because of how accept works, we need to decouple the supervised
%% process from the accepting proces, such that the accepting process
%% can become the connection services process.
start_link(#{ port := SrcPort,
              opts := Opts } = Spec) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                process_flag(trap_exit, true),
                case gen_tcp:listen(
                      SrcPort, 
                      Opts
                      ) of
                    {ok, LSock} ->
                        State = 
                            maps:merge(
                              Spec,
                              #{ listen_sock => LSock }),
                        self() ! accept,
                        State;
                    Error ->
                        %% FIXME: This is just to reduce restart
                        %% pressure, while fixing another bug.
                        %% timer:sleep(2000),
                        exit(Error)
                end
        end,
        %% This is just a placeholder to serve debug info.
        fun ?MODULE:listener_handle/2})}.

listener_handle(accept, State) ->
    Listener = self(),
    spawn_link(
      fun() ->
              accept(maps:put(listener, Listener, State))
      end),
    State;

listener_handle({'EXIT', Pid, _Reason}, State) ->
    _Peer = maps:get(Pid, State),
    %% log:info("removing: ~999p~n", [{Pid,_Peer,_Reason}]),
    maps:remove(Pid, State);

listener_handle(Msg, State) ->
    obj:handle(Msg, State).


accept(#{listen_sock := LSock, 
         handle := Handle,
         listener := Listener,
         on_accept := Init
        } = ListenState) ->
    %% tools:info("accepting: ~p~n", [LSock]),
    {ok, SrcSock} = gen_tcp:accept(LSock),
    %% tools:info("accepted: ~p~n", [SrcSock]),
    obj:set(Listener, self(), inet:peername(SrcSock)),

    %% The process running accept will become the connection service
    %% process, so fork of a new acceptor.
    Listener ! accept,

    %% And fall into the service loop.
    ConnectState = Init(maps:put(sock, SrcSock, ListenState)),
    serv:receive_loop(ConnectState, Handle).



pids(Listener) ->
    lists:filter(
      fun is_pid/1,
      maps:keys(obj:dump(Listener))).

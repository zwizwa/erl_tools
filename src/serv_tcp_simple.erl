-module(serv_tcp_simple).
-export([start_link/1, 
         accept/1]).

%% Version 2 of serv_tcp, which is a little convoluted.

%% Because of how accept works, we need to decouple the supervised
%% process from the accepting proces, such that the accepting process
%% can become the connection services process.
start_link(#{ port := SrcPort,
              packet := Packet } = Spec) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                {ok, LSock} =
                    gen_tcp:listen(
                      SrcPort, 
                      [binary,
                       {packet, Packet},
                       {active, true},
                       {reuseaddr, true}]),
                State = 
                    maps:merge(
                      Spec,
                      #{ listen_sock => LSock }),
                spawn_link(fun() -> accept(State) end),
               State
        end,
        %% This is just a placeholder to serve debug info.
        fun obj:handle/2})}.

accept(#{listen_sock := LSock, 
         handle := Handle,
         on_accept := Init
        } = ListenState) ->
    %% tools:info("accepting: ~p~n", [LSock]),
    {ok, SrcSock} = gen_tcp:accept(LSock),
    tools:info("accepted: ~p~n", [SrcSock]),

    %% The process running accept will become the connection service
    %% process, so fork of a new acceptor.
    spawn_link(fun() -> accept(ListenState) end),

    %% And fall into the service loop.
    ConnectState = Init(maps:put(sock, SrcSock, ListenState)),
    serv:receive_loop(ConnectState, Handle).




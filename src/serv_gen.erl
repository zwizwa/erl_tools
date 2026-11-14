-module(serv_gen).
-behaviour(gen_server).
-export([start_link/2,
         init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% gen_server wrapper for serv:start({handler, Init, Handle})
%% - state is {Handle, State}, Handle(Msg, State) -> State
%% - raw messages and the obj call protocol go to Handle unchanged
%% - gen_server:call arrives as {{gen, From}, Req}, obj:reply handles it

start_link(Init, Handle) ->
    gen_server:start_link(?MODULE, {Init, Handle}, []).

init({Init, Handle}) ->
    {ok, {Handle, Init()}}.

handle_call(Req, From, {Handle, State}) ->
    {noreply, {Handle, Handle({{gen, From}, Req}, State)}}.

handle_cast(Msg, {Handle, State}) ->
    {noreply, {Handle, Handle(Msg, State)}}.

%% - reload: was serv:receive_loop's code upgrade hook
handle_info(reload, S) ->
    {noreply, S};
handle_info(Msg, {Handle, State}) ->
    {noreply, {Handle, Handle(Msg, State)}}.

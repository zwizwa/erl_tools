-module(ws).
%% Dialyzer complains about not knowing these behaviors, so comment out.
%%-behaviour(cowboy_http_handler).
%%-behaviour(cowboy_websocket_handler).

%% Upstream interface.
-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

%% Socket interaction and tools
-export([call/4,
         call_sequence/2, call_sequence_bert/2,
         call_wait/4,
         call_exml/4,
         call_wait_exml/4,

         info/3, info_ehtml/2, info_ehtml/3, info_ehtml/1,

         console/2,
         
         command/3, command/2,
         showhide_select/3,
         sockets/0,
         eval/2,

         %% Tools
         encode_id/1,
         timestamp/1,

         %% Query values as produced by ws.js
         form_list/2
]).


%% For Single Page Applications (SPA), a websocket connection is used.
%% Messages sent over this socket are JSON- or BERT-encoded.

%% SPAs are structured like this:
%% - initial web page contains SPA layout template and a call to ws_start
%% - ws_start initiates the Erlang side of the application once the websocket is up


%% Cowboy API
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
handle(Req, State) ->
    log:info("ws: not expected: ~p~n", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.
terminate(_Reason, _Req, _State) ->
    log:info("ws: terminate: ~p~n",[{_Reason, _Req, _State}]),
    ok.


%% Websocket API

websocket_init(_TransportName, Req, _Opts) ->
    {Agent,_} = cowboy_req:header(<<"user-agent">>,Req),
    %% It's useful to have these registered.
    %% log:set_info_name({ws,self()}),
    tools:register_suffix(ws, self()),
    log:info("ws: init ~p~n",[Agent]),

    {ok, Req, #{terminate => fun(_) -> ok end}}.

websocket_handle({text, Json}, Req, State) ->
    %% Interpret all incoming messages as JSON.
    EJson = json:decode(Json),
    NextState = handle_ejson(EJson, State),  %% Async only
    {ok, Req, NextState};
websocket_handle({ping,_}, Req, State) -> 
    {ok, Req, State};
websocket_handle({pong,_}, Req, State) -> 
    {ok, Req, State};
websocket_handle(Msg, Req, State) ->
    log:info("ws:websocket_handle ignore: ~p~n",[Msg]),
    {ok, Req, State}.


%% Messages sent to the websocket process.
%% supports obj.erl RPC protocol
websocket_info({Pid,_}=Msg, Req, State) when is_pid(Pid) ->
    {ok, Req, obj:handle(Msg, State)};

%% Raw data.
websocket_info({text, _}=Raw, Req, State)   -> {reply, Raw, Req, State};
websocket_info({binary, _}=Raw, Req, State) -> {reply, Raw, Req, State};
websocket_info({bert, Term}, Req, State)    -> {reply, {binary, term_to_binary(Term)}, Req, State};

%% Maps are interpreted as encoded JSON messages to be sent to the
%% websocket.  FIXME: change protocol to wrap this?
websocket_info(Map, Req, State) when is_map(Map)-> 
    {reply, {text, json:encode(Map)}, Req, State};

%% Anything else gets sent to the delegate handler.
websocket_info(Msg, Req, #{handle := Handle}=State) -> 
    {ok, Req, Handle({info, Msg}, State)};

%% Or ignored
websocket_info(Msg, Req, State) -> 
    log:info("ws:websocket_info: ignore: ~p~n",[Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #{terminate := Terminate}=State) ->
    log:info("terminate~n"),
    Terminate(State), ok;
websocket_terminate(_Reason, _Req, _State) ->
    log:info("ws:websocket_terminate: no terminate handler~n"),
    ok.


%% Generic websocket start routine.
%%
%% This uses a multi-step bootstrap:
%% - HTML with initial page layout calls into ws_start handler here, parameterized by code.
%% - We instantiate that code here, parameterized by the websocket connection
handle_ejson(#{type := <<"ws_start">>,
               args := StartHmac},
             State) ->
    %% log:info("ws_start: ~p~n", [StartHmac]),
    {ok, Start} = web:hmac_decode(StartHmac),
    {ok, InitState} = Start(),
    maps:merge(State, InitState);


%% All ws.js browser applications support the ws_action message, which
%% executes HMAC-authenticated Erlang callbacks (closures).  See
%% web:button/1, web:checkbutton/1, ...  and ws_input in ws.js
handle_ejson(#{type := <<"ws_action">>, action := CallbackHmac} = Msg,
             State) ->
    %% log:info("ws_action: ~p ~p~n", [CallbackHmac, Fun]),
    %% Already dispatched, so these keys are no longer needed.
    M1 = maps:remove(action, Msg),
    M2 = maps:remove(type, M1),
    {ok, Fun} = web:hmac_decode(CallbackHmac),
    %% log:info("ws_action: ~p ~p~n",[Fun,M2]),
    Fun(M2, State);

%% Date and time from browser.
handle_ejson(#{type := <<"ws_datetime">>, 
               args := [Year,Month,Day,Hour,Minute,Second]}, State) ->
    DateTime = {{Year,Month,Day},{Hour,Minute,Second}},
    log:info("datetime: ~p~n", [DateTime]),

    %% Don't change the machine's time base, but record current
    %% timestamp in machine clock and browser time to reconstruct
    %% timestamps.
    maps:put(datetime,
             {machine_now(),
              DateTime},
             State);

%% All other messages are forwarded to the registered handler, if any.
handle_ejson(Msg, #{handle := Handle} = State) ->
    Handle({ws, Msg}, State);

handle_ejson(Msg, State) ->
    log:info("ws:handle_ejson: ignore: ~p~n",[Msg]), State.



%% call

%% Basic call format - see ws.js and method_call.js
%% Indirections:
%% ID -> DOM element data-behavior attribute -> behavior object -> method function
call_fmt(ID,Method,Arg) ->
    #{ type   => call,
       id     => encode_id(ID),
       method => Method,
       arg    => Arg }.
    
%% Some Arg types are sent using Binary ERlang Term format for more
%% efficient encoding.
call_msg(ID,Method,{s16_le,_}=Arg) -> {bert, call_fmt(ID,Method,Arg)};
%% Default is JSON.
call_msg(ID,Method,Arg) -> call_fmt(ID,Method,Arg).

%% FIXME: check code and make sure no binary IDs are sent, then remove
%% case and use exml:encode_key
encode_id(ID) when is_binary(ID) -> ID;
encode_id(ID) -> type_base:encode({pterm,ID}).




%% A-synchronous messages send -- do not wait for reply.  See call.js
%% Some types are sent using Binary ERlang Term format.
call(Ws, ID, Method, Arg) -> Ws ! call_msg(ID,Method,Arg), ok.

%% Pass continuation to implement synchronous call.  Other side will
%% use cont as a ws_action.
call_wait(Ws, ID, Method, Arg) ->
    Ws ! maps:put(
           cont, cont_reply(self()),
           call_msg(ID,Method,Arg)),
    wait_reply().
cont_reply(Pid) ->
    web:hmac_encode(
      fun(Msg, State) -> Pid ! {cont_value, Msg}, State end).
wait_reply() -> 
    receive {cont_value, Msg} -> Msg end.


%% E.g to avoid repaints when sending gui updates.
call_sequence(Ws, Messages) ->
    Ws ! fmt_sequence(Messages), ok.

fmt_sequence(Messages) ->
    #{ type => bundle,
       messages =>
           [call_msg(ID,Method,Arg)
            || {ID,Method,Arg} <- 
                   lists:flatten(Messages)] }.

call_sequence_bert(Ws, Messages) ->
    Ws ! {bert, fmt_sequence(Messages)}, ok.

%% Convenient shorthand for routines that expect innerHTML, which
%% needs to be encoded as binary.
call_exml(Ws, ID, Method, Els) ->
    call(Ws,ID,Method,exml:to_binary(Els)).
call_wait_exml(Ws, ID, Method, Els) ->
    call_wait(Ws,ID,Method,exml:to_binary(Els)).
    
              


%% Format message and send it over websocket to browser.  See ws.js
info_text(Ws, Text, Opts) ->      
    Bin = iolist_to_binary(Text),
    List = binary_to_list(Bin),
    UTF8 = unicode:characters_to_binary(List,latin1,utf8),
    call(Ws, live_log, append_text, [UTF8, Opts]).

info(Ws, Fmt, Args) ->
    info(Ws, Fmt, Args, #{}).
info(Ws, Fmt, Args, Opts) ->
    %% io:format("~p~n",[{Ws,Fmt,Args}]),
    info_text(Ws, io_lib:format(Fmt, Args), Opts).

%% Same, but for ehtml.
info_ehtml(Ws, Ehtml) ->
    info_ehtml(Ws, Ehtml, #{}).
info_ehtml(Ws, Ehtml, Opts) ->
    Bin = exml:to_binary([Ehtml]), %% FIXME: this can fail
    call(Ws, live_log, append_html, [Bin, Opts]).

%% Curried
info_ehtml(Ws) ->
    fun(Ehtml) -> info_ehtml(Ws, Ehtml) end.


%% Wrap text, ehtml as an io process.
console(Ws, Opts) ->
    log:info("console: ~p~n",[Opts]),
    log:format_to_io(
      fun() -> log:set_info_name({tools:annotate_pid(Ws), console}) end,
      fun(_, Fmt, Args) ->
              case log:format_record_ehtml([none, Fmt, Args]) of
                  {ehtml, Ehtml} -> info_ehtml(Ws, Ehtml, Opts);
                  {text, Text}   -> info_text(Ws, Text, Opts)
              end
      end).

%% Evaluate javascript code.
eval(Ws, Js) when is_binary(Js) ->
    Ws ! #{
      type => eval,
      code => Js,
      cont => cont_reply(self())
     },
    wait_reply();
eval(Ws, Js) -> eval(Ws, iolist_to_binary(Js)).


%% FIXME: how to get actual list of active websockets?
sockets() ->
     [ws0,ws1,ws2,ws3,ws4]. 

%% Run shell commands, streaming output to websocket log.
port_handle(Ws, Port) ->
    receive
        {Port, {data,{eol,Line}}} ->
            info(Ws,"~s~n",[Line]),
            port_handle(Ws, Port);
        Other ->
            log:info("port: ~p~n", [Other])
    end.
command(Ws, Fmt, Args) ->
    command(Ws, tools:format(Fmt,Args)).
command(Ws, Command) ->
    port_handle(
      Ws,
      open_port({spawn, Command},
                [{line, 1024}, binary, use_stdio, exit_status])).
    

%% datetime format.
machine_now() ->
    calendar:now_to_datetime(erlang:timestamp()).
    
%% Timestamp relative to browser time.
timestamp(Ws) ->
    {Machine0, Browser0} = obj:get(Ws, datetime),
    Machine1 = machine_now(),
    D2G = fun calendar:datetime_to_gregorian_seconds/1,
    calendar:gregorian_seconds_to_datetime(
      D2G(Browser0) + D2G(Machine1) - D2G(Machine0)).

%% See widget.js showhide
showhide_select(Ws,ID,Name) ->                            
    call(Ws,ID,select,type:encode({pterm,Name})).



%% Input and form data.


    


%% User data is represented as a triplet: key, type, value.  These can
%% be represented in decoded or encoded form, based on the
%% functionality exposed by a type module (an extension of type_base).
%% In the browser, the encoded triplet form is used represented as an
%% array of 3 strings, in Erlang, the decoded ktv() form is used.  For
%% the counterpart, see ws.js

%% In practice these are printable terms, using 'pterm' type from type_base.erl
-type key() :: _.
-type type() :: _.

%% Abbreviations: 
%% KTV : key, type, value
%% KTB : key, type, binary

-type ktb() :: {key(),{type(), binary()}}.
-type ktv() :: {key(),{type(), any()}}.



%% Unpack, but don't parse data yet.
%% Input format is generated by form_field() in ws.js
-spec ktb(atom(), [binary()]) -> ktb().
ktb(Types, [BinKey,BinType,BinVal]) ->
    {Types:decode({pterm,BinKey}),
     {Types:decode({pterm,BinType}),
      BinVal}}.

-spec form_list(atom(), _) -> [ktv()].
form_list(Types, EJson) ->

    %% Convert in two steps.  First step which reconstructs variable
    %% and type forms should not fail -- no user input is involved.
    KTBs = [ktb(Types, Binding) || Binding <- maps:get(form, EJson)],

    %% The values are typed in by the user, so convert them one by
    %% one. This way it is known which variable fails parsing.
    KTVs = 
        [try {Key, {Tag, Types:decode(TB)}}
         catch {type, Err} -> throw({bad_value, Key, Err}) end
         || {Key,{Tag,_}=TB} <- KTBs],
    KTVs.


%% FIXME: move spa_edit.erl input parser code here.

    

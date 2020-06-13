%% Forth-like stack language interpreter to sit at the client side of
%% a WebSocket.  This code is self-contained.  It implements its own
%% HTTP bootstrapping.

%% This could be used stand-alone for ad-hoc web app development, but
%% is mainly here to support exml_dyn (towards a simple FRP-style web
%% framwork).

%% Uses: exml, serv_ws, serv_tcp, jsone

%% Startup procedure: Initial page request serves html, which loads
%% stack_ws.js, which opens the websocket and falls into user-provided
%% code in ws_up member (see serv_ws).  I don't think I want to mess
%% with initial page load containing any functional html.  Just use
%% the websocket to start an application and let it push html through
%% the websocket.

%% At the app end, we want a single callback that is invoked whenever
%% a websocket is created.  The serving of bootstrap code is of no
%% interest to the app.

-module(stack_ws).
-export([start_link/1,
         on_accept/1, handle/2,
         req/2, query/1, 
         js/0, html/1, css/0
        ]).

-define(JS,<<"/stack_ws.js">>).
-define(CSS,<<"/style.css">>).

start_link(Init = #{port := _}) ->
    serv_tcp:start_link(
      maps:merge(
        serv_ws:defaults(),
        maps:merge(
          #{ req       => fun ?MODULE:req/2,
             on_accept => fun ?MODULE:on_accept/1,
             handle    => fun ?MODULE:handle/2 },
          Init))).

on_accept(State = #{sock := Sock}) -> 
    {ok, PN} = inet:peername(Sock),
    log:set_info_name({?MODULE, PN}),
    State1 = maps:put(key, crypto:strong_rand_bytes(32), State),
    serv_ws:on_accept(State1).

handle(Msg, State) ->
    %% debug tap point
    %% log:info("auth_serv:handle: ~p~n", [Msg]),
    handle_(Msg, State).

%% Websocket data.  We want to get rid of json as soon as possible.
%% KTV seems to be best: assume Key and Type are in pterm format, then
%% use type:decode on the Value.
handle_({data, #{ data := Data }}, State = #{ key := Key }) ->
    %% log:info("stack_ws: data: ~p~n", [Data]),
    Handle =
        maps:get(
          ws_msg, State,
          fun(T,S) -> log:info("stack_ws: ws_msg: ~p~n", [T]), S end),
    try
        case jsone:decode(iolist_to_binary(Data)) of

            %% Keep the default format as generic as possible: a pair
            %% of destination and data, both accompanied by a
            %% deserialization type tag.  Use arrays because they are
            %% more conveniently generated at the javascript end.
            [[DstType, DstVal],
             [MsgType, MsgVal]]
            when is_binary(DstType), is_binary(MsgType) ->
                Handle({decode(Key,DstType,DstVal),
                        decode(Key,MsgType,MsgVal)},
                       State);

            %% Some ad hoc locally handled messages.
            [<<"log">>,[Type,Val]] when
                  is_binary(Type) ->
                log:info("dbg: ~p~n", [decode(Key,Type,Val)]),
                State
        end
    catch
        C:E ->
            log:info("stack_ws: can't parse: ~p: ~p~n", [Data,{C,E}]),
            State
    end;

handle_({cmd, Program}, State) ->
    Compiled = compile(State, Program),
    JSON = jsone:encode(Compiled),
    %% log:info("JSON: ~s~n",[JSON]),
    serv_ws:handle({send, JSON}, State);

handle_({def, Name, Program}, State) ->
    Compiled = compile(State, Program),
    Compiled1 = compile(State, [[Name, Compiled], def]),
    %% log:info("Compiled1: ~p~n",[Compiled1]),
    JSON = jsone:encode(Compiled1),
    serv_ws:handle({send, JSON}, State);

%% Note that we have a lot of protocols arriving at the same handler.
%% It seems unwise to add another catch-all delegate in serv_ws.erl,
%% so we require the routing to happen before we delegate to serv_ws.
%% For now this is only used for epids, which is easy to tap.
handle_({epid_send,_,_}=Msg, State) ->
    EpidHandle = maps:get(epid_handle, State),
    EpidHandle(Msg, State);

handle_(Msg, State) ->
    %% log:info("serv_ws:handle: ~p~n", [Msg]),
    serv_ws:handle(Msg,State).

%% Use serialization from type.erl with a couple of exceptions: hmac
%% decoding needs the websocket's key, and allow for json escape as
%% well.
%% panel_dyn:test({cmd,[words, <<"json">>, tag, <<"log">>, tag, send]}).
%% panel_dyn:test({cmd,[{hmac,123}, <<"log">>, tag, send]}).

decode(Key,BT,BV) ->    
    T = type:decode({pterm,BT}),
    case T of
        json -> BV;
        hmac -> hmac_decode(Key, BV);
        _    -> type:decode({T,BV})
    end.
    


%% Ecode/decode authenticated messages.
hmac(Key,Bin) when is_binary(Bin) -> 
    crypto:hmac(sha256,Key,Bin).
hmac_encode(Key,Obj) ->
    Bin = term_to_binary({erlang:system_time(),Obj}),
    Hmac = hmac(Key,Bin),
    base64:encode(term_to_binary({Bin,Hmac})).
hmac_decode(Key,Base64) ->
    {Bin,Hmac} = erlang:binary_to_term(base64:decode(Base64),[safe]),
    case hmac(Key,Bin) of
        Hmac  ->
            {_Ts, Val} = erlang:binary_to_term(Bin),
            %% FIXME: Validate timestamp
            {ok, Val};
        Hmac1 ->
            {error, {hmac_fail, Hmac, Hmac1}}
    end.



%% Javascript code implements the Forth machine.
%%
%% - DOM access routines are implemented as stack operation primitives (s_op)
%% - Messages sent to the websocket consist JSON-encoded programs
%% - A program is a lists of machine operations (m_op)
%% - An m_op is either a literal load, or a stack op
%% - Programs have an erlang representation (compile/1).
%% - Erlang side supports macros


js() ->
<<"
var s = [];
function s_app1(f)    { s.push(f(s.pop())); }
function s_mapp1(o,m) { s.push(o[m](s.pop())); }

var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host + '/ws');
ws.onclose   = function()    { console.log('ws: close'); }
ws.onopen    = function()    { console.log('ws: open'); }
ws.onmessage = function(msg) {
    if (msg.data instanceof ArrayBuffer) { exec([['l',msg.data]]); }
    else { exec(JSON.parse(msg.data)); }
}
function send(o) {
    ws.send(JSON.stringify(o));
}
function render(html) {
    var tmp = document.createElement('div');
    tmp.innerHTML=html;
    return tmp.firstChild;
}
function insert(parent, child) {
    var c = parent.children;
    for(var i=0;i<c.length;i++) {
        if(child.id < c[i].id) {
            return parent.insertBefore(child,c[i]);
        }
    }
    parent.appendChild(child);
}
var s_op = {
    drop()      { s.pop(); },
    app1()      { s_app1(s.pop()); },
    mapp1()     { var m=s.pop(); s_mapp1(s.pop(),m); },
    print()     { console.log(s.pop()); },
    send()      { send(s.pop()); },
    ref()       { s_mapp1(document,'getElementById'); },
    replace()   { var old = s.pop(); old.parentNode.replaceChild(s.pop(),old); },
    body()      { document.body.innerHTML = s.pop(); },
    insert()    { var parent = s.pop(); insert(parent, s.pop()); },
    delete()    { var el = s.pop(); el.parentNode.removeChild(el); },
    render()    { s_app1(render); },
    words()     { var ws = []; for(var w in s_op) { ws.push(w); }; s.push(ws); },
    pathd()     { var path = s.pop(); path.setAttribute('d',s.pop()); },
    exec()      { exec(s.pop()); },
    tag()       { var t = s.pop(); var val = s.pop(); s.push([t, val]); },
    def()       { var d = s.pop(); s_op[d[0]] = function() { exec(d[1]); }; },
    // debug
    eval()      { s_app1(eval); },
    log()       { console.log(s); },
    clear()     { s = []; },
};
var m_op = {
    l(arg)  { s.push(arg); },
    s(name) { s_op[name](); },
};
function exec(prog) {
    for(var i=0;i<prog.length;i++) {
         var ins = prog[i];
         var opc = ins[0];
         var arg = ins[1];
         m_op[opc](arg);
    }
}
">>.


compile(Env = #{ key := Key }, Prog) ->
    lists:append( %% Splice
      lists:map(
        fun(Op) when is_atom(Op) ->
                case macro(Env, Op) of
                    {ok, Prog1} -> compile(Env, Prog1);
                    error -> [[s, Op]]
                end;
           ({hmac,Lit}) ->
                [[l, [hmac, hmac_encode(Key, Lit)]]];
           (Lit) ->
                [[l, Lit]]
        end,
        Prog)).

%% Note: macros could be compiled and moved to target as well.
macro(_Env = #{ macros := Macros }, Name) ->
    maps:find(Name, Macros);
macro(_Env, _Name) ->
    error.






%%
css() ->
<<"    
body {
    background-color: black; 
    color: white;
    font-family: Monospace;
    font-size: 20px;
}
a {
    color: white;
}
">>.

%% Simple query parser.
query(URI) ->
    try 
        [Path, QString] = re:split(URI,"\\?"),
        Bindings = re:split(QString,"&"),
        PropList = [{K,V} || [K,V] <- [re:split(B,"=") || B <- Bindings]],
        {Path,maps:from_list(PropList)}
    catch _C:_E ->
            %% log:info("auth_serv: query: ~999p~n",[{_C,_E}]),
            {iolist_to_binary(URI),#{}}
    end.


%% Any other path aside from /ws is handled here.
req({{abs_path, URI}, _Headers}, Env) ->

    %% The Env variable contains.
    {Path,_Q} = query(URI),
    %% log:info("Q: ~p ~p~n",[Path,Q]),

    case Path of
        ?JS ->
            resp(<<"text/javascript">>, ?MODULE:js());
        ?CSS ->
            resp(<<"text/css">>, ?MODULE:css());
        <<"/">> ->
            Body = maps:get(ws_body, Env, fun(_) -> [] end),
            %% ReloadHack = tools:format("?v=~p",[rand:uniform()]),
            ReloadHack = "",
            Script = 
                {'script',
                 [{type,<<"text/javascript">>},
                  {charset,<<"UTF-8">>},
                  {src, ?JS}],
                 [[<<" ">>]]},  %% WTF?
            Style =
                {'link',
                 [{rel,"stylesheet"},
                  {type,"text/css"}, 
                  {href, [?CSS,ReloadHack]}],
                 []},
            resp(
              <<"text/html">>,
              iolist_to_binary(
                exml:exml(
                  html(#{ script => Script,
                          style => Style,
                          body => Body(Env) }))));
        Other ->
            Bin = tools:format_binary("404: ~s~n", [Other]),
            {ok,
             [<<"HTTP/1.1 404 Not Found\r\n",
                "Content-Type: text/plain\r\n",
                "Content-Length: ">>, integer_to_list(size(Bin)),
              <<"\r\n\r\n">>,
              Bin]}
    end.

resp(Type,Bin) ->
    Resp=
        {ok,
         [<<"HTTP/1.1 200 OK\r\n",
            "Content-Type: ">>,Type,<<"\r\n",
            "Content-Length: ">>, integer_to_list(size(Bin)),
          <<"\r\n\r\n">>,
          Bin]},
    %% log:info("~p~n", [Resp]),
    Resp.
    

    
%% <script ... /> doesn't parse properly in Firefox, but
%% <script></script> does.  Adding a space inside the element fixed
%% it.  Weird...  Firefox bug?

html(#{script := Script,
       style  := Style,
       body   := Body}) ->
    {'html', [{xmlns,<<"http://www.w3.org/1999/xhtml">>}],
     [{'head',[],
       [{'meta',[{charset,<<"UTF-8">>}],[]},
        Script, Style,
        {'title',[],[[<<"stack_ws">>]]}
       ]},
      {'body',[{id,body}], Body}]}.



%% .. ! {cmd, [1,2,3,print]}.
%% .. ! {cmd, [reset,<<"main">>,<<"document">>,eval,<<"getElementById">>,mapp1,print]}.




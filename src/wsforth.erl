%% Forth-like interpreter to sit at the client side of a WebSocket.

%% Attempt to write a more minimal framework than ws.erl

%% This is stand-alone, not using Cowboy.

%% Initial template derived from auth_serv.erl
%% Uses: exml, serv_ws, serv_tcp, jsone
%% Wraps Javascript a Forth-like stack machine controlled over websocket

-module(wsforth).
-export([start_link/1,
         on_accept/1, handle/2,
         req/2, query/1, 
         js/0, html/1, css/0
        ]).

-define(JS,<<"/wsforth.js">>).
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
    serv_ws:on_accept(State).

handle(Msg, State) ->
    %% debug tap point
    %% log:info("auth_serv:handle: ~p~n", [Msg]),
    handle_(Msg, State).

handle_({data, #{ data := JSON }}, State) ->
    %% websocket data input
    Term = jsone:decode(iolist_to_binary(JSON)),
    Handle = maps:get(
               ws_ejson, State,
               fun(T,S) -> log:info("ejson: ~p~n", [T]), S end),
    Handle(Term,State);

handle_({cmd, Program}, State) ->
    %% Convert the Erlang representation into JSON, tagging data and
    %% code separately.  Atoms refer to stack ops.  Anything else is
    %% literal.
    TaggedProgram =
        lists:map(
          fun(Op) when is_atom(Op) -> [s, Op];
             (Op) -> [l, Op]
          end,
          Program),
    JSON = jsone:encode(TaggedProgram),
    %% log:info("JSON: ~s~n",[JSON]),
    serv_ws:handle({send, JSON}, State);

handle_(Msg, State) ->
    %% log:info("serv_ws:handle: ~p~n", [Msg]),
    serv_ws:handle(Msg,State).


%% Javascript code implements the Forth machine.
%% - a program is a list of machine ops encoded in 2-element array: [<opcode>,<argument>]
%% - the two machine ops distinguish between data (literal stack load) and code (stack op)
%% - the stack ops manipulate the data stack

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
var s_op = {
    drop()    { s.pop(); },
    eval()    { s_app1(eval); },
    app1()    { s_app1(s.pop()); },
    mapp1()   { var m=s.pop(); s_mapp1(s.pop(),m); },
    print()   { console.log(s.pop()); },
    send()    { send(s.pop()); },
    ref()     { s_mapp1(document,'getElementById'); },
    replace() { var old = s.pop(); old.parentNode.replaceChild(s.pop(),old); },
    render()  { s_app1(render); },
    // debug
    log()     { console.log(s); },
    clear()   { s = []; },
    exec()    { exec(s.pop()); }
};
var m_op = {
    l(arg) { s.push(arg); },
    s(arg) { s_op[arg](); },
};
function exec(prog) {
    var i;
    for(i=0;i<prog.length;i++) {
         var ins = prog[i];
         var opc = ins[0];
         var arg = ins[1];
         m_op[opc](arg);
    }
}
">>.


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
            resp(<<"text/javascript">>, wsforth:js());
        ?CSS ->
            resp(<<"text/css">>, wsforth:css());
        <<"/">> ->
            Body = maps:get(ws_body, Env, fun(_) -> [] end),
            ReloadHack = tools:format("?v=~p",[rand:uniform()]),
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
        {'title',[],[[<<"webredo">>]]}
       ]},
      {'body',[], Body}]}.



%% webredo_test ! {cmd, [1,2,3,print]}.
%% webredo_test ! {cmd, [reset,<<"main">>,<<"document">>,eval,<<"getElementById">>,mapp1,print]}.



%% Startup procedure: Initial request serves html, which loads
%% wsforth.js, which opens the websocket and requests a particular
%% program to start.  I don't think I want to mess with initial page
%% load containing any functional html.  Just use the websocket to
%% start an application.

%% At the app end, we want a single callback that is invoked whenever
%% a websocket is created.  The serving of bootstrap code is of no
%% interest to the app.


%% var a = A.parentNode.replaceChild(document.createElement("span"), A);

%% Forth-like interpreter to sit at the client side of a WebSocket.

%% Attempt to write a more minimal framework than ws.erl
%% Initial template derived from auth_serv.erl
%% Uses: exml, serv_ws, serv_tcp, jsone
%% Wraps Javascript a Forth-like stack machine controlled over websocket


-module(wsforth).
-export([start_link/1,
         on_accept/1, handle/2,
         req/1, query/1, 
         js/0, html/0
]).

start_link(Init) ->
    serv_tcp:start_link(
      maps:merge(
        serv_ws:defaults(),
        maps:merge(
          Init,
          #{ req       => fun ?MODULE:req/1,
             on_accept => fun ?MODULE:on_accept/1,
             handle    => fun ?MODULE:handle/2 }))).

on_accept(State = #{ sock := Sock, listener := Listener }) ->
    {ok, PN} = inet:peername(Sock),
    log:set_info_name({?MODULE, PN}),
    obj:set(Listener, self(), PN),
    %% log:info("on accept: ~p~n",[State]),
    register(webredo_test, self()),
    serv_ws:on_accept(State).

handle(Msg, State) ->
    %% debug tap point
    %% log:info("auth_serv:handle: ~p~n", [Msg]),
    handle_(Msg, State).

handle_({data, Data}, State) ->
    %% websocket data input
    log:info("data: ~p~n", [Data]),
    State;

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
    log:info("JSON: ~s~n",[JSON]),
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
console.log('wsforth.js');
var proto = ({'https:': 'wss://', 'http:': 'ws://'})[window.location.protocol];
var ws = new WebSocket(proto + location.host + '/ws');
ws.onclose = function() {
    console.log('ws.onclose');
}

var s = [];
function s_app1(f)    { s.push(f(s.pop())); }
function s_mapp1(o,m) { s.push(o[m](s.pop())); }
var s_op = {
    drop()  { s.pop(); },
    eval()  { s_app1(eval); },
    app1()  { s_app1(s.pop()); },
    mapp1() { var m=s.pop(); s_mapp1(s.pop(),m); },
    // debug
    log()   { console.log(s); },
    clear() { s = []; },
    exec()  { exec(s.pop()); }
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
ws.onmessage = function (msg) {
    var prog = JSON.parse(msg.data);
    exec(prog);
};
">>.


%% Simple query parser.
query(URI) ->
    try 
        [_Path, QString] = re:split(URI,"\\?"),
        Bindings = re:split(QString,"&"),
        PropList = [{K,V} || [K,V] <- [re:split(B,"=") || B <- Bindings]],
        maps:from_list(PropList)
    catch _C:_E ->
            %% log:info("auth_serv: query: ~999p~n",[{_C,_E}]),
            #{}
    end.


%% Any other path aside from /ws is handled here.
req({{abs_path, Path}, _Headers}) ->

    Q = query(Path),
    log:info("Q: ~p ~p~n",[Path,Q]),

    %% Check for a valid 'use' token
    case Path of
        "/wsforth.js" ->
            resp(<<"text/javascript">>, wsforth:js());
        _ ->
            resp(<<"text/html">>, ?MODULE:html())
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

html() ->
    iolist_to_binary(
      exml:exml(
        {'html', [{xmlns,<<"http://www.w3.org/1999/xhtml">>}],
         [{'head',[],
           [{'meta',[{charset,<<"UTF-8">>}],[]},
            {'script',
             [{type,<<"text/javascript">>},
              {charset,<<"UTF-8">>},
              {src,<<"/wsforth.js">>}],
             [[<<" ">>]]},  %% WTF?
            {'title',[],[[<<"webredo">>]]}
           ]},
          {'body',[{id,<<"main">>}],
           [[<<"Loading..">>]
           ]}]})).

%% webredo_test ! {cmd, [1,2,3,print]}.
%% webredo_test ! {cmd, [reset,<<"main">>,<<"document">>,eval,<<"getElementById">>,mapp1,print]}.

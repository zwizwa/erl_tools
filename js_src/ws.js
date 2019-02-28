var ws = new Object();
var config = { timeout: 5000 };
var last_evt;

// Use dependency injection for Bert
var bert_module = require('./bert');
var widgets = require('./widgets');
var bert_deps = { UTF8Decoder: window['TextDecoder'] };
var bert = new bert_module.Bert({ UTF8Decoder: window['TextDecoder'] });

var tools = require('./tools');

var ws_console = {
    log: function() {
        send({log: arguments});
    }
}

function error(errmsg) {
    console.log(errmsg);
    return {error: errmsg};
}


// Incluses protocol config.
function send_bert(msg) {
    ws.send(new Uint8Array(bert.encode(msg)));
}
function send_json(msg) {
    ws.send(JSON.stringify(msg));
}
var send_config = send_json;
function send(msg) {
    send_config(msg);
}


// Open websocket and pass in representation of server-side start code.
// After that, handle messages coming in from websocket.
function start_bert(args, method_call) {
    send_config = send_bert;
    start(args, method_call);
}
function start(args, method_call) {

    var handle;

    /* We support these messages. */
    var handlers = {
        // Handlers for ws-level messages
        set_cookie: function(msg) {
            document.cookie = msg.cookie;
        },
        reload: function(msg) {
            window.location.reload();
        },
        set_config: function(msg) {
            config[msg.key] = msg.value;
        },
        ping: function(msg) {
            return 'pong';
        },
        eval: function(msg) {
            return eval(msg.code);
        },
        bundle: function(msg) {
            tools.each(msg.messages, handle);
        },
        redirect_console: function(msg) {
            console = ws_console;
        },
        // Call a DOM object with mixed in behavior
        call: method_call
    };

    handle = function(msg) {
        var rpl = {
            type: "ws_action",
            action: msg.cont
        };
        //console.log(msg.cont);
        try {
            rpl.ok = handlers[msg.type](msg);
        }
        catch(e) {
            rpl.error = e;
            rpl.error_str = e.toString();
        }
        if (rpl.action) {
            try { send(rpl); }
            catch(e) {
                rpl.error = "JSON"
                rpl.error_str = e.toString();
                send(rpl);
            }
        }
        else if (rpl.error) {
            console.log(rpl.error);
        }
    };


    /* Websocket */
    if (!("WebSocket" in window)) {
        alert("WebSocket NOT supported by your Browser!");
        return;
    }

    if (window.location.protocol == "https:") {
        ws = new WebSocket("wss://"+window.location.host+"/ws");
    }
    else {
        ws = new WebSocket("ws://"+window.location.host+"/ws");
    }

    ws.binaryType = "arraybuffer"; // default is "blob"
    ws.onopen = function() {
        console.log("ws.onopen");
        var msg = {type: "ws_start",
                   args: args};
        send(msg);
        send_datetime();
    }
    ws.onmessage = function (evt) {
        //console.log(evt.data);
        last_evt = evt

        if ("{" == evt.data[0]) {
            // JSON
            var msg = JSON.parse(evt.data);
            //console.log('jsonmsg',typeof(msg));
            handle(msg);
        }
        else if (evt.data instanceof ArrayBuffer) {
            // BERT
            var msg = bert.decode(evt.data);
            // console.log('bertmsg',msg);
            handle(msg);
        }
        else {
            console.log(['unknown format:', evt.data]);
        }
    };
    ws.onerror = function(evt) {
        console.log("ws.onerror:");
        console.log(evt);
    }
    ws.onclose = function() {
        console.log("ws.onclose:");
        // It's been long a puzzle what to do here.  Reloading
        // automatically after a timeout is not a good idea.  Changing
        // the layout to notify the user isn't generic either.  Let's
        // settle on reloading whenever an event is sent.
        ws.send = function() {
            window.location.reload();
        }
        // Optionally, replace a node in the page with different
        // layout. Add a delay here such that a reload initiated by
        // the other side does not clear the display.
        var el = document.getElementById("onclose_cell");
        if (el) {
            var html = el.getAttribute("onclose_html");
            if (html) {
                setTimeout(
                    function() { el.innerHTML = html; },
                    500);
            };
        };
    };
    return ws;
}

function send_datetime() {
    var date = new Date();
    var msg = {
        type: 'ws_datetime',
        args: [
            date.getFullYear(),
            date.getMonth() + 1, // 0-11 + 1
            date.getDate(),
            date.getHours(),
            date.getMinutes(),
            date.getSeconds()
        ]
    };
    send(msg);
}
// JavaScript event handlers for DOM inputs and forms. This sends the
// content of the form or single input to Erlang over websocket.
function send_input(action, el) {
    var msg = { type: "ws_action",
                action: action,
                form: widgets.tools.form_data(el) };
    send(msg);
}
function send_event(action, event) {
    var msg = {
        type: "ws_action",
        action: action,
        form: [[event.type,"pterm",
                "{"+event.clientX+","+event.clientY+"}"]]
    };
    send(msg);
}


// API
exports.start      = start;
exports.start_bert = start_bert;

exports.send       = send;
exports.send_input = send_input;
exports.send_event = send_event;

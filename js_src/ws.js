var ws = new Object();
var last_evt;

// Use dependency injection for Bert
var bert_module = require('./bert');
var bert_deps = { UTF8Decoder: window['TextDecoder'] };
var bert = new bert_module.Bert({ UTF8Decoder: window['TextDecoder'] });


// Sending input and form data back to Erlang is done as an array of
// 3-element arrays of strings, encoding:
//
// - key
// - type
// - value
//
// These are then converted back to Erlang by the type.erl module.
//
// See web:form_data/1
// {Name=atom(),{Type=atom(),Value=binary()}}
//
function form_field(input) {
    return [input.name,                          // key
            input.getAttribute('data-decoder'),  // type
            input_value(input)];                 // value
}
// Convert input's value to string based on kind of input.
function input_value(el) {
    if (el.type == 'checkbox') {
        return el.checked ? 'true' : 'false';
    }
    else if (el.type == 'select-one') {
        var opts = el.options;
        return opts[opts.selectedIndex].value;
    }
    else {
        return el.value;
    }
}


// JavaScript event handlers for DOM inputs and forms. This sends the
// content of the form or single input to Erlang over websocket.
function input(action, el) {
    var msg = { type: "ws_action", action: action };
    // console.log('el',el);

    // Form returns list of fields
    if ('form' == el.nodeName) {
        msg.form= [];
        for (i=0; i<el.elements.length; i++) {
            var input = el.elements[i];
            if (input.name) {
                msg.form.push(form_field(input));
            }
        }
    }
    // A single imput returns list with one field
    else {
        if (el.name) {
            msg.form = [form_field(el)];
        }
    }
    //console.log('msg', msg);
    send(msg);
}


function error(errmsg) {
    console.log(errmsg);
    return {error: errmsg};
}



// Open websocket and pass in representation of server-side start code.
// After that, handle messages coming in from websocket.
function start(args, method_call) {

    /* We support these messages. */
    var handlers = {
        method_call: method_call,
        set_cookie: function(msg) {
            document.cookie = msg.cookie;
        },
        reload: function(msg) {
            window.location.reload();
        },
        ping: function(msg) {
            return 'pong';
        },
        eval: function(msg) {
            return eval(msg.code);
        }
    };
    function try_catch(handler, msg) {
        try { return handler(msg) }
        catch(e) { return e.toString(); }
    }


    var handle = function(msg) {
        var handler = handlers[msg.type];
        var rpl = {
            type: "ws_action",
            action: msg.cont,
            arg: handler ?
                try_catch(handler, msg) :
                error(['unknown message type', msg.type])
        }
        if (rpl.action) {
            try { send(rpl); }
            catch(e) {
                // JSON encoding problem
                rpl.arg = e.toString();
                send(rpl);
            }
        }
    };


    /* Websocket */
    if (!("WebSocket" in window)) {
        alert("WebSocket NOT supported by your Browser!");
        return;
    }
    ws = new WebSocket("ws://"+window.location.host+"/ws");
    ws.binaryType = "arraybuffer"; // default is "blob"
    ws.onopen = function() {
        console.log("ws.onopen");
        var msg = {type: "ws_start",
                   args: args};
        send(msg);
        send_datetime();
    }
    ws.onmessage = function (evt) {
        // console.log(evt.data);
        last_evt = evt
        if ("{" == evt.data[0]) {
            // JSON
            var msg = JSON.parse(evt.data);
            //console.log('jsonmsg',msg);
            handle(msg);
        }
        else if (evt.data instanceof ArrayBuffer) {
            // BERT
            var msg = bert.decode(evt.data);
            //console.log('bertmsg',msg);
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
        console.log("ws.onclose");
        //document.body.style.background = 'grey';
        setTimeout(function(){
            window.location.reload();
        }, 5000); // fixme: this should be more than worst case load time of any page.
    };
    return ws;
}

function send(msg) {
    ws.send(JSON.stringify(msg));
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


// API
exports.start = start;
exports.send  = send;
exports.input = input;

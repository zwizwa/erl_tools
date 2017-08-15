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
function input_value(el) {
    if (el.type == 'checkbox') {
        return el.checked ? 'true' : 'false';
    }
    else if (el.type == 'select-one') {
        return el.options[el.options.selectedIndex].value;
    }
    else {
        return el.value;
    }
}


// JavaScript event handlers for DOM inputs and forms This sends the
// content of the form or input to Erlang over websocket.
function input(action, el) {
    var msg = { type: "ws_action", action: action };
    // console.log('el',el);

    // Copy some properties from the DOM object
    if ('form' == el.nodeName) {
        msg.form= [];
        for (i=0; i<el.elements.length; i++) {
            var input = el.elements[i];
            if (input.name) {
                msg.form.push(form_field(input));
            }
        }
    }
    else {
        // FIXME: this is no longer used.  Use the 'form' property
        // instead.
        copy_fields(el, msg);

        // Represent this as a single-field form to be able to reuse
        // the form routines server side.
        if (el.name) {
            msg.form = [form_field(el)];
        }
    }
    //console.log('msg', msg);
    send(msg);
}
function copy_fields(el, msg) {
    ['value', 'id', 'checked', 'name']
        .forEach(function(tag) { msg[tag] = el[tag]; });
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
        }
    };
    var handle = function(msg) {
        var handler = handlers[msg.type];
        if (!handler) {
            console.log(['unknown message', msg.type]);
            return;
        }
        handler(msg);
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

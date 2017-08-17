// TL;DR.  An object is a DOM element associated to some behavior.

// To make Erlang and DOM/JavaScript work together, DOM elements are
// thought of as objects with internal state and "class" behavior.
//
//
// - Erlang can send JSON or BERT messages (see ws.js) that are
//   represented as JavaScript objects
//
// - The message always contains a property 'type', the value of which
//   is used to look up a handler.  Think of these as "static
//   methods", as they do not use object state.  See ws.js
//
// - If the message property 'type' is 'method_call', the indirection
//   mechanism defined in this file.  It associates state (a DOM
//   element) to behavior.
//
// - The message contains a second property 'id' which is used to find
//   the DOM element.
//
// - The DOM element contains a property 'data-behavior' which is used to
//   find the behavior associated to this object.  Think of that as
//   the object's "class".
//
// - Once the DOM element and the type (class) object have been found,
//   the 'method' object in the original message is used to index the
//   class object, returning a function.
//
// - This function is executed, passing in the DOM element and the
//   contents of the 'arg' property in the original message.
//
// Put this all together, and you can send a message from Erlang to an
// object living in the web page.
//


// EXAMPLE

// <div id="my_cell" data-behavior="cell"/>
//
// This then makes it possible to send it a message.  If the
// collection of behaviors contains .cell from widgets.js, the Erlang
// call
//
//   ws0 ! #{ type => method_call, id => my_cell, arg => <<"new content">> }
//
// can be used to set the cell's new content



function route_msg(behaviors, msg) {
    var el = document.getElementById(msg.id);
    if (!el) {
        console.log("method_call","element not found",msg.id)
        return;
    }
    route_el_msg(behaviors, el, msg);
}
function route_el_msg(behaviors, target_el, msg) {
    var el = target_el;
    var bn;
    while (!(bn = el.getAttribute('data-behavior'))) {
        // See if parent has 
        el = el.parentElement;
        if (el == document.body) {
            console.log("method_call","no data-behavior in parent chain", el);
            return;
        }
    }
    var b = behaviors[bn];
    if (!b) {
        console.log("method_call","no behavior", bn);
        return;
    }
    var m = b[msg.method];
    if (!m) {
        console.log("method_call",t,"no method",msg.method);
        return;
    }
    //console.log(m,el,target_el,msg.arg);
    // FIXME: send target_el as 3rd argument?
    // route_evt doesn't need it, as it has event.target
    m(el, msg.arg);
}


// In addition to supporting Erlang -> DOM/JavaScript messages, allow
// the same mechanism to be used to dispatch DOM events such as mouse
// clicks.
function route_evt(behaviors, event) {
    //console.log("route_evt",event);
    var msg = {
        method: event.type,
        arg: event
    };
    route_el_msg(behaviors, event.target, msg);
}




exports.msg = route_msg;
exports.evt = route_evt;


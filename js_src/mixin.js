// TL;DR.  An object is a DOM element associated to some behaviors.

// To make Erlang and DOM/JavaScript work together, DOM elements are
// thought of as objects with internal state and several "mixins" that
// define behavior.
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
//   mechanism defined in this file is used.  It associates state (a
//   DOM element) to behavior.
//
// - The message contains a second property 'id' which is used to find
//   the DOM element.
//
// - The DOM element contains a property 'data-mixin' which conains a
//   space-separate list of behavior names.  
//
// - Each name refers to a property recorded in a dictionary of
//   behaviors or mixins, which in turn contain functions (methods)
//   parameterized by the element receiving the message.
//
// - Once the DOM element and the type (class) object have been found,
//   the 'method' string in the original message is used to index the
//   class object, returning a function.
//
// - This function is executed, passing in the DOM element and the
//   contents of the 'arg' property in the original message.
//
// Put this all together, and you can send a message from Erlang to an
// object living in the web page.  Note that this is application
// behavior, not DOM behavor.  Practically, these abstract methods
// will call into DOM methods to implement their action.
//


// EXAMPLE

// <div id="my_cell" data-mixin="cell"/>
//
// This then makes it possible to send it a message.  If the
// collection of behaviors contains .cell from widgets.js, the Erlang
// call
//
//   ws0 ! #{ type => method_call, id => my_cell, arg => <<"new content">> }
//
// can be used to set the cell's new content

var tools = require("./tools");

function error(errmsg) {
    console.log("method_call", errmsg);
    throw {method_call: errmsg};
}


function route_msg(behaviors, msg) {
    if ("id" in msg) {
        var el = document.getElementById(msg.id);
        if (!el) {
            error(["element not found",msg.id]);
        }
        return route_el_msg(behaviors, el, msg);
    } else {
        var els = document.getElementsByName(msg.name);
        var rMessage = [];
        for (var i = 0; i < els.length; ++i) {
            rMessage.push(route_el_msg(behaviors, els[i], msg));
        }
        return rMessage;
    }
}
function route_el_msg(behaviors, target_el, msg) {
    var el = target_el;
    var bns;
    while (!(bns = el.getAttribute('data-mixin'))) {
        // Go up the parent chain
        el = el.parentElement;
        if (el == document.body) {
            error([target_el,
                   "no data-mixin in parent chain", el]);
        }
    }
    // FIXME: multiple classes using bn.split(' ')
    var found = false;
    var bs = bns.split(' ');
    var i, b, m, bn;
    for (var i=0; i<bs.length; i++) { // no tools.each: local exit
        bn = bs[i];
        if (!(b = behaviors[bn])) {
            error([target_el,
                   "unknown behavior", bn]);
        }
        if ((m = b[msg.method])) {
            return m(el, msg.arg);
        }
    }
    error([target_el,
           "method", msg.method, 
           "not found in behaviors", bns]);
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


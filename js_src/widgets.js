var tools = require("./tools");

function log_append(log, item, opts) {
    var first = log.childNodes[0];
    if ((opts.loc == "head") && first) {
        log.insertBefore(item, first);
    }
    else {
        log.appendChild(item);
    }
    if (opts.scroll == "no") {
    }
    else {
        // FIXME: remove window scroll: limit all log views to div
        window.scrollTo(0,document.body.scrollHeight);

        // FIXME: log's main element should reference its container div
        var div = document.getElementById("live_log_div");
        div.scrollTop = div.scrollHeight;
    }
}
function append_text(log_el, text, opts) {
    log_append(log_el, document.createTextNode(text), opts);
}
function append_html(log_el, html, opts) {
    var div = document.createElement('div');
    div.innerHTML = html;
    log_append(log_el, div.firstChild, opts);
}


// display_list
function display_list_each(container, fun) {
    tools.each(container.children, fun);
}
function display_list_one(container, name, fun) {
    tools.each(container.children, function(node) {
        if (node.getAttribute('name') == name) { fun(node); }
    });
}
function set_node_style_display(node, display) {
    //console.log(node, display);
    node.style.display = display;
}
function display_node_enable(node) {
    set_node_style_display(node, 'block');
}
function display_node_disable(node) {
    set_node_style_display(node, 'none');
}
function display_node_toggle(node) {
    set_node_style_display(
        node,
        (node.style.display == 'none') ? 'block' : 'none');
}
function display_list_select(container, name) {
    display_list_each(container, display_node_disable);
    display_list_one(container, name, display_node_enable);
}
function display_list_enable(container, name) {
    display_list_one(container, name, display_node_enable);
}
function display_list_disable(container, name) {
    display_list_one(container, name, display_node_disable);
}

function display_event(input_el, event) {
    // Implement behavior for different input types and buttons.
    var id = input_el.getAttribute('data-target');
    var container = document.getElementById(id);
    // console.log(input_el.type, id, container);
    if (input_el.type == 'select-one') {
        var opts = input_el.options;
        var name = opts[opts.selectedIndex].value;
        display_list_select(container, name);
    }
    else if (input_el.type == 'checkbox') {
        display_list_each(container, 
                          input_el.checked ?
                          display_node_enable :
                          display_node_disable);
    }
    else if (input_el.type == 'submit') {
        display_toggle(container);
    }
}


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
// Convert a form or input element to form data.
function form_data(el) {
    var form;

    // Form returns list of fields
    if ('form' == el.nodeName) {
        form = [];
        tools.each(el.elements, function(input) {
            if (input.name) {
                form.push(form_field(input));
            }
        });
    }
    // A single imput returns list with one field
    else {
        if (el.name) {
            form = [form_field(el)];
        }
    }
    return form;
}

// Constructors for specific element types
function create_element(spec) {
    ns_elements = {
        path: function() {
            // https://stackoverflow.com/questions/16488884/add-svg-element-to-existing-svg-using-dom
            // Create a path in SVG's namespace
            return document.createElementNS(
                "http://www.w3.org/2000/svg", 'path'); 
        }
    }
    var create_el = ns_elements[spec.t];
    var el = create_el ? create_el() : document.createElement(spec.t); 
    for (attr in spec.a) {
        el.setAttribute(attr, spec.a[attr]);
    }
    tools.each(spec.e, function(el_spec) {
        el.appendChild(create_element(el_spec));
    });
    // console.log(el);
    return el;
}

// arr contains waveform data
// tx contains transform: tx.scale, tx.offset, tx.inc
// FIXME: not well supported
function path_set_waveform_(path, arr, tx) {
    var d_point;
    var psl = path.pathSegList;
    psl.clear();
    tools.each(arr, function(y, x) {
        var point = ((y * tx.scale) + tx.offset);
        if (null == d_point) {
            d_point = point;
            var m = path.createSVGPathSegMovetoAbs(-tx.inc, d_point);
            psl.appendItem(m)
        }
        var l = path.createSVGPathSegLinetoRel(tx.inc, point - d_point);
        psl.appendItem(l);
        d_point = point;
    });
}

function path_set_waveform(path, arr, tx) {
    var d_point, path_d;
    tools.each(arr, function(y, x) {
            // console.log(x,y);
            var point = ((y * tx.scale) + tx.offset);
            if (null == d_point) {
                d_point = point;
                path_d = 'M-1,' + d_point;
            }
            path_d += 'l1,' + (point - d_point);
            d_point = point;
        });
    path.setAttribute('d',path_d);
}



// Behavior for standard dom objects
module.exports = {


    // el :: <input type='checkbox' />
    checkbox: { 
        set: function(el, val) { el.checked = val; }
    },
    // el :: <td /> or <div />, something to contain the value.
    cell: { 
        set: function(el, arg) {
            if (typeof(arg) == "string") {
                el.innerHTML = arg;
            }
            else {
                el.innerHTML = '';
                el.appendChild(create_element(arg));
            }
        },
        // Note: Use create_element to create SVG elements.
        append: function(el, arg) {
            // console.log(arg);
            if (typeof(arg) == "string") {
                var tmp = document.createElement('div');
                tmp.innerHTML = arg;
                tools.each(tmp.children, function(child) {
                    el.appendChild(child);
                });
            }
            else {
                el.appendChild(create_element(arg));
            }
        }
    },
    // el :: any element that contains a list of children to display/hide
    display_list: {
        select:  display_list_select,
        enable:  display_list_enable,
        disable: display_list_disable
    },
    // el :: input element with 'data-target' attribute
    display_control: {
        change: display_event,
        click:  display_event
    },
    // el :: settable input element
    input: {
        set: function(el, arg) {
            if (el.type == 'checkbox') {
                el.checked = arg;
            }
            else if (el.type == 'select-one') {
                var o = el.options;
                tools.each(o, function(option, i) {
                    if (option.value == arg) {
                        o.selectedIndex = i;
                    }
                });
            }
            else {
                el.value = arg;
            }
        },
        get: function(el) {
            if (el.type == 'checkbox') {
                return el.checked;
            }
            else if (el.type == 'select-one') {
                //FIXME
            }
            else {
                return el.value;
            }
        },
        get_tagged: function(el) {
            return [el.getAttribute('data-decoder'),  // type            
                    input_value(el)];
        }
    },
    log: {
        append_text: function(el, arg) {
            append_text(el, arg[0], arg[1]);
        },
        append_html: function(el, arg) {
            append_html(el, arg[0], arg[1]);
        }
    },

    // Not a behavior, just some associated tools exposed.
    tools: {
        form_data: form_data,
        path_set_waveform: path_set_waveform
    }

}

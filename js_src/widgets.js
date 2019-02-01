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
        if (div) div.scrollTop = div.scrollHeight;
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
    return [input_name(input),                   // key
            input.getAttribute('data-decoder'),  // type
            input_value(input)];                 // value
}

function input_name(el) {
    var dn = el.getAttribute('data-name');
    if (dn) { return dn; }
    if (el.name) { return el.name; }
    return el.getAttribute('id');
}

// Convert input's value to string based on kind of input.
function input_value(el) {
    var dv;
    if (el.type == 'checkbox') {
        return el.checked ? 'true' : 'false';
    }
    else if (el.type == 'select-one') {
        var opts = el.options;
        return opts[opts.selectedIndex].value;
    }
    else {
        var dv = el.getAttribute('data-value');
        if (dv) {
            // "clickable" are encoded differently.
            return dv;
        }
        else {
            return el.value;
        }
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
        form = [form_field(el)];
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

// Most values are strings
function decode(data_decoder, encoded) {
    if (data_decoder == 'boolean') {
        if (encoded == "true")  return true;
        if (encoded == "false") return false;
    }
    else {
        console.log("no_decoder",data_decoder);
        return null;
    }
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
             //console.log("cell_set",arg);
             //console.log("cell_set-el",el);
            if (typeof(arg) == "string") {
                el.innerHTML = arg;
            }
            else {
                el.innerHTML = '';
                el.appendChild(create_element(arg));
            }
        },
        set_attribute: function(el, args) {
            var name = args[0];
            var val  = args[1];
            el.setAttribute(name, val);
        },
        set_display: function(el, arg) {
            el.style.display = arg;
        },
        add_class: function(el, arg) {
            if (typeof(arg) == 'object') {
                for (var i = 0; i < arg.length; ++i) {      
                    if (typeof(arg[i]) == "string") {
                        el.classList.add(arg[i]);
                    }
                }   
            } else {
                el.classList.add(arg);
            }
        },

        remove_class: function(el, arg) {
            if (typeof(arg) == 'object') {
                for (var i = 0; i < arg.length; ++i) {      
                    if (typeof(arg[i]) == "string") {
                        el.classList.remove(arg[i]);
                    }
                }   
            } else {
                el.classList.remove(arg);
            }
        },
        // Note: Use create_element to create SVG elements.
        append: function(el, arg) {
            // console.log("cell_append",arg);
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
        },
        remove: function(el, arg) {
            var child = document.getElementById(arg);
            el.removeChild(child);
        }
    },


    // el :: item in select list.
    display: {
        select: function(el) {
            tools.each(el.parentNode.children, function(node) {
                node.style.display = 'none';
            });
            el.style.display = 'block';
        }
    },


    // el :: settable input element
    input: {
        set: function(el, arg) {
            if (el.type == 'checkbox') {
                el.checked = decode(el.getAttribute('data-decoder'), arg);
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
    }

}

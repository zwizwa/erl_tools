function for_children(el, fun) {
    for (var i=0; i<el.children.length; i++) {
        fun(el.children[i]);
    }
}
function select(container, select_name) {
    for_children(
        container, function(node) {
            node.style.display = 'none';
            // console.log(node);
            if (node.getAttribute('name') == select_name) {
                node.style.display = 'block';
            }
        });
}
function select_one(container, display) {
    for_children(
        container, function(node) {
            node.style.display = display;
        });
}
function select_toggle(container, display) {
    for_children(
        container, function(node) {
            node.style.display =
                (node.style.display == 'none') ?
                'block' : 'none';
        });
}
function showhide_event(input_el, event) {
    // Implement behavior for different input types and buttons.
    var id = input_el.getAttribute('data-target');
    var container = document.getElementById(id);
    console.log(input_el.type, id, container);
    if (input_el.type == 'select-one') {
        var opts = input_el.options;
        var name = opts[opts.selectedIndex].value;
        select(container, name);
    }
    else if (input_el.type == 'checkbox') {
        var display = input_el.checked ? 'block' : 'none';
        select_one(container, display);
    }
    else if (input_el.type == 'submit') {
        select_toggle(container);
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
        set: function(el, val) { el.innerHTML = val; }
    },
    // el :: <td /> or <div />, something to contain the values.
    list: { 
        // NOT TESTED
        clear: function(el, html) { 
            el.innerHTML = '';
        },
        append: function(el, html) { 
            var tmp = document.createElement('div');
            tmp.innerHTML = html;
            for (var i=0; i<tmp.children.length; i++) {
                el.appendChild(tmp.children[i]);
            }
        }
    },
    // el :: any element that contains a list of children to display/hide
    showhide_list: {
        select: select 
    },
    // el :: input element with 'data-target' attribute
    showhide_control: {
        change: showhide_event,
        click:  showhide_event
    }
}

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

// Behavior for standard dom objects
module.exports = {
    checkbox: { // el :: <input type='checkbox' />
        set: function(el, val) { el.checked = val; }
    },
    cell: { // el :: <td /> or <div />, something to contain the value.
        set: function(el, val) { el.innerHTML = val; }
    },
    showhide_list: { // el :: any element that contains a list of children to display/hide
        select: select 
    },
    showhide_control: {
        change: function(input_el, event) {
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
        }
    }
}

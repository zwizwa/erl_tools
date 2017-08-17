function select(el, select) {
    for (var i=0; i<el.children.length; i++) {
        var child = el.children[i];
        child.style.display = 'none';
        console.log(child);
        if (child.getAttribute('name') == select) {
            child.style.display = 'block';
                }
    }
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
        change: function(el, event) {
            var id = el.getAttribute('data-target');
            var val = el.options[el.options.selectedIndex].value;
            // console.log(id, val, el, event);
            select(document.getElementById(id), val);
        }
    }
}

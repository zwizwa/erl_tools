// Behavior for standard dom objects
module.exports = {
    checkbox: { // el :: <input type='checkbox' />
        set: function(el, val) { el.checked = val; }
    },
    cell: { // el :: <td /> or <div />, something to contain the value.
        set: function(el, val) { el.innerHTML = val; }
    },
    showhide: { // el :: any element that contains a list of children to display/hide
        select: function(el, select) {
            for (var i=0; i<el.children.length; i++) {
                var child = el.children[i];
                child.style.display = 'none';
                console.log(child);
                if (child.getAttribute('name') == select) {
                    child.style.display = 'block';
                }
            }
        }
    }
}

// Behavior for standard dom objects
module.exports = {
    checkbox: { // el :: <input type='checkbox' />
        set: function(el, val) { el.checked = val; }
    },
    cell: { // el :: <td /> or <div />, something to contain the value.
        set: function(el, val) { el.innerHTML = val; }
    }
}

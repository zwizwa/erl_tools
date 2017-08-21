// Misc "ground level" library.

function each(o, f) {
    var i;
    if (o.length !== undefined) {
        // Array
        for (i=0; i<o.length; i++) { f(o[i],i,o); }
        return o;
    }
    if (o.forEach !== undefined) {
        o.forEach(f);
        return o;
    }x
    console.log("error: each", o, f);
}

module.exports = {
    each: each
}

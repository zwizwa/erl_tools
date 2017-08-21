// Misc "ground level" library.

function each(o, f) {
    var i;
    if (o.length !== undefined) {
        // Array
        for (i=0; i<o.length; i++) { 
            console.log(i,o[i]);
            f(o[i],i,o); 
        }
        return o;
    }
    console.log("error: each", o, f);
    throw(each,o,fun);
}

module.exports = {
    each: each
}

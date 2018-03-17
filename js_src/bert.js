//       Copyright (C) 2009 Ryan Tomayko <tomayko.com/about>
//       Copyright (C) 2013 Jason Lunz
//       Copyright (c) 2017 Tom Schouten
//
// Permission  is  hereby granted, free of charge, to any person ob-
// taining a copy of  this  software  and  associated  documentation
// files  (the "Software"), to deal in the Software without restric-
// tion, including without limitation the rights to use, copy, modi-
// fy, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is  fur-
// nished to do so, subject to the following conditions:
//
// The  above  copyright  notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF  ANY  KIND,
// EXPRESS  OR  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE  AND  NONIN-
// FRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER  IN  AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN  THE
// SOFTWARE.


//
// This code is templated on bert.js from
// https://github.com/znull/node-bertrpc
// 08c446eeec9de2648e77fa248f1a0320aada52e0
//
// - factored into stream object and expression parser
// - only support what is needed for gateway interaction
// - return types for BERT -> JS
//   - atoms -> strings
//   - lists -> arrays
//   - bytes -> strings (same as EJSON)
//   - integer -> number
//   - maps -> object (keys are converted to string)
//   - {atom, binary} -> special type or [string,arraybuf]



function Stream(arraybuf) {
    this.buffer = arraybuf;
    this.offset = 0;
    this.u8 = new Uint8Array(arraybuf);
}
Stream.prototype = {
    left: function() {
        return this.u8.length - this.offset;
    },
    wind: function(nb_bytes) {
        this.offset += nb_bytes;
    },
    peek: function() {
        return this.u8[this.offset];
    },
    pop: function() {
        var b = this.peek();
        this.wind(1);
        return b;
    },
    pop_slice: function(size_field_size) {
        var size = this.int_be(size_field_size);
        var slice = this.buffer.slice(this.offset, this.offset + size);
        this.wind(size);
        return slice;
    },
    int_be: function(length) {
       var acc = 0;
       while(length--) {
           acc = ((acc * 256) + this.pop()) | 0;
       }
       return acc;
   }

}

// If no UTF8 decoder is available, use this fallback.
// GateWay code should not use anything but ASCII.
var ascii_decoder = {
    decode: function(arr) {
        var str = '';
        var i;
        for (i=0; i=arr.length; i++) {
            str += String.fromCharCode(arr[i])
        }
        return str;
    }
}
function Bert(deps) {
    if (deps.UTF8Decoder) {
        this.utf8_decoder = new deps.UTF8Decoder();
    }
    else {
        console.log("WARNING: no UTF8Decoder");
        this.utf8_decoder = ascii_decoder;
    }
}
Bert.prototype = {

    /* WIRE PROTOCOL CODES */

    BERT_START:    131,
    SMALL_ATOM:    115,
    ATOM:          100,
    BINARY:        109,
    SMALL_INTEGER: 97,
    INTEGER:       98,
    SMALL_BIG:     110,
    LARGE_BIG:     111,
    FLOAT:         99,
    NEW_FLOAT:     70,
    STRING:        107,
    LIST:          108,
    SMALL_TUPLE:   104,
    LARGE_TUPLE:   105,
    NIL:           106,
    MAP:           116,
    ZERO:          0,

    types: {
        s16_le: function(buffer) { return new Int16Array(buffer); }
    },

    decode: function (arraybuf) {
        var s = new Stream(arraybuf);
        if (s.pop() != this.BERT_START) throw new Error("Not a valid BERT.");
        var obj = this.decode_inner(s);
        if (s.left() != 0) throw new Error(["Invalid BERT:",s.left()]);
        return obj;
    },
    decode_inner: function (s) {
        var type = s.pop();
        //console.log(type);
        if (type == this.SMALL_ATOM)    return this.decode_atom(s, 1);
        if (type == this.ATOM)          return this.decode_atom(s, 2);
        if (type == this.BINARY)        return this.decode_binary(s);
        if (type == this.SMALL_INTEGER) return this.decode_integer(s, 1);
        if (type == this.INTEGER)       return this.decode_integer(s, 4);
        if (type == this.STRING)        return this.decode_bytelist(s);
        if (type == this.LIST)          return this.decode_list(s);
        if (type == this.SMALL_TUPLE)   return this.decode_tuple(s, 1);
        if (type == this.LARGE_TUPLE)   return this.decode_tuple(s, 4);
        if (type == this.MAP)           return this.decode_map(s);
        if (type == this.NIL)           return this.decode_nil(s);
        // not supported: floats and bignums -- see original bert.js
        if (type == this.SMALL_BIG)     return this.decode_big(s, 1);
        if (type == this.LARGE_BIG)     return this.decode_big(s, 4);
        if (type == this.FLOAT)         return this.decode_float(s);
        if (type == this.NEW_FLOAT)     return this.decode_new_float(s);
        throw new Error("Unexpected BERT type: " + String.charCodeAt(type));
    },
    decode_array: function (s, count) {
        var size  = s.int_be(count);
        var arr = new Array();
        while(size--) {
            arr.push(this.decode_inner(s));
        }
        return arr;
    },
    decode_list: function (s) {
        var arr  = this.decode_array(s, 4);
        var tail = this.decode_inner(s);
        // See decode_nil
        if (Array.isArray(tail) && (0 == tail.length)) { return arr; }
        else { return { type: 'improper-list', arr: arr, tail: tail } }
    },
    decode_bytelist: function (s) {
        return s.pop_slice(2);
    },
    decode_integer: function (s, count) {
        return s.int_be(count);
    },
    decode_binary: function (s) {
        return this.utf8_decoder.decode(s.pop_slice(4));
    },
    decode_atom: function (s, count) {
        return this.utf8_decoder.decode(s.pop_slice(count));
    },
    decode_nil: function (s) {
        return new Array();
    },
    decode_map: function(s) {
        var size = s.int_be(4);
        var obj  = new Object();
        while(size--) {
            var key = this.decode_inner(s);
            var val = this.decode_inner(s);
            obj[key] = val;
        }
        return obj;
    },
    decode_tuple: function (s, count) {
        var size = s.int_be(count);
        var arr = new Array();
        while(size--) {
            arr.push(
                // Inside a tuple, binaries have a special meaning and
                // are not converted to string by default.
                (s.peek() == this.BINARY) ?
                    (s.pop(), s.pop_slice(4)) :
                    this.decode_inner(s));
        }
        var cons = this.types[arr[0]];
        if (!cons) throw new Error("No such type: " + arr[0]);
        return cons.apply(null, arr.slice(1));
    },

    // All push_ functions only use .push() on the object.  So rep
    // could be something else.  This doesn't try to be smart: just
    // encode JavaScript objects.  Leave smarts to the other side.
    encode: function (obj) {
        a = new Array();
        a.push(this.BERT_START);
        this.push_inner(a, obj);
        return a;
    },
    push_inner: function (a, obj) {
        var type = typeof(obj);
        this["push_" + type].call(this, a, obj);
    },
    push_string: function (a, str) {
        a.push(this.BINARY);
        this.push_int(a, str.length, 4);
        this.push_chars(a, str);
    },
    push_atom: function (a, str) {
        a.push(this.ATOM);
        this.push_int(a, str.length, 2);
        this.push_chars(a, str);
    },
    push_chars: function (a, str) {
        for (var i=0; i<str.length; i++) {
            a.push(str.charCodeAt(i));
        }
    },
    push_int: function (a, val, length) {
        while(length > 0) {
            length--;
            var shifted = val >> (8 * length);
            a.push(shifted & 255);
        }
    },
    push_boolean: function(a, val) {
        if (val) this.push_atom(a, "true");
        else     this.push_atom(a, "false");
    },
    push_number: function (a, obj) {
        var remainder = (obj % 1 != 0);
        if (remainder) {
            this.push_float(a, obj);
        }
        else if (obj >= 0 && obj < 256) {
            a.push(this.SMALL_INTEGER);
            this.push_int(a, obj, 1);
        }
        else if (obj >= -134217728 && obj <= 134217727) {
            a.push(this.INTEGER);
            this.push_int(a, obj, 4);
        }
        else {
            // bignums not supported
            this.push_float(a, obj);
        }
    },
    push_float: function (a, obj) {
        // floats not supported
        a.push(this.INTEGER);
        this.push_int(a, obj|0, 4);
    },
    push_object: function (a, obj) {
        // special objects
        if (obj == null) {
            this.push_atom(a, "null");
        } 
        // array as list
        else if (Array.isArray(obj)) {
            a.push(this.LIST);
            this.push_int(a, obj.length, 4);
            for (var i=0; i<obj.length; i++) {
                this.push_inner(a, obj[i]);
            }
            a.push(this.NIL);
        }
        // object as map with atom keys
        else {
            var l = 0;
            for (var key in obj) { l++; }
            a.push(this.MAP);
            this.push_int(a, l, 4);
            for (var key in obj) {
                this.push_atom(a, key);
                this.push_inner(a, obj[key]);
            }
        }
    }
}


module.exports.Bert = Bert;




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

// Behavior associated to data-type=log
module.exports.log = {
    append_text: function(el, arg) {
        append_text(el, arg[0], arg[1]);
    },
    append_html: function(el, arg) {
        append_html(el, arg[0], arg[1]);
    }
}

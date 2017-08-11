// Translated from js_src/bert.js
//
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

#include "bert.h"

static struct bert_object *r_object(struct bert_reader *r);

static uint32_t r_uint(struct bert_reader *r, uint32_t size) {
    uint32_t acc = 0;
    while(size--) { acc = (acc << 8) + r->pop(r); }
    return acc;
}
static int32_t r_sint(struct bert_reader *r, uint32_t size) {
    if (size == 4) return r_uint(r, size);
    r->error(r, "unsupported signed int size");
}
static struct bert_object *r_tuple(struct bert_reader *r, uint32_t size_size) {
    uint32_t size = r_uint(r, size_size);
    struct bert_object *el[size];
    for(uint32_t i=0; i<size; i++) { el[i] = r_object(r); }
    return r->tuple(r, size, el);
}
static struct bert_object *r_list(struct bert_reader *r) {
    struct bert_object *val_tuple = r_tuple(r, 4);
    struct bert_object *val_tail = r_object(r);
    return r->list(r, val_tuple, val_tail);
}
static struct bert_object *r_slice(struct bert_reader *r,
                                   struct bert_object* (*obj)(
                                       struct bert_reader *r, uint32_t size),
                                   uint32_t size_size) {
    uint32_t size = r_uint(r, size_size);
    struct bert_object *val = obj(r, size);
    r->wind(r, size);
    return val;
}
static struct bert_object *r_object(struct bert_reader *r) {
    uint8_t type = r->pop(r);
    if (type == BERT_LIST)          return r_list(r);
    if (type == BERT_BINARY)        return r_slice(r, r->binary, 4);
    if (type == BERT_NIL)           return r->nil;
    if (type == BERT_SMALL_ATOM)    return r_slice(r, r->atom, 1);
    if (type == BERT_ATOM)          return r_slice(r, r->atom, 2);
    if (type == BERT_SMALL_INTEGER) return r->integer(r, r_uint(r, 1));
    if (type == BERT_INTEGER)       return r->integer(r, r_sint(r, 4));
    if (type == BERT_SMALL_TUPLE)   return r_tuple(r, 1);
    if (type == BERT_LARGE_TUPLE)   return r_tuple(r, 4);
    if (type == BERT_STRING)        return r_slice(r, r->string, 2);
    r->error(r, "Unexpected BERT type");
    return NULL; // not reached
}

static struct bert_object* c_null(void) {
    return NULL;
}
struct bert_object *bert_decode(struct bert_reader *r) {
    /* Undefined constructors will return NULL values. */
    if (!r->tuple)   r->tuple   = (void*)&c_null;
    if (!r->list)    r->list    = (void*)&c_null;
    if (!r->binary)  r->binary  = (void*)&c_null;
    if (!r->atom)    r->atom    = (void*)&c_null;
    if (!r->string)  r->string  = (void*)&c_null;
    if (!r->integer) r->integer = (void*)&c_null;
    uint8_t start = r->pop(r);
    if (start != BERT_START) { r->error(r, "Not a valid BERT"); }
    return r_object(r);
}


static void w_uint(struct bert_writer *w, uint32_t val_size, uint32_t val) {
    while(val_size--) { w->push(w, val >> (8 * val_size)); }
}
static void w_tagged_size(struct bert_writer *w,
                          uint8_t tag, uint32_t size_size, uint32_t size) {
    w->push(w, tag);
    w_uint(w, size_size, size);
}
static void w_tagged_bytes(struct bert_writer *w,
                           uint8_t tag, uint32_t size_size,
                           const uint8_t *buf, int32_t nb_bytes) {
    if (nb_bytes < 0) nb_bytes = strlen((const char*)buf);
    w_tagged_size(w, tag, size_size, nb_bytes);
    for (uint32_t i=0; i<nb_bytes; i++) { w->push(w, buf[i]); }
}
static void w_small_tuple(struct bert_writer *w, uint32_t nb_el) {
    w_tagged_size(w, BERT_SMALL_TUPLE, 1, nb_el);
}
static void w_large_tuple(struct bert_writer *w, uint32_t nb_el) {
    w_tagged_size(w, BERT_LARGE_TUPLE, 4, nb_el);
}
static void w_small_atom(struct bert_writer *w, const uint8_t *buf, int32_t nb_bytes) {
    w_tagged_bytes(w, BERT_SMALL_ATOM, 1, buf, nb_bytes);
}
static void w_atom(struct bert_writer *w, const uint8_t *buf, int32_t nb_bytes) {
    w_tagged_bytes(w, BERT_ATOM, 2, buf, nb_bytes);
}
static void w_binary(struct bert_writer *w, const uint8_t *buf, int32_t nb_bytes) {
    w_tagged_bytes(w, BERT_BINARY, 4, buf, nb_bytes);
}
static void w_push(struct bert_writer *w, uint8_t byte) {
    if (w->buf) { w->buf[w->size] = byte; }
    w->size++;
}
static void w_list(struct bert_writer *w, uint32_t nb_el) {
    w_tagged_size(w, BERT_LIST, 4, nb_el);
}
static void w_nil(struct bert_writer *w) {
    w->push(w, BERT_NIL);
}
static void w_set_api(struct bert_writer *w) {
    if (!w->push) w->push = w_push;
    w->uint        = w_uint;
    w->small_tuple = w_small_tuple;
    w->large_tuple = w_large_tuple;
    w->small_atom  = w_small_atom;
    w->atom        = w_atom;
    w->binary      = w_binary;
    w->list        = w_list;
    w->nil         = w_nil;
}
// Write subterms incrementally
uint32_t bert_write_sub(struct bert_writer *w, bert_write_seq_t seq){
    w_set_api(w);
    seq(w);
    return w->size;
}
// Two pass with whole message callback.
void bert_write_packet(struct bert_writer *w,
                       uint32_t packet_size_size,  // e.g. 4 for {packet,4}
                       bert_write_seq_t seq,
                       bert_write_done_t done) {

    /* 1. dry run to determine size */
    w->size = 0;
    w->buf = NULL;
    uint32_t term_size = 1 + /* BERT_START */ + bert_write_sub(w, seq);

    /* 2. second run attached to local buffer.  write {packet,N}
       header, BERT header, and subterm, and send it out */
    w->size = 0;
    uint8_t buf[packet_size_size + term_size];
    w->buf = buf;

    w_uint(w, packet_size_size, term_size);
    w->push(w, BERT_START);
    bert_write_sub(w, seq);

    done(w, buf, w->size);
}

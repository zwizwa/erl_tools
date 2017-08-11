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

#ifndef BERT_H
#define BERT_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define BERT_START         131
#define BERT_SMALL_ATOM    115
#define BERT_ATOM          100
#define BERT_BINARY        109
#define BERT_SMALL_INTEGER  97
#define BERT_INTEGER        98
#define BERT_SMALL_BIG     110
#define BERT_LARGE_BIG     111
#define BERT_FLOAT          99
#define BERT_NEW_FLOAT      70
#define BERT_STRING        107
#define BERT_LIST          108
#define BERT_SMALL_TUPLE   104
#define BERT_LARGE_TUPLE   105
#define BERT_NIL           106
#define BERT_MAP           116
#define BERT_ZERO            0

/* BERT parser with parameterized constructors (i.e a generalized
   right fold). */

// Representation is defined elsewhere
struct bert_object;

/* Interface */
struct bert_reader;
struct bert_reader {
    /* Stream access */
    uint8_t (*pop)(struct bert_reader *);
    void (*wind)(struct bert_reader *, uint32_t bytes);

    /* Recursive Constructors */
    struct bert_object* (*tuple)(struct bert_reader *, uint32_t size, struct bert_object **el);
    struct bert_object* (*list)(struct bert_reader *, struct bert_object *tuple, struct bert_object *tail);

    /* Primitive constructors: reader points at object's binary data. */
    struct bert_object* (*binary)(struct bert_reader *, uint32_t size);
    struct bert_object* (*atom)(struct bert_reader *, uint32_t size);
    struct bert_object* (*string)(struct bert_reader *, uint32_t size);
    struct bert_object* (*integer)(struct bert_reader *, int32_t val);
    struct bert_object* nil;

    /* Error handler: this should abort control flow. */
    void (*error)(struct bert_reader *, const char *msg);
};

struct bert_object *bert_decode(struct bert_reader *s);



/* BERT writer.

   A term is abstracted as a function that generates the sequence of
   tags.  This function is called twice, so beware of side effects.
*/
struct bert_writer;
struct bert_writer {
    /* Provided by caller */
    void (*push)(struct bert_writer *, uint8_t byte);
    uint8_t *buf;

    /* API used by caller */
    void (*small_tuple)(struct bert_writer *, uint32_t nb_el);
    void (*large_tuple)(struct bert_writer *, uint32_t nb_el);
    void (*small_atom) (struct bert_writer *, const uint8_t *buf, int32_t nb_bytes);
    void (*atom)       (struct bert_writer *, const uint8_t *buf, int32_t nb_bytes);
    void (*binary)     (struct bert_writer *, const uint8_t *buf, int32_t nb_bytes);
    void (*uint)       (struct bert_writer *, uint32_t val, uint32_t size);
    void (*list)       (struct bert_writer *w, uint32_t nb_el);
    void (*nil)        (struct bert_writer *w);

    /* Private */
    uint32_t size;
};
typedef void (*bert_write_seq_t)(struct bert_writer *);
typedef void (*bert_write_done_t)(struct bert_writer *, uint8_t *, uint32_t);

uint32_t bert_write_sub(struct bert_writer *w, bert_write_seq_t);
void bert_write_packet(struct bert_writer *w, uint32_t packet_size_size,
                       bert_write_seq_t, bert_write_done_t);


#endif //BERT_H

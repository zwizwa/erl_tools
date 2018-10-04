// (c) 2018 Tom Schouten -- see LICENSE file

#include "system.h"
#include <stdio.h>
#include <sqlite3.h>
#include <setjmp.h>

#include "bert.h"
#include "port.h"

// https://www.sqlite.org/c_interface.html
// https://www.sqlite.org/quickstart.html

sqlite3 *db;
sqlite3_stmt *stmt;
jmp_buf error_jmp_buf;

/* Use the generalized right fold over the erlang term format provided
   in bert.[ch] to iterate over the data primitives in the order they
   appear in the stream.

   Ignore tuple and list structure, and primitive atoms except for
   atom and binary, effectively flattening and filtering the input
   into a sequence of binaries and atoms:

   E.g. {<<"insert into config (var,val) values (?,?)">>,
         [{text,<<"myvar">>},
          {blob,<<"myval">>}]}

   Flattens to these constructor calls in sequence:

     binary(<query string>)
     atom('text'),
     binary(<<"myvar">>)
     atom('blob')
     binary(<<"myval">>)

   The meaning of an atom is then to set the sqlite3 binding type of
   the subsequent binary.  After that, the binding type is restored to
   text.  Binaries are collected in a arg list.

   The first binary is the query string.  The remaining binaries are
   arg bindings. */

typedef int (*bind_t)(sqlite3_stmt*, int, const void*, int n, void(*)(void*));
struct slice {
    bind_t bind;
    const uint8_t *buf;
    uint32_t len;
};
#define MAX_NB_SLICES 10
struct reader {
    struct bert_reader r;
    const uint8_t *buf;
    uint32_t offset, length, nb_slices;
    bind_t bind;
    struct slice arg[MAX_NB_SLICES];
};
// dummy type - values are not used, only traversal side effect
struct bert_object {};
static void wind(struct reader *r, uint32_t nb_bytes) {
    uint32_t new_offset = r->offset + nb_bytes;
    if (new_offset > r->length) { ERROR("message truncated"); }
    r->offset = new_offset;
}
static uint8_t pop(struct reader *r) {
    uint8_t byte = r->buf[r->offset];
    wind(r, 1);
    return byte;
}
// An atom in the stream sets the current binder, which is picked up by binary().
static int blobtype(struct reader *r, uint32_t nb_bytes, const char *cstring) {
    return (strlen(cstring) == nb_bytes)
        && (!strncmp(cstring, (const char*)(r->buf+r->offset), nb_bytes));
}
static struct bert_object *atom(struct reader *r, uint32_t nb_bytes) {
    if (blobtype(r,nb_bytes,"blob")) {r->bind = (bind_t)sqlite3_bind_blob; return NULL;}
    if (blobtype(r,nb_bytes,"text")) {r->bind = (bind_t)sqlite3_bind_text; return NULL;}
    ERROR("unknown bind type");
}
// Side effect stores slice in current slot.
static struct bert_object* binary(struct reader *r, uint32_t nb_bytes) {
    if (r->nb_slices >= MAX_NB_SLICES) { ERROR("MAX_NB_SLICES"); }
    struct slice *s = &r->arg[r->nb_slices++];
    s->bind = r->bind;
    s->buf = r->buf + r->offset;
    s->len = nb_bytes;
    r->bind = (bind_t)sqlite3_bind_text; // restore default
    return NULL;
}


static void parse_error(struct reader *r, const char *msg) {
    ERROR(msg);
}

/* Generic send */
void msg_send(struct bert_writer *w,
              uint8_t *buf, uint32_t len) {
    //LOG("msg_send(%p, %d)\n", buf, len);
    assert_write(1, buf, len);
}

void sentinel() {
    /* Terminate with an empty message */
    uint8_t sentinel[4] = {};
    assert_write(1, sentinel, sizeof(sentinel));
    fflush(stdout); // ?
}

/* Package error message and send it to Erlang side. */
struct sql_error {
    struct bert_writer w;
    const char *msg;
};
void sql_error_seq(struct bert_writer *w) {
    struct sql_error *err = (void*)w;
    w->small_tuple(w, 2);
    w->atom  (w, (const uint8_t*)"sqlite3_errmsg", -1);
    w->binary(w, (const uint8_t*)err->msg, -1);
}
void sql_error(int rv) {
    struct sql_error err = {
        .msg = sqlite3_errmsg(db),
    };
    bert_write_packet(&err.w, 4, &sql_error_seq, &msg_send);
    sentinel();

    // Abort iteration
    // LOG("sqlite3_errmsg -> %s\n", err.msg);
    longjmp(error_jmp_buf, 1);
}



struct sql_row {
    struct bert_writer w;
    struct slice *col;
    uint32_t nb_cols;
};
void sql_row_seq(struct bert_writer *w) {
    struct sql_row *row = (void*)w;
    w->list(w, row->nb_cols);
    for(uint32_t c=0; c<row->nb_cols; c++){
        struct slice *s = &row->col[c];
        w->binary(w, s->buf, s->len);
    }
    w->nil(w);
}

void query(const uint8_t *buf, uint32_t length) {

    /* Convert BERT to list of binary terms. */
    struct reader r = {
        .buf = buf,
        .length = length,
        .bind = (bind_t)sqlite3_bind_text,
        .r = {
            .pop    = (void*)&pop,
            .wind   = (void*)&wind,
            .error  = (void*)&parse_error,
            /* We expect [binary() | [{atom(), binary()}]] */
            .binary = (void*)&binary,
            .atom   = (void*)&atom,
        }
    };
    bert_decode(&r.r); // run for side-effect
    if (r.nb_slices == 0) { ERROR("empty statement"); }


    /* First term is the query string. */
    int rv = sqlite3_prepare_v2(
        db, (const char*)r.arg[0].buf, r.arg[0].len, &stmt, NULL);
    if (rv != SQLITE_OK) { sql_error(rv); }

    /* Subsequent terms are optional blob values to bind to the
       statement. */
    for(uint32_t i=1; i<r.nb_slices; i++) {
        struct slice *a = &r.arg[i];
        //LOG("bind %d %d\n", i, s->len);
        a->bind(stmt, i, a->buf, a->len, SQLITE_STATIC /*ok?*/);
    }

    /* Step through the query results, sending out rows as list of
       binaries. */
    while(1) {
        int rv = sqlite3_step(stmt);
        if (rv == SQLITE_ROW) {

            /* Get rows converted to binary */
            int nb_cols = sqlite3_column_count(stmt);
            if (!nb_cols) break;
            struct slice col[nb_cols];
            for (int c=0; c<nb_cols; c++) {
                col[c].buf = sqlite3_column_blob(stmt, c);
                col[c].len = sqlite3_column_bytes(stmt, c);
            }

            /* Send it out as a list of binaries. */
            struct sql_row row = {
                .nb_cols = nb_cols,
                .col = col
            };
            bert_write_packet(&row.w, 4, &sql_row_seq, &msg_send);
        }
        else if (rv == SQLITE_DONE) {
            break;
        }
        else {
            sql_error(rv);
        }
    }
    sqlite3_finalize(stmt); stmt=NULL;
    sentinel();
}
#ifdef BUILD
#define BUILDINFO " (build " BUILD ")"
#else
#define BUILDINFO ""
#endif

// FIXME
#ifndef MAIN
#define MAIN sqlite3_main
#endif

//ssize_t raw_read(int fd, void *buf, size_t count);
//ssize_t raw_write(int fd, const void *buf, size_t count);

int MAIN(int argc, char **argv) {

    if (argc < 2) {
        LOG("%s" BUILDINFO "\n", argv[0]);
        LOG("usage: %s <dbfile>\n", argv[0]);
        exit(1);
    }
    int rc;
    //erl_init(NULL,0);
    if ((rc = sqlite3_open(argv[1], &db))) {
        LOG("%s: %s\n", argv[1], sqlite3_errmsg(db));
        sqlite3_close(db);
        exit(1);
    }
    uint8_t *msg;
    uint32_t msg_len;
    // LOG("entering message loop\n");
    while((msg_len = assert_read_u32(0))) {
        if (!(msg = malloc(msg_len))) {
            ERROR("malloc(0x%08x) failed\n", msg_len);
        }
        assert_read_fixed(0, msg, msg_len);

        if(!setjmp(error_jmp_buf)) {
            // TRY
            query(msg,msg_len);
        }
        else {
            // CATCH
            sqlite3_finalize(stmt); stmt=NULL;
        }
        free(msg);
    }
    sqlite3_close(db);
    return 0;
}


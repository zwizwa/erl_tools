#ifndef PORT_H
#define PORT_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "system.h"

/* Erlang port I/O, suitable for malloc-less operation to run on
   bare-bones microcontroller.  Errors are handled through the ASSERT
   macro.  See system.h */

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif

/* RAW READ / ASSERT */

static inline int assert_read(int fd, uint8_t *buf, uint32_t nb) {
    if (nb == 0) return 0;
    int rv;
    do {
        rv = READ(fd, buf, nb);
    } while(rv == -1 && errno == EINTR); // Haskell uses signals

    if (rv > 0) {
        //LOG("%2d: rv=%d\n", fd, rv);
    }
    else if (rv == 0) {
        LOG("fd %2d: EOF\n", fd);
    }
    else if (rv < 0) {
        int e = errno;
        LOG("fd %2d: errno=%d\n", fd, e);
        //char *msg = strerror(e);
        //LOG("%d: errno=%d: %s\n", fd, e, msg); // SIGSEGV?
    }
    ASSERT(rv > 0);
    return rv;
}
static inline int assert_read_fixed(int fd, uint8_t *buf, uint32_t nb) {
    int got = 0;
    while (got < nb) {
        int rv = assert_read(fd, buf+got, nb-got);
        ASSERT(rv > 0);
        got += rv;
    }
    ASSERT_EQ(got, nb);
    return got;
}
static inline int assert_read_port8(int fd, void *buf) {
    uint8_t size;
    assert_read(fd, &size, 1);
    assert_read(fd, buf, size);
    return size;
}
static inline int assert_read_port8_cstring(int fd, char *buf) {
    uint32_t len = assert_read_port8(fd, buf);
    buf[len] = 0;
    return len;
}
static inline uint32_t assert_read_u32(int fd) {
    uint8_t be[4];
    assert_read(fd, &be[0], 4);
    return be[0] << 24 | be[1] << 16 | be[2] << 8 | be[3];
}
static inline void *assert_read_packet4(int fd) {
    uint32_t buf_len = assert_read_u32(fd);
    if (!buf_len) return NULL;
    uint8_t *buf;
    ASSERT(buf = malloc(buf_len+1));
    assert_read(fd, buf, buf_len);
    buf[buf_len] = 0; // hack for LOG("%s").
    return buf;
}



/* RAW WRITE / ASSERT */

/* Non-buffered */
static inline int assert_write(int fd, const uint8_t *buf, uint32_t nb) {
    int left = nb;
    while(left > 0) {
        int rv;
        ASSERT((rv = WRITE(fd, buf, left)) > 0);
        buf  += rv;
        left -= rv;
    }
    return nb;
}
static inline void assert_write_port8(int fd, void *buf, uint8_t nb_bytes) {
    assert_write(fd, &nb_bytes, 1);
    assert_write(fd, buf, nb_bytes);
}

/* SEND */

/* Abstract buffered send.  E.g use putchar and fflush(stdout) */
void s_u8(uint8_t b);
void s_flush(void);


// send things
static inline void s_u32(uint32_t w) { s_u8(w>>24); s_u8(w>>16); s_u8(w>>8); s_u8(w); }
static inline void s_bytes(uint32_t len, uint8_t *buf) { for(uint32_t i = 0; i<len; i++) s_u8(buf[i]); }
static inline void s_tag_term() { s_u8(131); }
static inline void s_tag_u32()  { s_u8(98); }
static inline void s_tag_nil()  { s_u8(106); }
static inline void s_tag_binary(uint32_t len) { s_u8(109); s_u32(len); } // 5
static inline void s_tag_tuple(uint32_t size) { s_u8(104); s_u8(size); } // 2
static inline void s_tag_list(uint32_t len) { s_u8(108); s_u32(len); } // 5


// http://erlang.org/doc/apps/erts/erl_ext_dist.html
// Produce a 4-byte size prefixed encoded term, encoding a small tuple of binaries.
static inline void s_term_tuple_of_binaries(int argc, char **argv) {
    uint32_t len[argc];
    uint32_t len_sum = 0;
    for(int i=0; i<argc; i++) {
        len_sum += (len[i] = argv[i] ? strlen(argv[i]) : 0);
    }
    uint32_t total =
        1 +          /* s_tag_term */
        2 +          /* s_tag_tuple*/
        (5 * argc) + /* s_tag_binary */
        len_sum;     /* binary data */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_tuple(argc);
    for(int i=0; i<argc; i++){
        s_tag_binary(len[i]);
        s_bytes(len[i],(uint8_t*)argv[i]);
    }
}
static inline void s_term_list_of_binaries(int argc, char **argv) {
    uint32_t len[argc];
    uint32_t len_sum = 0;
    for(int i=0; i<argc; i++) {
        len_sum += (len[i] = argv[i] ? strlen(argv[i]) : 0);
    }
    uint32_t total =
        1 +          /* s_tag_term */
        5 +          /* s_tag_list*/
        (5 * argc) + /* s_tag_binary */
        len_sum +    /* binary data */
        1;           /* s_nil */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_list(argc);
    for(int i=0; i<argc; i++){
        s_tag_binary(len[i]);
        s_bytes(len[i],(uint8_t*)argv[i]);
    }
    s_tag_nil();
}
static inline void s_term_list_of_u32(int n, uint32_t *a){
    uint32_t total =
        1 +          /* s_tag_term */
        5 +          /* s_tag_list*/
        5 * n +      /* integer tag + data */
        1;           /* s_nil */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_list(n);
    for(int i=0; i<n; i++){
        s_tag_u32();
        s_u32(a[i]);
    }
    s_tag_nil();
}




#endif

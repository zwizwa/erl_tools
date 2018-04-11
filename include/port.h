
// To the extent possible under law, Tom Schouten has waived all
// copyright and related or neighboring rights to the erl_tools
// library.
//
// Code:    http://zwizwa.be/git/erl_tools
// License: http://creativecommons.org/publicdomain/zero/1.0

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
    //LOG("assert_read(%d,%p,%d)\n", fd, buf, nb);
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
        exit(0);
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
    uint8_t be[4] = {};
    assert_read_fixed(fd, &be[0], 4);
    return be[0] << 24 | be[1] << 16 | be[2] << 8 | be[3];
}
static inline void *assert_read_packet4_len(int fd, uint32_t *save_len) {
    uint32_t buf_len = assert_read_u32(fd);
    if (save_len) { *save_len = buf_len; }
    if (!buf_len) return NULL;
    uint8_t *buf = malloc(buf_len+1);
    if (!buf) { LOG("malloc(0x%08x) failed\n", buf_len+1); }
    ASSERT(buf);
    assert_read_fixed(fd, buf, buf_len);
    buf[buf_len] = 0; // hack for LOG("%s").
    return buf;
}
static inline void *assert_read_packet4(int fd) {
    return assert_read_packet4_len(fd, NULL);
}




/* RAW WRITE / ASSERT */

/* Non-buffered */
static inline int assert_write(int fd, const uint8_t *buf, uint32_t nb) {
    int left = nb;
    while(left > 0) {
        int rv;
        ASSERT_ERRNO(rv = WRITE(fd, buf, left));
        buf  += rv;
        left -= rv;
    }
    return nb;
}
static inline void assert_write_port8(int fd, void *buf, uint8_t nb_bytes) {
    assert_write(fd, &nb_bytes, 1);
    assert_write(fd, buf, nb_bytes);
}
static inline void assert_write_port32(int fd, void *buf, uint32_t nb_bytes) {
    uint8_t nb_bytes_buf[4] = {
        nb_bytes >> 24,
        nb_bytes >> 16,
        nb_bytes >> 8,
        nb_bytes
    };
    assert_write(fd, &nb_bytes_buf[0], 4);
    assert_write(fd, buf, nb_bytes);
}

#endif

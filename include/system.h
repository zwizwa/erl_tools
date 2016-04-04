#ifndef SYSTEM_H
#define SYSTEM_H

// System interface: logging and failure.
// Default implementation for stdlib.

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

ssize_t raw_read(int fd, void *buf, size_t count);
ssize_t raw_write(int fd, const void *buf, size_t count);


#ifndef LOG
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#endif
#ifndef ERROR
#define ERROR(...) do{LOG(__VA_ARGS__);exit(1);}while(0)
#endif
#ifndef READ
#define READ  raw_read
#endif
#ifndef WRITE
#define WRITE raw_write
#endif

#define ASSERT(assertion) ({ \
            if(!(assertion)) { \
                ERROR("%s: %d: ASSERT FAIL: " #assertion "\n", __FILE__, __LINE__); \
            } })
#define ASSERT_EQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a != _b) { \
                ERROR("ASSERT FAIL: " #a "(%d) == " #b "(%d)\n", _a, _b); \
            } })
#define ASSERT_GT(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a <= _b) { \
                ERROR("ASSERT FAIL: " #a "(%d) <= " #b "(%d)\n", _a, _b); \
            } })
#define ASSERT_ERRNO(a) ({ \
            __typeof__(a) _a = (a); \
            if(-1 == (_a)) { \
                ERROR("ASSERT FAIL: " #a ", errno = %d, %s\n", errno, strerror(errno)); \
            } })



#endif

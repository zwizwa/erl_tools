#include "system.h"
#include <stdio.h>
#include <string.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


// FIXME
#ifndef MAIN
#define MAIN poll_main
#endif

#ifndef BUILDINFO
#define BUILDINFO ""
#endif

#define EVENTS (POLLPRI | POLLERR)

int MAIN(int argc, char **argv) {

    if (argc < 2) {
        LOG("%s" BUILDINFO "\n", argv[0]);
        LOG("usage: %s <gpio_value_path>\n", argv[0]);
        exit(1);
    }
    int nb_fd = argc-1;
    char **path = argv+1;
    struct pollfd pfd[nb_fd];
    for (int i=0; i<nb_fd; i++) {
        pfd[i].revents = 0;
        pfd[i].events = EVENTS;
        ASSERT_ERRNO(pfd[i].fd = open(path[i], O_RDONLY));
    }
    for(;;) {
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd[0], nb_fd, -1)); // block indefinitely
        ASSERT(rv > 0);
        for (int i=0; i<nb_fd; i++) {
            if (pfd[i].revents & EVENTS) {
                lseek(pfd[i].fd, 0, SEEK_SET);
                char buf[3] = {};
                ASSERT(2 == read(pfd[i].fd, buf, sizeof(buf)));
                int val = atoi(buf);
                printf("%d,%d\n", i, val);
                fflush(stdout);
            }
        }
    }
}

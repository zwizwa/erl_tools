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

#define GPIO_EVENTS (POLLPRI | POLLERR)
#define STDIN_EVENTS (POLLHUP | POLLIN)


int MAIN(int argc, char **argv) {

    int nb_fd = argc-1;
    char **gpio_nb = argv+1;
    struct pollfd pfd[nb_fd + 1];
    // Watch all GPIO nodes
    for (int i=0; i<nb_fd; i++) {
        pfd[i].revents = 0;
        pfd[i].events = GPIO_EVENTS;
        char path[100];
        sprintf(path, "/sys/class/gpio/gpio%s/value", gpio_nb[i]);
        ASSERT_ERRNO(pfd[i].fd = open(path, O_RDONLY));
    }
    // and remote close.
    pfd[nb_fd].fd = 0;
    pfd[nb_fd].revents = 0;
    pfd[nb_fd].events = STDIN_EVENTS;

    for(;;) {
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd[0], nb_fd + 1, -1));
        ASSERT(rv >= 0);
        if (pfd[nb_fd].revents & STDIN_EVENTS) {
            exit(0);
        }
        for (int i=0; i<nb_fd; i++) {
            if (pfd[i].revents & GPIO_EVENTS) {
                lseek(pfd[i].fd, 0, SEEK_SET);
                char buf[3] = {};
                ASSERT(2 == read(pfd[i].fd, buf, sizeof(buf)));
                int val = atoi(buf);
                printf("%d,%d\n", i, val);
            }
        }
        fflush(stdout);

    }
}

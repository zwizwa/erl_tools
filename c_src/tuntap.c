#include "system.h"
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <linux/if.h>
#include <linux/if_tun.h>

#ifndef MAIN
#define MAIN poll_main
#endif

// https://stackoverflow.com/questions/1003684/how-to-interface-with-the-linux-tun-driver
// https://www.kernel.org/doc/Documentation/networking/tuntap.txt
// https://wiki.wireshark.org/Development/LibpcapFileFormat

static void log_packet(uint8_t *buf, ssize_t n) {
    for(ssize_t i=0; i<n; i++) {
        LOG(" %02x", buf[i]);
        if (i%16 == 15) LOG("\n");
    }
    LOG("\n");
}

static inline void test_send(int fd) {
    while(1) {
        sleep(1);
        uint8_t test[] = {
            0xff,0xff,0xff,0xff,0xff,0xff,
            0xff,0xff,0xff,0xff,0xff,0xff,
            0x20,0x20,
            1,2,3,4
        };
        write(fd, &test[0], sizeof(test));
    }
}
static inline void test_recv(int fd, const char *ifname) {
    while(1) {
        uint8_t buf[2048];
        ssize_t nbytes;
        ASSERT((nbytes = read(fd, &buf[0], sizeof(buf))) > 0);
        LOG("%s: (%d)\n", ifname, (int)nbytes);
        log_packet(&buf[0], nbytes);
    }
}

int MAIN(int argc, char **argv) {
    ASSERT(argc > 1);
    int fd;
    ASSERT_ERRNO(fd = open("/dev/net/tun", O_RDWR));
    LOG("%s %d\n", argv[0], fd);
    struct ifreq ifr = { .ifr_flags = IFF_TAP | IFF_NO_PI };
    strncpy(ifr.ifr_name, argv[1], IFNAMSIZ);
    ASSERT_ERRNO(ioctl(fd, TUNSETIFF, (void *) &ifr));

    //test_send(fd);
    test_recv(fd, ifr.ifr_name);
    return 0;
}

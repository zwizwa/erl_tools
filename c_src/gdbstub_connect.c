#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <stdint.h>
#include "system.h"

// This is supposed to be run through authorized_keys COMMAND=
// mechanism, which allows fine-grained access control to perform
// update of port binaries.

int main(int argc, char **argv) {
    const char *devname = getenv("SSH_ORIGINAL_COMMAND");
    ASSERT(devname);
    // FIXME: Assert that devname contains a valid device name.
    // Command injection is possible.
    LOG("devname = %s\n", devname);
    char socat[1024] = {};
    snprintf(socat, sizeof(socat)-1, "socat %s,raw,echo=0 -", devname);
    LOG("%s\n", socat);
    system(socat);
}

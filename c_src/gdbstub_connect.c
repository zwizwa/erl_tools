#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <stdint.h>
#include "system.h"

// This is supposed to be run through authorized_keys COMMAND=
// mechanism, which allows fine-grained access control to perform
// update of port binaries.

static int socat(const char *devname) {
    // FIXME: Assert that devname contains a valid device name.
    // Command injection is possible.
    LOG("devname = %s\n", devname);
    char socat[1024] = {};
    snprintf(socat, sizeof(socat)-1, "socat %s,raw,echo=0 -", devname);
    LOG("%s\n", socat);
    system(socat);
    return 0;
}
int main(int argc, char **argv) {
    const char *devname;
    // Direct invocation
    if ((argc == 2) && (devname = argv[1])) {
        return socat(devname);
    }
    // Associated to dedicated key in authorized_keys, in which case
    // this env var contains just the arg, not the command.
    if ((devname = getenv("SSH_ORIGINAL_COMMAND"))) {
        return socat(devname);
    }
    ASSERT(devname);
}

#!/bin/bash


cd $(dirname $0)
[ -z "$ELF" ] && ELF=$(readlink -f ./axo_connect.dynamic.host.elf)

test_usb() {

set -x

# FIXME: This has been postponed in favor of a dedicated program for axo.
# FIXME: This has to run on a physical host.
ABS_ELF="/i/$HOSTNAME/$ELF"
echo ABS_ELF=$ABS_ELF
(ssh 10.1.3.12 $ABS_ELF) 2>&1

}

# test_usb

exit 0

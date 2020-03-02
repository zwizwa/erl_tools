#!/bin/bash

# This can be executed with pwd != the build dir.  We use the location
# of the .o file to figure out where we are.

ERL_TOOLS=$(dirname $O)/..
. $ERL_TOOLS/c_src/env.$ARCH.sh
set -x
$GCC \
    -I$ERL_TOOLS/include \
    -I$ERL_TOOLS/c_src \
    $CFLAGS \
    $CFLAGS_EXTRA \
    -MD -MF $D \
    -DFIRMWARE=\"$FIRMWARE\" \
    -DBUILD=\"$VERSION\" \
    -o $O \
    -c $C

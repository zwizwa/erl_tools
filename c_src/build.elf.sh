ERL_TOOLS=$(dirname $E)/..
. $ERL_TOOLS/c_src/env.$ARCH.sh

# The LD name is fake. Use linker's defaults.
if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
    echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
    exit 1
fi
set -x
$GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS $ELF_LDLIBS

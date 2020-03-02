UC_TOOLS=$(dirname $E)/..
. $UC_TOOLS/linux/env.$ARCH.sh

# The LD name is fake. Use linker's defaults.
if [ "$LD" != dynamic.$ARCH.ld ]; then
    echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
    exit 1
fi
$GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS

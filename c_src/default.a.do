# $1 target
# $2 basename
# $3 temp

# Object filename encodes target architecture.
ARCH="${2##*.}"
ENV=env.$ARCH.sh
redo-ifchange env.sh
. ./env.sh
O_LIB=$(echo $C_LIB | sed "s/\\.c/\\.$ARCH.o/g")
redo-ifchange $O_LIB
# echo $O_LIB >&2
ar -r $3 $O_LIB 2>/dev/null

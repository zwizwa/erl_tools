# $1 target
# $2 basename
# $3 temp

# Object filename encodes target architecture.
ARCH="${2##*.}"
ENV=env.$ARCH.sh
C=$(basename $2 .$ARCH).c
LIB_A=lib.$ARCH.a
redo-ifchange env.sh $ENV $C $LIB_A

. ./$ENV

# Library dependencies are stored in a comment in the main C file.
eval $(grep '^ELF_' <$C)


O=$2.o
redo-ifchange $O
$GCC $LDFLAGS -o $3 $O $LDLIBS $ELF_LDLIBS $LIB_A




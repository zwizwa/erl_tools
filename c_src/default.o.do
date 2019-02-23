#!/bin/bash
# $1 target
# $2 basename
# $3 temp

# Object filename encodes target architecture.
O=$1
ARCH="${2##*.}"
C=$(basename $2 .$ARCH).c
ENV=$ARCH.env.sh

# e.g.: foo.c -> foo.arch.o
# echo "$C -> $O" >&2


redo-ifchange $C env.sh $ENV
. ./$ENV

$GCC $CFLAGS -MD -MF $3.deps.tmp -o $3 -c $C >&2 || exit 1

# Transform the Makefile style dependency list into just a list of
# prerequisites.
DEPS=$(sed -e "s/^$3://" -e 's/\\//g' <$3.deps.tmp)
rm -f $3.deps.tmp
redo-ifchange $DEPS

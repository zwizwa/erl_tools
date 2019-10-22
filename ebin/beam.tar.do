# It's _much_ faster to start erlc once and have it compile everything

redo-ifchange $(ls ../src/*.erl ../src/*.hrl)

DIR=$(readlink -f .)
SRC=$(readlink -f ../src)
erlc -I $SRC $SRC/*.erl >&2
tar cf $3 *.beam >&2
rm -f *.beam >&2


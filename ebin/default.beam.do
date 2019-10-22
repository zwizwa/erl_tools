mkdir -p tmp
erlc -o tmp -I ../src ../src/$2.erl >&2
mv tmp/$2.beam $3


# So this is really slow.  It's better to have erlc compile multiple files.

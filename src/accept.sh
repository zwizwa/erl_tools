#!/bin/bash
for f in *.erl.expect; do
    if [ -f $f.new ]; then
        echo $f
        cp -a $f.new $f
    fi
done

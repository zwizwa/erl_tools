#!/bin/bash
for f in *.expect; do
    if [ -f $f.new ]; then
        echo $f
        cp -a $f.new $f
        rm -f $f.new
    fi
done

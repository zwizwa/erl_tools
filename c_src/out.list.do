# Create a list of targets

# Allow top level to override this
[ -z "$TARGETS" ] && TARGETS=host

APPS="v4l sqlite3 gdbstub_connect"

# The convention is to use one item per line.
for target in $TARGETS; do
    for app in $APPS; do 
        echo $app.$target.elf >>$3; 
    done
done

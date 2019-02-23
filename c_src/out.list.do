# Create a list of targets
TARGETS="\
v4l.host.elf \
sqlite3.host.elf \
gdbstub_connect.host.elf \
"
redo-ifchange $TARGETS

# The convention is to use one item per line.
for target in $TARGETS; do echo $target >>$3; done



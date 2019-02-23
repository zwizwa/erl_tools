CFLAGS="-I../include -Wall -DMAIN=main -DREAD=read -DWRITE=write"
# FIXME: do this per elf
LDFLAGS="-ljpeg"

LDFLAGS_v4l="-ljpeg"

# C sources for static library
C_LIB="bert.c"

# C sources for application binaries
C_APP="sqlite3.c v4l.c gdbstub_connect.c"

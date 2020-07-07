CFLAGS="-I../include -Wall -DMAIN=main -DREAD=read -DWRITE=write"

# C sources for static library
C_LIB="bert.c"

# C sources for application binaries
C_APP="sqlite3.c v4l.c gdbstub_connect.c"

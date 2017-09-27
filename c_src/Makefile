
# Allow optional targets to be specified as an environment variable
# passed to the top level rebar3.  E.g.:
# export ERL_TOOLS_EXTRA_TARGETS=sqlite3.host.elf
TARGETS := sqlite3.elf $(ERL_TOOLS_EXTRA_TARGETS)

.PHONY: all
all: $(patsubst %,../priv/%,$(TARGETS))

.PHONY: clean
clean:
	rm -rf *.o ../priv/*.elf

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write


# Optional host build, e.g. for running unit tests while cross-compiling.
HOST_LIB_O := bert.host.o
%.host.o: %.c
	[ ! -z "$(HOST_CC)" ]
	$(HOST_CC) $(HOST_CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)
../priv/%.host.elf: %.host.o $(HOST_LIB_O)
	[ ! -z "$(HOST_CC)" ]
	mkdir -p ../priv
	$(HOST_CC) $(HOST_LDFLAGS) -o $@ $< $(HOST_LIB_O) -lsqlite3
	file $@

# Default target build
LIB_O := bert.o
%.o: %.c
	. ./default.env ; $(CC) $(CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)

../priv/%.elf: %.o $(LIB_O)
	mkdir -p ../priv
	. ./default.env ; $(CC) $(LDFLAGS) -o $@ $< $(LIB_O) -lsqlite3
	file $@


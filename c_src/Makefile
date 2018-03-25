# Allow optional targets to be specified as an environment variable
# passed to the top level rebar3.  E.g.:
# export ERL_TOOLS_EXTRA_TARGETS=sqlite3.host.elf

# To support multi-platform builds, we use GCC's -dumpmachine output
# as a directory name.
ERL_TOOLS_BUILD_DIR=$(REBAR_BUILD_DIR)/erl_tools
ERL_TOOLS_BIN_DIR=$(ERL_TOOLS_BUILD_DIR)/$(shell $(CC) -dumpmachine)

TARGETS := $(ERL_TOOLS_BIN_DIR)/sqlite3.elf $(ERL_TOOLS_EXTRA_TARGETS)

.PHONY: all
all: $(TARGETS) #$(patsubst %,../priv/%,$(TARGETS))
	set >$(ERL_TOOLS_BUILD_DIR)/env.log
	@echo Building in $(REBAR_BUILD_DIR)

.PHONY: clean
clean:
	rm -rf *.o ../priv/*.elf

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write


# Default target build
LIB_O := bert.o
%.o: %.c
	. ./default.env ; $(CC) $(CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)

# Rebar makes_build/default/lib/erl_tools/priv a symlink to the source
# tree.  Binaries are supposed to go there so that doesn't make any
# sense.  Patch it here, so multi-platform releases don't step on each
# other's toes.
#.PHONY: patch_priv
#patch_priv:
#	[ -h ../priv

$(ERL_TOOLS_BIN_DIR)/%.elf: %.o $(LIB_O)
	mkdir -p $(ERL_TOOLS_BIN_DIR)/bin
	. ./default.env ; $(CC) $(LDFLAGS) -o $@ $< $(LIB_O) -lsqlite3
	file $@
	(cd ../priv ; ln -sf $@ .) # preserve previous semantics



# Secondary "host" build using different environment variables.  This
# is useful for building host binaries during a cross-compile,
# e.g. for running tests.
HOST_LIB_O := host/bert.o
host/%.o: %.c
	[ ! -z "$(HOST_CC)" ]
	mkdir -p host
	$(HOST_CC) $(HOST_CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)
../priv/host/%.elf: host/%.o $(HOST_LIB_O)
	[ ! -z "$(HOST_CC)" ]
	mkdir -p ../priv/host
	$(HOST_CC) $(HOST_LDFLAGS) -o $@ $< $(HOST_LIB_O) -lsqlite3
	file $@

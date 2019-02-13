

# Allow optional targets to be specified as an environment variable
# passed to the top level rebar3.  E.g.:
# export ERL_TOOLS_EXTRA_TARGETS=sqlite3.host.elf

# To support multi-platform builds, we use GCC's -dumpmachine output
# as a directory name under which to store the target binary.  We then
# link the binary into the source's priv/ dir EVERY TIME such that the
# release tool can follow that symlink to create the tarball.  Note
# that this means release builds cannot be run in parallell for
# different architectures!

ERL_TOOLS_BUILD_DIR=$(REBAR_BUILD_DIR)/erl_tools

# FIXME: The multiple targets approach doesn't work very well.
# It seems simplest to use symbolic links for _build instead.

#ERL_TOOLS_BIN_DIR=$(ERL_TOOLS_BUILD_DIR)/$(shell $(CC) -dumpmachine)
ERL_TOOLS_BIN_DIR=$(ERL_TOOLS_BUILD_DIR)/priv

SQLITE3_ELF = $(ERL_TOOLS_BIN_DIR)/sqlite3.elf
GPIO_POLL_ELF = $(ERL_TOOLS_BIN_DIR)/gpio_poll.elf
TARGETS := $(SQLITE3_ELF) $(GPIO_POLL_ELF)

.PHONY: all
all: $(TARGETS) $(patsubst %,../priv/%,$(ERL_TOOLS_EXTRA_TARGETS))
	set >$(ERL_TOOLS_BUILD_DIR)/env.log
	@echo Building in $(REBAR_BUILD_DIR)
	(cd ../priv ; ln -sf $(SQLITE3_ELF) .) # re-create 'current target' link for release tool

.PHONY: clean
clean:
	rm -rf *.o ../priv/*.elf

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write


# Default target build
LIB_O := bert.o
%.o: %.c
	. ./default.env ; $(CC) $(CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)

$(ERL_TOOLS_BIN_DIR)/%.elf: %.o $(LIB_O)
	mkdir -p $(ERL_TOOLS_BIN_DIR)
	. ./default.env ; $(CC) $(LDFLAGS) -o $@ $< $(LIB_O) -lsqlite3
	file $@



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



# Simple stand-alone build
build/host/%.elf: %.c Makefile
	mkdir -p $$(dirname $@)
	gcc -o $@ $< -I../include -DMAIN=main -Wall -ljpeg -g

# OpenWRT SDK for nexx router.  This assumes cross project is
# available under:
CROSS := /i/cross
build/nexx/%.elf: %.c Makefile
	[ ! -z "$(CROSS)" ]
	mkdir -p $$(dirname $@)
	. $(CROSS)/env-nexx.sh ; mipsel-openwrt-linux-gcc -Os -o $@ $< -I../include -DMAIN=main -Wall -Werror -ljpeg
	file $@




# Create a list of build targets to push.  As a side effect, this
# should rebuild the files if necessary.
PUSH_LIST := build/host/v4l.elf build/nexx/v4l.elf
push.list: $(PUSH_LIST)
	echo $(PUSH_LIST) >$@
# Perform individual file deployment
%.push: %
	[ ! -z "$$DST" ]
	../../../apps/exo/priv/ssh/exo_ssh.sh $$DST update bin/$$(basename $*) <$<

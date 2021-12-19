## FIXME: remove this from distribution.

.PHONY: all test clean dialyzer eunit accept

REBAR = ../rebar3/rebar3

# Gradually Nixifying this, so make it easy to configure what is built.
ALL ?= rebar_compile

all: $(ALL)

rebar_compile: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

clean: $(REBAR)
	rm -f `find -name '*.elf'` ; $(REBAR) clean --all ; rm -rf _build typer.hrl ; 

# Cross compilation testing
cross.%:
	. ~/devtools/env.$* ; make ; 


# FIXME: the .target files are for external build scripts.

dialyzer:
	echo eunit >.target
	$(REBAR) dialyzer ; cat _build/default/*.dialyzer_warnings

eunit:
	echo eunit >.target
	rm -f src/*.erl.expect.new
	export ERL_TOOLS_SRC=`readlink -f src` ; $(REBAR) eunit | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"

accept: eunit
	cd src ; ./accept.sh
	git diff src/*.erl.expect


# FIXME: clean this up
# It reuses the PLT file created by "rebar3 dialyzer"
typer.hrl:
	rm -f $@
	typer --plt `find _build/default -name  '*_plt'` -r -pz _build/default `find src/*.erl` >$@.tmp
	mv $@.tmp $@

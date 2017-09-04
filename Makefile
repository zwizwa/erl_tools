## FIXME: remove this from distribution.

REBAR = ~/devtools/rebar3/rebar3

all: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

clean: $(REBAR)
	$(REBAR) clean --all ; rm -rf _build

# Cross compilation testing
cross.%:
	. ~/devtools/env.$* ; make ; 

dialyzer:
	$(REBAR) dialyzer ; cat _build/default/*.dialyzer_warnings

eunit:
	rm -f src/*.erl.expect.new
	export ERL_TOOLS_SRC=`readlink -f src` ; $(REBAR) eunit | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"

eunit_accept: eunit
	cd src ; ./accept.sh
	git diff src/*.erl.expect



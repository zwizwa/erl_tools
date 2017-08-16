## FIXME: remove this from distribution.

REBAR = ~/devtools/rebar3/rebar3

all: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

clean: $(REBAR)
	$(REBAR) clean --all ; rm -rf _build

# Cross compilation testing
%.cross:
	. ~/devtools/$*.env ; make

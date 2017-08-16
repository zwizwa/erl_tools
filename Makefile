REBAR = rebar3/rebar3

all: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

clean: $(REBAR)
	$(REBAR) clean --all ; rm -rf _build

$(REBAR):
	git clone https://github.com/erlang/rebar3.git
	cd rebar3 ; ./bootstrap

# Cross compilation testing
%.cross: clean
	. ~/cross/$*.env ; make

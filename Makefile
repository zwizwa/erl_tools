PROJECT = erl_tools
PROJECT_DESCRIPTION = Erlang Tools
PROJECT_VERSION = 0.0.1

erlang.mk:
	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

include erlang.mk

mrproper: clean
	rm -rf .erlang.mk

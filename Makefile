PROJECT = erl_tools
PROJECT_DESCRIPTION = Erlang Tools
PROJECT_VERSION = 0.0.1


include erlang.mk

erlang.mk:
	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

mrproper: clean
	rm -rf .erlang.mk

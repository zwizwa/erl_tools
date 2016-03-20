PROJECT = erl_tools
PROJECT_DESCRIPTION = Erlang Tools
PROJECT_VERSION = 0.0.1


include erlang.mk

mrproper: clean
	rm -rf .erlang.mk

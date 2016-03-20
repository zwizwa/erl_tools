PROJECT = erl_tools
PROJECT_DESCRIPTION = Erlang Tools
PROJECT_VERSION = 0.0.1


include erlang.mk

erlang.mk: .bootstrap
.bootstrap:
	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
	touch .bootstrap

mrproper: clean
	rm -rf .erlang.mk

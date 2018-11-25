%% Interface to the exo framework.
%% What is exo?
%% - an incremental build system
%% - a network operating system

%% This interface attempts to reduce the number of assumptions made.
%% More specifically, it should be easy to connect into an exo system
%% without actually running the exo Erlang application.  It seems to
%% be simplest to use a single root exo node, or one per hard-wired
%% network segment.

-module(exo_lib).
-export([]).


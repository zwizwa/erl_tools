%% Instantiate an epid_app subgraph as a set of C macro invocations.
%% Generic code.  See exo_patch.erl for specific code.

%% Example of a manually coded C fragment that uses mod_cproc.c from
%% uc_tools
%%
%% uint32_t in = hw_gpio_read(IN);
%% LET(in_edge,       /*=*/ proc_edge, in);
%% LET(in_edge_count, /*=*/ proc_acc,  in_edge.out);
%% if (in_edge.out) {
%%     infof("count = %d\n", in_edge_count);
%% }

%% FIXME: I'm going to implement this in lab_board.erl first.

-module(epid_cproc).
-export([example/0]).


example() ->
    ok.

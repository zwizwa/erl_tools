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
-export([example/0, compile/2]).


example() ->
    LocalPid = local_pid,
    Env = #{
      13 => {input,#{in => {epid,'A',1}}},
      14 => {input,#{in => {epid,'B',2}}},
      15 => {count,#{in => {epid,LocalPid,14}}},
      16 => {count,#{in => {epid,LocalPid,13}}}
     },
    compile(LocalPid, Env).

maps_to_list_sorted(M) ->
    [{K,maps:get(K,M)} || K <- lists:sort(maps:keys(M))].

compile(LocalPid, Env) ->

    %% Separate internal and external references, as they are handled
    %% differently.  External references
    {Im, Nm} =
        maps:fold(
          fun(N, {Proc, Inputs}, {Is, Ns}) ->
                  case Proc of
                      input ->
                          %% External epids
                          {maps:put(N,maps:get(in, Inputs), Is), Ns};
                      _ ->
                          %% All processor inputs are "simple",
                          %% e.g. internal nodes.
                          InNodes =
                              maps:map(
                                fun(_InName, InEpid) ->
                                        case InEpid of
                                            {epid, LocalPid, InN} -> InN
                                        end
                                end,
                                Inputs),
                          {Is, maps:put(N, {Proc, InNodes}, Ns)}
                  end
          end,
          {#{},#{}},
          Env),
    Is = tools:enumerate(maps_to_list_sorted(Im)),
    Ns = maps_to_list_sorted(Nm),
    #{ inputs => Is, procs => Ns}.
    

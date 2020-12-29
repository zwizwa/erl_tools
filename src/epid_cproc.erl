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
-export([example/0, compile/2, code/1]).


example() ->
    LocalPid = local_pid,
    Env = #{
      13 => {input,#{in => {epid,'A',1}}},
      14 => {input,#{in => {epid,'B',2}}},
      15 => {count,#{in => {epid,LocalPid,14}}},
      16 => {count,#{in => {epid,LocalPid,13}}}
     },
    Reduced = compile(LocalPid, Env),
    Code = code(Reduced),
    log:info("Reduced:~n~p~nCode:~n~s", [Reduced, Code]),
    ok.

env_to_seq(M) ->
    [{K,maps:get(K,M)} || K <- lists:sort(maps:keys(M))].

%% Separate internal and external references, as they are handled
%% differently.  The resulting representation can be used to generate
%% the C code and the Erlang dispatcher.
compile(LocalPid, Env) ->

    {Im, Nm} =
        maps:fold(
          fun(N, {Proc, Inputs}, {Is, Ns}) ->
                  case Proc of
                      input ->
                          %% External epids
                          InEpid = maps:get(in, Inputs),
                          {maps:put(N,InEpid, Is),
                           Ns};
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
                          {Is,
                           maps:put(N, {Proc, InNodes}, Ns)}
                  end
          end,
          {#{},#{}},
          Env),
    Is = env_to_seq(Im),
    Ns = env_to_seq(Nm),
    #{ inputs => Is, procs => Ns}.
    
%% This generates let.h syntax for mod_cproc.c conventions.
%%
%% Note that inputs are named to stick with the assumption throughout
%% that epid_app inputs are named.  The LET() macro uses an array
%% initializer to implement this.  We do what is convenient; constant
%% propagation is left to the C compiler.

code(Reduced) ->
    sink:gen_to_list(fun(Sink) -> code(Sink, Reduced) end).

code(Sink, _Reduced = #{ inputs := Inputs, procs := Procs }) ->
    EInputs = tools:enumerate(Inputs),
    InputIndex = maps:from_list([{N,I} || {I,{N,_}} <- EInputs]),
    lists:foreach(
      fun({Node, {Proc, InNodes}}) ->
              Sink({data,
                    ["LET(n", integer_to_list(Node),
                     ", proc_", atom_to_list(Proc)
                    ]}),
              lists:foreach(
                fun({InName, Node}) ->
                        Sink({data,
                              [", .", atom_to_list(InName), " = ",
                       case maps:find(Node, InputIndex) of
                           {ok, Index} ->
                               ["input[",
                                integer_to_list(Index),
                                "]"];
                           error ->
                               ["n",
                                integer_to_list(Node),
                                ".out"]
                       end]})
                end,
                maps:to_list(InNodes)),
              Sink({data,");\n"})
      end,
      Procs),
    Sink(eof).


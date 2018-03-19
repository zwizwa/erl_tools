-module(reflection).
-export([module_has_export/2, module_source/1, load_erl/3]).

%% The point of the code below is to have "immediate" code
%% distribution on edit.  It creates a fast path, reusing the rebar3
%% directory structure, e.g:

%% tom@panda:~/erl_tools/_build/default$ find -name 'tools.*'
%% ./lib/erl_tools/ebin/tools.beam

module_has_export(Module,Export) ->
    MI = erlang:get_module_info(Module),
    E = proplists:get_value(exports, MI),
    lists:member(Export,E).

module_source(Module) when is_atom(Module) ->
    MI = erlang:get_module_info(Module),
    C = proplists:get_value(compile, MI),
    S = proplists:get_value(source, C),
    [Erl,_,Package|_] = lists:reverse(re:split(S,"/")),
    {Package,Erl}.

%% Prefix points into a rebar3 directory structure, e.g. ".../_build/default"
load_erl(Prefix,Source,Nodes) ->
    BSource = tools:format_binary("~s",[Source]),
    [ModName, <<>>] = re:split(Source, ".erl"),
    Module = binary_to_existing_atom(ModName, utf8),
    _Info = {Package, BSource} = module_source(Module),
    Path = tools:format("~s/lib/~s/src/~p.erl", [Prefix, Package, Module]),
    log:info("diag:load_erl ~p~n", [{_Info,Nodes}]),
    {ok, Module, BeamCode} = compile:file(Path,[binary]),
    Load = fun(Node) ->
              rpc:call(Node,log,info,["load_binary ~p~n", [_Info]]),
              rpc:call(Node,code,load_binary,[Module,Path,BeamCode])
           end,
    tools:pmap(
      fun({Node,OnLoad}) ->
              %% Load and execute
              Load(Node), 
              case module_has_export(Module,{OnLoad,0}) of
                  false -> ignore;
                  true -> rpc:call(Node,Module,OnLoad,[])
              end;
         (Node) ->
              %% Only load
              Load(Node)
      end,
      Nodes).


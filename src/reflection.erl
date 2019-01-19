-module(reflection).
-export([module_has_export/2,
         module_source/1, module_source_unpack/1, module_source_raw/1,
         sync_file/3, update_file/3,
         inotifywait/1, inotifywait_handle/2, push_erl_change/2,
         load_erl/3, run_erl/1, run_beam/3]).

%% The point of the code below is to have "immediate" code
%% distribution on edit.  It creates a fast path, reusing the rebar3
%% directory structure, e.g:

%% tom@panda:~/erl_tools/_build/default$ find -name 'tools.*'
%% ./lib/erl_tools/ebin/tools.beam

module_has_export(Module,Export) ->
    MI = erlang:get_module_info(Module),
    E = proplists:get_value(exports, MI),
    lists:member(Export,E).

module_source_raw(Module) ->
    Info = erlang:get_module_info(Module),
    Compile = proplists:get_value(compile, Info),
    File = proplists:get_value(source, Compile),
    File.


%% Often for cross-compilation, the source is not available on the
%% target.  However, during debugging, it is ok to assume that the
%% developer's machine has the source available but that it might not
%% be in the exact location that is embedded in the target beam file.

%% Old approach.

module_source(Module) when is_atom(Module) ->
    module_source_unpack(module_source_raw(Module)).
module_source_unpack(File) ->
    [Erl,_,Package|_] = lists:reverse(re:split(File,"/")),
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





%% Run misc .erl files on build host instance.
scan(IOList) ->
    Str = binary_to_list(
            iolist_to_binary(IOList)),
    {ok, Toks, _} = erl_scan:string(Str),
    %% io:format("~p~n",[{IOList,Str,Toks}]),
    Toks.
parse(Toks) ->
    {ok, Form} = erl_parse:parse_form(Toks),
    %% io:format("~p~n",[Form]),
    Form.
run_erl(ErlFile) ->
    io:format("~p~n",[ErlFile]),  
    {ok, ScriptBin} = file:read_file(ErlFile),
    {ok, Module, BeamCode} = compile:forms(parse(scan(ScriptBin))),
    _ = code:load_binary(Module, ErlFile, BeamCode),
    apply(Module,run,[]).

run_beam(StrModule, ErlFile, BeamFile) ->
    io:format("~p~n",[BeamFile]),  
    {ok, BeamCode} = file:read_file(BeamFile),
    Module = list_to_atom(StrModule),
    _ = code:load_binary(Module, ErlFile, BeamCode),
    apply(Module,run,[]).


%% 2019-01-18: Another attempt to get proper "immediate updates".  The
%% current project's incremental update scripts are getting too slow,
%% so here's another approach.
%%
%% Components:
%%
%% - inotifywatch wrapper
%%
%% - rsync-like file copy in Erlang
%%
%% There is no point in doing this unless it is fast, so what about
%% properly designing it that way.  One important optimization is to
%% eliminate connection overhead.  To do this, run a daemon on an
%% Erlang node on the development machine, let it watch file
%% modifications, and let it compile and upload code without leaving
%% the Erlang VM.

%% Note that the point of this is to make Erlang code updates fast.
%% However, the tool should also handle other file types and run an
%% external compiler.

%% In addition to compilation and file transfer, the tool should also
%% perform smart reloads.  This logic needs to be part of the project.
%% This module only contains library tools.



%% FIXME: This preserves attributes, while for some installs the
%% target should be root:root

%% FIXME: Getting the list of attributes can be done in parallel on
%% host and target.



%% Instead of syncing, use an event-based approach.  Start from a
%% synced state, then use file watches.  This is probably more useful.

%% There is a C extension for inotify, but it seems more useful to
%% just use inotifywait from the inotify-tools Debian package.

inotifywait(#{ files := Files } = Config) ->
    inotifywait(
      maps:put(cmd,
               iolist_to_binary(
                 ["inotifywait -m",
                  [[" ", File] || File <- Files]]),
               Config));
inotifywait(#{ cmd := Cmd, handle := _Handle } = Config) ->
    serv:start(
      {handler,
       fun() ->
               Opts = [{line, 1000}, binary, use_stdio, exit_status],
               Port = open_port({spawn, Cmd}, Opts),
               maps:merge(Config, #{ port => Port })
       end,
       fun reflection:inotifywait_handle/2}).
inotifywait_handle({Port, {exit_status,_}=E}, _State = #{port := Port}) ->
    log:info("~p~n",[E]),
    exit(E);
inotifywait_handle({Port, {data, {eol, Line}}},
                   State = #{port := Port, handle := Handle }) ->
    %% FIXME: This assumes the file names have no spaces.  Since this
    %% is an ad-hoc tool, I'm not going to bother with handling that
    %% case.  If you have spaces in your path, you already know you're
    %% asking for trouble.
    case re:split(Line, " ") of
        [File, EventsC | _] ->
            %% It seems convenient to unpack multiple events here.
            %% It's not clear why inotifywait doesn't do this.
            lists:foreach(
              fun(Event) -> Handle({inotify, {File, Event}}, State) end,
              re:split(EventsC, ","));
        _ ->
            %% log:info("WARNING: inotifywait_handle: ~p~n", [Line]),
            ok
    end,
    State;
inotifywait_handle(Msg, State) ->
    obj:handle(Msg, State).




sync_file(LocalFile, Node, RemoteFile) ->
    case filelib:last_modified(LocalFile) of
        0 -> throw({no_such_file, LocalFile});
        Modified ->
            case rpc:call(Node, filelib, last_modified, [RemoteFile]) of
                Modified -> same;
                {badrpc, nodedown}=E -> throw(E);
                _ -> copy_file(LocalFile, Node, RemoteFile), copied
            end
    end.

%% Compile the file inside the VM.  Note this requires that the paths
%% are set properly to allow for include files.
push_erl_change(File, #{ path := Path, nodes := Nodes }) ->
    Opts = [verbose,report_errors,report_warnings,binary],
    case compile:file(Path(File), Opts) of
        {ok, Mod, Bin} ->
            _ = tools:pmap(
                  fun(Node) ->
                          RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,
                          RemoteFile = RPC(code,which,[Mod]),
                          %% log:info("pushing ~p to ~p, ~s~n", [Mod, Node, RemoteFile]),
                          reflection:update_file(Node, RemoteFile, Bin),
                          RPC(code,purge,[Mod]),
                          RPC(code,load_file,[Mod])
                  end,
                  Nodes);
        error ->
            ok
    end.


update_file(Node, RemoteFile, Bin) when is_atom(Node) and is_binary(Bin) ->
    RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,
    {ok, FileInfo} = RPC(file,read_file_info,[RemoteFile]), 
    _ = RPC(file,delete,[RemoteFile]), %% For executables
    ok = RPC(file,write_file,[RemoteFile,Bin]),
    ok = RPC(file,write_file_info,[RemoteFile,FileInfo]),
    ok.

copy_file(LocalFile, Node, RemoteFile) ->
    {ok, FileInfo} = file:read_file_info(LocalFile),
    RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,
    _ = RPC(file,delete,[RemoteFile]), %% For executables
    {ok, Bin} = file:read_file(LocalFile),
    ok = RPC(file,write_file,[RemoteFile,Bin]),
    ok = RPC(file,write_file_info,[RemoteFile,FileInfo]),
    ok.



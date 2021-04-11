-module(reflection).
-export([module_has_export/2, module_has_export/3,
         module_source/1, module_source_unpack/1, module_source_raw/1,
         sync_file/3, update_file/4, fileinfo/1,
         inotifywait/1, inotifywait_handle/2, push_erl_change/2,
         push_expect/2, run_expect/1,
         load_erl/3, run_erl/1, run_beam/3, run_module/3,
         push_change/2, push_change/3,
         describe_build_product/2, push_build_product/4,
         module_info_source/1,
         find_parent/2, redo/2, redo/3, copy/2, clone_module/2]).

%% 2019-03-08 This code has been in substantial flux in recent weeks,
%% and has been lifted from exo without being cleaned up completely.

module_has_export(Module,Export) ->
    module_has_export(node(), Module, Export).

module_has_export(Node,Module,Export) ->
    MI = rpc:call(Node,erlang,get_module_info,[Module]),
    E = proplists:get_value(exports, MI),
    lists:member(Export,E).

module_info_source(Info) ->
    Compile = proplists:get_value(compile, Info),
    File = proplists:get_value(source, Compile),
    File.

module_source_raw(Module) ->
    Info =
        try
            erlang:get_module_info(Module)
        catch 
            error:badarg ->
                %%throw({not_loaded, Module})
                log:info("~p not loaded~n", [Module]),
                _ = c:l(Module),
                erlang:get_module_info(Module)
        end,
    module_info_source(Info).
            


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


beam_file(ErlFile) ->
    tools:format("~s",[re:replace(ErlFile, "erl$", "beam")]).

run_module(ErlFile,Run,Args) ->
    case compile:file(ErlFile,[binary]) of
        {ok, Module, BeamCode} ->
            case code:load_binary(Module, beam_file(ErlFile), BeamCode) of
                {module, Module} ->
                    apply(Module,Run,Args);
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

run_beam(Module, ErlFile, BeamFile) when is_atom(Module) ->
    io:format("~p~n",[BeamFile]),  
    {ok, BeamCode} = file:read_file(BeamFile),
    _ = code:load_binary(Module, ErlFile, BeamCode),
    apply(Module,run,[]);
run_beam(StrModule, ErlFile, BeamFile) when is_list(StrModule) ->
    Module = list_to_atom(StrModule),
    run_beam(Module, ErlFile, BeamFile).


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

inotifywait(#{ cmd := Cmd, handle := _Handle } = Config) ->
    serv:start(
      {handler,
       fun() ->
               Opts = [{line, 1000}, binary, use_stdio, exit_status],
               Port = open_port({spawn, Cmd}, Opts),
               maps:merge(Config, #{ port => Port })
       end,
       fun ?MODULE:inotifywait_handle/2});

inotifywait(#{ files := Files, handle := _Handle } = Config) ->
    inotifywait(
      maps:put(cmd,
               iolist_to_binary(
                 ["inotifywait -m",
                  [[" ", File] || File <- Files]]),
               Config)).


inotifywait_handle({Port, {exit_status,_}=E}, _State = #{port := Port}) ->
    log:info("~p~n",[E]),
    exit(E);
inotifywait_handle({Port, {data, {eol, Line}}},
                   State = #{port := Port, handle := Handle }) ->
    %% log:info("~p~n",[Line]),
    %% FIXME: This assumes the file names have no spaces.  Since this
    %% is an ad-hoc tool, I'm not going to bother with handling that
    %% case.  If you have spaces in your path, you already know you're
    %% asking for trouble.
    case re:split(Line, " ") of
        [File, EventsC | _] ->
            %% It seems convenient to unpack multiple events here.
            %% It's not clear why inotifywait doesn't do this.
            lists:foldl(
              Handle, State,
              [{inotify, {File, Event}}
               || Event <- re:split(EventsC, ",")]);

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


%% Let it fail asap if node is bad.
make_rpc(Node) ->
    fun(M,F,A) -> rpc:call(Node,M,F,A) end.


%% FIXME: Return compiler output so it can be sent to a compilation
%% buffer.

%% Compile the file inside the VM.  Note this requires that the paths
%% are set properly to allow for include files.
push_erl_change(File, #{ nodes := Nodes } = Env) ->
    %% log:info("push_erl_change: ~p~n", [{File,Nodes}]),
    Path = maps:get(path, Env, fun(F) -> F end),
    Opts = [verbose,
            %% report_errors,report_warnings,
            return_errors, return_warnings,
            binary,
            {d,'TEST'}  %% Push test code as well
           ],
    case compile:file(Path(File), Opts) of
        %% FIXME: Warnings are ignored.
        {ok, Mod, Bin, _Warnings} ->
            NodeReport =
                tools:pmap(
                  fun(Node) -> push_erl_beam(Env, Node, Mod, Bin) end,
                  Nodes),
            Report = 
                [{src,File},
                 {beam_md5,crypto:hash(md5, Bin)}]
                ++ maps:to_list(NodeReport),


            %% FIXME: make a proper report
            Short = tools:format("~p", [Nodes]),
            Long  = tools:format("~p", [Report]),
            {ok, {Short, Long}};

        {error, Errors, _Warnings} ->
            {error,
             {see_compiler_output, 
              iolist_to_binary(
                ["?MODULE:push_erl_change:\n",
                 lists:map(fun format_error/1, Errors)])}}
    end.

format_line(none) ->
    "?";
format_line(Line) ->
    integer_to_list(Line).

format_error({File,ErrorInfos}) ->
    [[File,": ",format_line(Line),": ",
      %% FIXME: this should be done by compile module somehow
      try io_lib:format("~s",[ErrorDescriptor])
      catch _:_ -> io_lib:format("~p",[ErrorDescriptor]) end,
     "\n"]
     %% Can this be more than one, or always signleton list?
     || {Line,_Mod,ErrorDescriptor} <- ErrorInfos].

push_erl_beam(Env, Node, Mod, Bin) ->
    %% log:info("push_erl_beam/4: ~p~n", [{Node,Mod}]),
    RPC = make_rpc(Node),
    MaybeEbin = maps:find(ebin, Env),
    case {MaybeEbin,RPC(code,which,[Mod])} of
        {_, {badrpc,nodedown}=Report} ->
            %% log:info("Node ~p is down~n", [Node]),
            Report;
        {{ok, Ebin}, non_existing} ->
            Path = Ebin(Node),
            log:info("Node ~p, mod ~p, using default path ~s~n", [Node,Mod,Path]),
            RemoteFile = tools:format("~s/~s.beam", [Path, Mod]),
            push_erl_beam(Env, Node, Mod, Bin, RemoteFile);
        {_, non_existing} ->
            log:info("Node ~p, mod ~p: no default path~n", [Node,Mod]),
            {non_existing, no_default};
        {_, RemoteFile} ->
            %% log:info("Node ~p, mod ~p: overwriting~n", [Node,Mod]),
            push_erl_beam(Env, Node, Mod, Bin, RemoteFile)
    end.
push_erl_beam(Env, Node, Mod, Bin, RemoteFile) ->
    %% log:info("push_erl_beam/5: ~p~n", [{Node,Mod,RemoteFile}]),
    RPC = make_rpc(Node),
    %% log:info("pushing ~p to ~p, ~s~n", [Mod, Node, RemoteFile]),
    Rv1 = try 
           ?MODULE:update_file(Env, Node, RemoteFile, Bin)
        catch
            %% Do this as error recovery.
            %% Doing it every time is too
            %% expensive.
            {update_file,{{error,erofs},_,_}}=_Msg ->
                log:info("~p~n",[_Msg]),
                case maps:find(remount_rw, Env) of
                    {ok, RemountRw} ->
                        RemountRw(Node),
                        ?MODULE:update_file(Env, Node, RemoteFile, Bin);
                    _ ->
                        %% Can't modify.  Just load the beam file to memory
                        log:info("WARNING: ~p: Can't write, loading to memory~n", [Mod]),
                        {error, erofs}
                end
        end,
    Rv2 = RPC(code,purge,[Mod]),
    Rv3 = RPC(code,load_file,[Mod]),
    %% _ = RPC(log,info,["load: ~p~n",[Mod]]),
    _ = RPC(log,info,["load: ~p~n",[{Mod,RemoteFile}]]),

    %% Optionally, run some code after loading.  This is kept fairly
    %% generic, in the form of code executed on the build host.

    %% log:info("Env = ~p~n", [Env]),
    OnLoadReport =
        case maps:find(on_load, Env) of
            {ok, OnLoad} when is_function(OnLoad) ->
                [{onload, OnLoad(maps:merge(Env, #{ node => Node, mod => Mod }))}];
            _ ->
                []
        end,
    [{file,RemoteFile},
     {update,Rv1},
     {purge,Rv2},
     {load,Rv3}] ++ OnLoadReport.


%% While Erlang changes are simple because they can be made on a per
%% module basis, this is usually not the case for other dependencies.
%% How to properly build and upload multi-file components?  Needed:
%% - map single file to final build product
%% - call "make" on that build product
%% - upload it
%% - run the associated restart code


%% FIXME: Do update time stamp.
%% FIXME: Define a better error handling strategy.
update_file(Env, Node, RemoteFile, Bin) when is_atom(Node) and is_binary(Bin) ->

    %% log:info("update_file ~p~n",[{Node,RemoteFile,size(Bin)}]),
    RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,

    case RPC(file,read_file_info,[RemoteFile]) of
        {badrpc,nodedown}=E ->
            log:info("Node ~p is down.~n",[Node]),{error,E};
        {error, enoent}=_E ->
            log:info("Node ~p doesn't have ~s~n",[Node,RemoteFile]),
            FileInfo =
                case maps:find(fileinfo, Env) of
                    {ok, FI} -> FI;
                    _ ->  fileinfo(#{})
                end,
            log:info("FIXME: using ~p~n", [FileInfo]),
            update_file(Env, Node, RemoteFile, Bin, FileInfo);
        {ok, FileInfo} ->
            %% log:info("File exists: ~p:~s,~nFileInfo=~p~n", [Node, RemoteFile, FileInfo]),
            update_file(Env, Node, RemoteFile, Bin, FileInfo)
    end.
update_file(_Env, Node, RemoteFile, Bin, FileInfo) ->
    %% log:info("update_file/5: ~p~n", [{Node,RemoteFile,FileInfo}]),
    RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,
    _ = RPC(file,delete,[RemoteFile]), %% For executables
    WriteRv = RPC(file,write_file,[RemoteFile,Bin]),
    %% log:info("update_file/5: WriteRv ~p~n", [WriteRv]),
    case WriteRv of
        ok ->
            ok = RPC(file,write_file_info,[RemoteFile,FileInfo]), ok;
        Err ->
            throw({update_file,{Err,Node,RemoteFile}})
    end.
    
%% http://erlang.org/doc/man/file.html#type-file_info
%% This seems clumsy.
%% use change_mode, change_owner, change_group instead.
fileinfo(Spec) ->
    Mode = maps:get(mode, Spec, 8#100644),
    Uid  = maps:get(uid,  Spec, 1000),
    Gid  = maps:get(gid,  Spec, 1000),

    T = calendar:now_to_local_time(erlang:timestamp()),
    {file_info,
     0, %% size
     regular, %% type
     read_write, %% access
     T,T,T, %% atime, mtime, ctime,
     Mode, %% mode
     1, %% links
     0, %% major device
     0, %% minor device
     0, %% inode
     Uid,  %% uid
     Gid}. %% gid

copy_file(LocalFile, Node, RemoteFile) ->
    {ok, FileInfo} = file:read_file_info(LocalFile),
    RPC = fun (M,F,A) -> rpc:call(Node,M,F,A) end,
    _ = RPC(file,delete,[RemoteFile]), %% For executables
    {ok, Bin} = file:read_file(LocalFile),
    ok = RPC(file,write_file,[RemoteFile,Bin]),
    ok = RPC(file,write_file_info,[RemoteFile,FileInfo]),
    ok.





push_change(File, PushType) ->
    case find_parent(filename:dirname(File), ".push_change") of
        {ok, DotPushChange} ->
            {ok, Bin} = file:read_file(DotPushChange),
            PushChange = type_base:decode({pterm, Bin}),
            %% log:info("push_change: ~p~n", [{File,DotPushChange,PushChange}]),
            Nodes0 = maps:get(node, PushChange),
            push_change({file, File}, Nodes0, PushType);
        _Error  ->
            %% Let's not treat this as an error, since we get all
            %% Emacs save operations.
            Short = tools:format_binary("No .push_change file for ~s", [File]),
            {ok, {Short, Short}}
    end.

%% Old interface: called by Emacs after parsing s-expr file.  Deprecated.
push_change({file, File}, Nodes0, PushType) ->
    %% log:info("push_change/2: ~p~n",[{{file, File}, Nodes0}]),
    Nodes = lists:map(fun resolve_any_node/1, Nodes0),
    Type = tools:filename_extension(File),
    Report = 
        try PushType(Type,File,Nodes)
        catch {C,E} -> {error, {{File, Nodes}, {C,E}}}
        end,
    %% log:info("?MODULE:push_change: ~p~n", [Report]),
    Report.
%%push_change(_Msg, _Nodes, _) ->
%%    Error = {error, {bad_command, _Msg, _Nodes}},
%%    log:info("?MODULE:push_change: ~p~n", [Error]),
%%    Error.



%% resolve_node(A) when is_atom(A) -> A;
resolve_node(B) when is_binary(B) -> binary_to_atom(B,utf8);
resolve_node(L) -> list_to_atom(L).
    
%% Syntax used in .push_node
%% FIXME: This can be removed.  .push_node now uses the result syntax.
resolve_any_node({erl,_}=N) -> N;
resolve_any_node({elf,_}=N) -> N;
resolve_any_node({bin,_}=N) -> N;
resolve_any_node(B) when is_binary(B) ->
    case re:split(B,":") of
        [<<"elf">>,Platform,SSH] ->
            {elf,{Platform,SSH}};
        [Node] ->
            {erl, resolve_node(Node)}
    end;
resolve_any_node(L) when is_list(L) ->
    resolve_any_node(list_to_binary(L)).



%% FIXME: This doesn't support deployment feedback in emacs since
%% bin/push_build_product.sh is only a single-ended notification and can't propagate
%% back to the output or exit value of "redo install".
redo(File, Nodes) ->
    log:info("redo: ~p~n", [File]),
    %% Find the top level directory.
    case redo_root(File) of
        {ok, RedoRoot} ->
            redo(File, Nodes, RedoRoot);
        Other ->
            log:info("Can't find redo root: ~p~n", [{File,Other}])
    end.

redo(File, Nodes, RedoRoot) ->

    PushChangeState = #{ file => File, nodes => Nodes },
    %% log:info("ReadoRoot: ~p~n", [RedoRoot]),
    Cmd = tools:format(
            "bash -c '"
            "export PUSH_CHANGE_STATE=~s ; "
            "export REDO_VERBOSE_ENTER=1 ; "
            "cd ~s ; "
            "./redo.sh "
            "-j$(nproc) "
            "--no-status --no-color install 2>&1"
            "'", 
            [encode(PushChangeState),
             RedoRoot]),
    log:info("redo: start~n"),
    %% log:info("~s~n", [Cmd]),
    %% This used to use run:script_output/2, but since we're not
    %% really doing anything with the output it is best to print
    %% it as early as possible.

    %% {_Out, Rv} =
    %%     case run:script_output(Cmd, infinity) of
    %%         {ok, Out} ->
    %%             {Out,
    %%              {ok, {see_output,
    %%                    tools:format("~s~n~s~n", [Cmd, Out])}}};
    %%         {error, {E, Out}} ->
    %%             Short = tools:format("~p",[{error,E}]),
    %%             Long  = tools:format("~s~n~s~n", [Cmd, Out]),
    %%             {Out,
    %%              {error, {Short, Long}}}
    %%     end,
    %% log:info("redo:~n~s", [_Out]),
    
    {ok, %% Allways runs up to script exit.
     {ExitCode, Out}} =
        run:fold_script(
          Cmd, 
          fun({data,{eol,Line}}, Lines) ->
                  tools:info("~s~n",[Line]),
                  {cont, [Line ++ "\n"|Lines]};
             ({exit_status, ECode},Lines) ->
                  {done, {ECode, lists:flatten(lists:reverse(Lines))}}
          end,
          [],
          infinity,
          [{line, 1024}]),
    Rv = 
        case ExitCode of
            0 -> {ok, {"redo ok", Out}};
            _ -> {error, {"redo error", Out}}
        end,
    log:info("redo: end (exit=~p)~n",[ExitCode]),
    Rv.

redo_root(File) ->
    try
        {ok, DotRedo} = find_parent(filename:dirname(File), ".redo"),
        {ok, filename:dirname(DotRedo)}
    catch _:_ ->
            error
    end.



%% To pass context data through a Erlang -> shell -> Erlang callback
%% sequence.  FIXME: Does this need authentication?
encode(Term) ->
    base64:encode(term_to_binary(Term)).

decode(EncodedTerm) ->
    try {ok, binary_to_term(base64:decode(EncodedTerm))}
    catch C:E -> {error,{C,E}} end.

find_parent(Dir, File) ->
    Path = tools:format("~s/~s", [Dir, File]),
    %% log:info("find_parent: ~p~n", [Path]),
    case filelib:is_file(Path) or filelib:is_dir(Path) of
        false ->
            case Dir of 
                "/" -> error;
                _ -> find_parent(filename:dirname(Dir), File)
            end;
        true ->
            {ok, Path}
    end.
                
                    

copy(File, TypedNodes) ->
    Nodes = [Node || {erl, Node} <- TypedNodes],
    tools:re_case(
      File,
      %% Priv files can be placed in the remote priv directory.
      [{".*/(.*?)/priv/(.*)",
        fun([BApp, Rel]=_Match) ->
                App = list_to_atom(BApp),
                Report =
                    tools:pmap(
                      fun(Node) ->
                              case file:read_file(File) of
                                  {ok, Bin} ->
                                      Env = #{},
                                      case rpc:call(Node, code, priv_dir, [App]) of
                                          %%{badrpc,_}=E ->
                                          %%    throw({reflection_copy,Node,E});
                                          {badrpc,nodedown}=Report_ ->
                                              Report_;
                                          Priv ->
                                              RemoteFile = tools:format("~s/~s", [Priv, Rel]),
                                              _ = ?MODULE:update_file(Env, Node, RemoteFile, Bin),
                                              {Priv,Rel}
                                      end;
                                  Error ->
                                      throw({Error, File})
                              end
                      end,
                      Nodes),
                %% {error, {app, tools:format_binary("FIXME: ~p",[Report])}}
                Long = tools:format("~p", [Report]),
                {ok, {<<"OK">>, Long}}
        end},
       %% Anything else is likely project-specific.  See exo dispatch_build_product.
       {"",
        fun(_) ->
                Short = tools:format("reflection:copy line ~p",[?LINE]),
                Long  = tools:format("~s:~nproject-specific report needed for:~n~p~n",[Short, {File,Nodes}]),
                %% log:info(Long),
                {error, {Short, Long}}
        end}]).


%% redo install ->
%% some application specific notification mechansm (push.sh) ->
%% push_build_product ->
%% some application specific deployment

%% FIXME: This is a lot of language interface jumping, but I don't
%% really see a good way to do this better given the tools. 
%%
%% Some thoughts:
%% 1. Erlang is good to sit between user and build system
%% 2. Erlang is also good to perform the deployment
%% 3. In the middle sits the redo build system, which should be stand-alone


%% $ cat ~/exo/bin/push_build_product.sh 
%% #!/bin/bash
%% # FIXME: This script should be generic.
%% # echo $0 $* >&2
%% echo "push_build_product $(readlink -f .) $1 $PUSH_CHANGE_STATE" | socat - TCP:localhost:12345


push_build_product(SrcPath, RelPath, PushChangeStateEncoded, DispatchBuildProduct) ->
    case decode(PushChangeStateEncoded) of 
        {ok, _PushChangeState = #{ nodes := Nodes }} ->
            %% log:info("push_build_product: nodes: ~p~n", [Nodes]),
            Desc = reflection:describe_build_product(SrcPath,RelPath),
            _ = tools:pmap(
                  fun(Node) ->
                          %% log:info("push_build_product: node: ~p~n", [Node]),
                          DispatchBuildProduct(Node, Desc) end,
                  Nodes);
        _ ->
            Error = {no_push_context, {SrcPath, RelPath}},
            log:info("~999p~n", [Error])
    end.




%% Map path and filename to a description that is easier to match on
%% in dispatch_build_product/2
%% For now this is very exo-specific.  Parameterize.
describe_build_product(SrcPath, RelPath) ->
    Dir  = filename:dirname(RelPath),
    File = filename:basename(RelPath),
    Common = #{
      from_push => {SrcPath,RelPath},
      dir  => Dir,
      file => File,
      path => tools:format("~s/~s", [SrcPath, Dir])
     },
    Specific =
        case re:split(File,"\\.") of
            %% Most .do files in the project use architecture encoding
            %% in the filename.
            [Base,Arch,Ext] ->
                #{ base => Base,
                   ext  => b2a(Ext),
                   arch => b2a(Arch) };
            %% When there is more variation than just host.  This
            %% 'dot' is passed on to the target.
            [Base,Kind,Arch,Ext] ->
                #{ base => tools:format_binary("~s.~s",[Base,Kind]),
                   ext  => b2a(Ext),
                   arch => b2a(Arch) };

            %% FIXME: This is for rdm_bridge.elf, which doesn't use
            %% the "host" tag.  Issue a warning.
            [Base,<<"elf">>=Ext] ->
                log:info("WARNING: naked elf: ~s.~s~n",[Base,Ext]),
                #{ base => Base,
                   ext  => b2a(Ext),
                   arch => host };

            %% Special cases
            [Base,<<"js">>=Ext] ->
                #{ base => Base,
                   ext  => b2a(Ext),
                   arch => b2a(Ext) };
            [Base] ->
                #{ base => Base,
                   ext  => none,
                   arch => none }
        end,
    maps:merge(Common, Specific).

b2a(B) -> binary_to_atom(B,utf8).
    

clone_module(Node, Module) ->
    RPC = fun(M,F,A) -> rpc:call(Node, M, F, A) end,
    BeamFile = code:which(Module),
    {ok, Bin} = file:read_file(BeamFile),
    _ = RPC(code,purge,[Module]),
    {module, Module} = RPC(code,load_binary,[Module,BeamFile,Bin]),
    ok.

%% This maps module to expect file. 
%% FIXME: We also need a way to do the reverse.
run_expect(Mod) ->
    try
        %% Just ask the module
        run_expect(Mod, Mod:expect_file())
    catch error:undef ->
            log:info("reflection:run_expect: Module '~p' "
                     "doesn't export expect_file/0~n", [Mod])
    end.
   
run_expect(Mod,ExpectFile) ->
    %% Compilation worked, now execute the test.
    log:info("expect: running ~p:expect_test()~n", [Mod]),
    Report = (catch Mod:expect_test()),
    log:info("report: ~p~n", [Report]),
    log:info("updating: ~s~n", [ExpectFile]),
    _ = file:copy(ExpectFile ++ ".new", ExpectFile),
    %% Notify emacs.  FIXME: This is currently hardcoded
    emacs:revert(filename:basename(ExpectFile)).


%% .expect files are always contained inside an Erlang module.  How to
%% find those files?  To reduce need for annotation, it is assumed
%% that the .expect file's basename is the module name, which then is
%% used to find the source.

push_expect(F,PushErl) ->

    Dir = filename:dirname(F),
    BN = filename:basename(F, ".expect"),

    log:info("expect: ~p~n", [{Dir,BN}]),
    Mod = list_to_atom(BN),
    Erl = module_source_raw(Mod),

    %% Only push to build host, since nobody else has the source files.
    case PushErl(Erl, [{erl, node()}]) of
        {ok,_}=OK ->
            _ = run_expect(Mod,F),
            OK;
        Error ->
            Error
    end.

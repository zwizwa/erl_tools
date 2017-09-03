-module(expect).
-export([run/2, run/3, cmd/1, update/1,
         %% Old format
         load/1, save/2,
         %% New format
         parse_trace_file/1]).

%% Inspired by: https://blog.janestreet.com/testing-with-expectations/
%% Main idea:
%% - Make it trivial to add a test
%% - Diff of the expect file indicates change of meaning / error
%% - A committed diff indicates accepted change of meaning

%% In-place operation on file.
run(InFile, OutFile) ->
    run(InFile, OutFile, #{}).

run(InFile, OutFile, Additional) when is_map(Additional) ->
    Old = load(InFile),
    New = update(maps:merge(Old, Additional)),
    save(OutFile, New),
    maps:from_list(
      [{Key, maps:get(Key, New)}
       || Key <- maps:keys(Additional)]).

load(FileName) ->
    try
        {ok, Bin} = file:read_file(FileName),
        Str = binary_to_list(Bin),
        {ok, Tokens, _} = erl_scan:string(Str),  %% Str contains trailing '.'
        {ok, Tests = #{type := expect}} = erl_parse:parse_term(Tokens),
        maps:remove(type, Tests)
    catch
        _:_ -> #{}
    end.

save(FileName, New) ->
    IOList = 
        ["%% -*- erlang -*-\n",
         "#{ type => expect",
         [io_lib:format(",~n   ~p~n   => ~p", [Spec,Result]) 
          || {Spec,Result} <- maps:to_list(New)],
         "\n}.\n"],
    %% log:info("~s",[IOList]),
    ok = file:write_file(FileName, IOList).
    

update(Tests) ->
    maps:map(
      fun(Expr, _) -> catch cmd(Expr) end,
      Tests).

cmd({Mod,Fun,Args}) ->
    apply(Mod,Fun,Args);
cmd(Str) -> 
    {ok,Toks,_} = erl_scan:string(Str),
    {ok,[Expr]} = erl_parse:parse_exprs(Toks),
    {value,Val,_} = erl_eval:expr(Expr,[]),
    Val.

    
    


%% Putting this here for now.  Later, transition to this format for
%% the the code above.  The idea is to use erl_parse:parse_exprs
%% because it keeps track of line numbers.

%% Trace file format reuses erlang map syntax.
parse_trace_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Str = tools:format("#{~s}.",[Bin]),
    {ok,Toks,_}=erl_scan:string(Str),
    {ok,[{map,_,Assocs}]} = erl_parse:parse_exprs(Toks),
    [{Line, eval(K), eval(V)} 
     || {map_field_assoc,Line,K,V} <- Assocs].

eval(AbsStx) ->       
    {value,Val,_} = erl_eval:expr(AbsStx,[]),
    Val.
     



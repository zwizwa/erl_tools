-module(expect).
-export([run/2, run/3, cmd/1, update/1, update/2,
         %% Old format
         load/1, save/2, save/3,
         %% New format
         load_form/1, update_form/3, update_form/2, save_form/2,
         print_diff/2,
         %% Used for custom trace tests
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
        {ok, Tests = #{type := _ }} = erl_parse:parse_term(Tokens),
        maps:remove(type, Tests)
    catch
        _:_ -> #{}
    end.

save(FileName, New) ->
    save(FileName, New, expect).
save(FileName, New, FileTypeTag) ->
    IOList = 
        ["%% -*- erlang -*-\n",
         "#{ type => ",
         io_lib:format("~p",[FileTypeTag]),
         [io_lib:format(",~n   ~p~n   => ~p", [Spec,Result]) 
          || {Spec,Result} <- maps:to_list(New)],
         "\n}.\n"],
    %% log:info("~s",[IOList]),
    ok = file:write_file(FileName, IOList).
    

update(Tests) ->
    update(Tests, fun cmd/1).
update(Tests, Eval) when is_function(Eval) ->
    maps:map(
      fun(Expr, _) -> catch Eval(Expr) end,
      Tests);
update(Tests, Functions) when is_map(Functions) ->
    update(
      Tests,
      %% Useful for testing internal functions.
      fun({Fun,Args}) -> apply(maps:get(Fun,Functions),Args);
         (Cmd) -> cmd(Cmd)
      end).
                          
    


%% FIXME: specify abstract "apply" function in test specs, but do not save it to file

cmd({Mod,Fun,Args}) when is_atom(Mod) and is_atom(Fun) ->
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
     


%% Experiment: trying a .hrl file

%% - file is included in an -ifdef(TEST) section, this will:
%%   - allow it to be compiled during test
%%   - keep it out of the main build
%%


%% Syntax: single thunk, containing a single clause, containing a
%% single Term which is an assoc list from expressions to terms.
load_form(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Str = tools:format("~s",[Bin]),
    {ok,Toks,_} = erl_scan:string(Str),

    {ok, Form} = erl_parse:parse_form(Toks),
    unpack(Form).

%% Some ad-hoc formatting.  Can't figure out how to have
%% erl_prettypr:format display strings and binaries in a readable way.
save_form(FileName, {FunName, Pairs}) ->
    ok = file:write_file(
           FileName,
           ["%% -*- erlang -*-\n",
            atom_to_list(FunName),"() ->\n[\n",
            lists:join(
              ",\n",
              [["{ ",
                ["%" || _ <- lists:seq(1,78)],
                "\n",
                erl_prettypr:format(Form),
                "\n, %% =>\n",
                io_lib:format("~p",[Val]),
                "\n}\n"]
               || {Form,Val} <- Pairs]),
            "].\n"]).

%% save_form(FileName, Form) ->
%%     Str = erl_prettypr:format(pack(Form)),
%%     ok = file:write_file(
%%            FileName,
%%            ["%% -*- erlang -*-\n", Str]).
      
%% Full file.    
unpack(
  {function,_,FunName,0,
   [{clause,_,[],[],
     [Term]}]}) ->
    {FunName, unpack_list(Term)}.
%% Unpack the assoc list, parsing the second element in the pair but
%% leaving the first intact.
unpack_list({nil,_}) -> [];
unpack_list({cons,_,{tuple,_,[Expr,Term]},Tail}) ->
    [{Expr,erl_parse:normalise(Term)} | unpack_list(Tail)].

%% pack({FunName,List}) ->
%%     {function,0,FunName,0,
%%      [{clause,0,[],[],
%%        [pack_list(List)]}]}.
%% pack_list([]) -> {nil,0};
%% pack_list([{Expr,Term}|Tail]) -> 
%%     {cons,0,{tuple,0,[Expr,erl_parse:abstract(Term)]},
%%      pack_list(Tail)}.
                 
                 
    
%% Check an evaluated form with the previous values, and write it
%% back.
update_form(FileIn,
            FileOut,
            TestResults) ->
    {Name,Old} = load_form(FileIn),
    {Forms,_} = lists:unzip(Old),
    {NewVals,_} = lists:unzip(TestResults),
    New = lists:zip(Forms,NewVals),
    save_form(FileOut, {Name,New}),
    %% Return diff.
    lists:append(
      lists:map(
        fun({_,{Old,Old}}) -> [];
           (Different) -> [Different] end,
        lists:zip(Forms, TestResults))).

print_diff(FileName, Diff) ->
    lists:foreach(
      fun({Form,{Old,New}}) ->
              io:format(
                "~s:~p: ~s~n- ~p~n+ ~p~n",
                [FileName,
                 erl_syntax:get_pos(Form),
                 erl_prettypr:format(Form),
                 Old,
                 New])
      end,
      Diff).

update_form(FileIn, TestResults) ->
    update_form(FileIn, FileIn ++ ".new", TestResults).


%% expect:check_form("/home/tom/src/scope_display:expect_tests().

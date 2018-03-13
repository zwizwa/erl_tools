-module(expect).
-export([load_form/1, update_form/3, update_form/2, save_form/2,
         print_diff/2, diff_form/3, run_form/2]).

%% Inspired by: https://blog.janestreet.com/testing-with-expectations/
%% Main idea:
%% - Make it trivial to add a test
%% - Diff of the expect file indicates change of meaning / error
%% - A committed diff indicates accepted change of meaning

%% This is integrated with the build system.  See Makefile rules
%% "expect" and "expect_accept".

%% Boilerplate example.
%% This includes an .expect file for this module.
%% See tools.erl for another example
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("expect.expect").
expect_test() ->
    expect:run_form(
      filename:dirname(?FILE)++"/expect.expect",
      fun expect_expect/0).
-endif.


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
            join(
              ",\n",
              [["{ ",
                ["%" || _ <- lists:seq(1,78)],
                "\n",
                erl_prettypr:format(Form),
                "\n, %% =>\n",
                format_val(Val),
                "\n}\n"]
               || {Form,Val} <- Pairs]),
            "].\n"]).

%% Compat with older version.
%% join(Lists,Sep) -> lists:join(Lists,Sep).
join(_, []) -> [];
join(Sep, [First | Els]) -> [First, [[Sep,El] || El <- Els]]. 
    
    

%% Value needs to be parsable, e.g. Can't have #Fun<...>.
%% See type_base.erl for similar code.
format_val(Val) ->
    ValFmt = tools:format_binary("~70p",[Val]),
    try
        Val = type_base:decode({pterm, ValFmt}),
        ValFmt
    catch 
        _:_ -> 
            [[["%% ", Line, "\n"] || Line <- re:split(ValFmt,"\n")],
             "not_printable"]
    end.



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
            TestPairs) ->
    {Name, Old} = load_form(FileIn),
    {Forms, OldVals} = lists:unzip(Old),
    {Tests,_} = lists:unzip(TestPairs),
    NewVals = [catch Test() || Test <- Tests],
    New = lists:zip(Forms, NewVals),
    save_form(FileOut, {Name, New}),
    {Forms,NewVals,OldVals}.


run_form(FileName, TestThunk) ->
    {Forms,NewVals,OldVals} = update_form(FileName, TestThunk()),
    Diff = expect:diff_form(Forms, OldVals, NewVals),
    expect:print_diff(FileName, Diff),
    Diff = [].

    
    

diff_form(Forms, OldVals, NewVals) ->
    %% Return diff.
    lists:append(
      lists:map(
        fun({_,{OldVal,NewVal}}=Test) ->
                case NewVal of
                    OldVal -> [];
                    _ -> [Test]
                end
        end,
        lists:zip(Forms, lists:zip(OldVals, NewVals)))).

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

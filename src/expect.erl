-module(expect).
-export([run/1]).


%% In-place operation on file.
run(FileName) ->
    Old = load(FileName),
    New = update(Old),
    save(FileName ++ ".new", New).


load(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Str = binary_to_list(Bin),
    {ok, Tokens, _} = erl_scan:string(Str),  %% Str contains trailing '.'
    {ok, [expect|Tests]} = erl_parse:parse_term(Tokens),
    Tests.

save(FileName, New) ->
    ok = file:write_file(
           FileName,
           ["%% -*- erlang -*-\n",
            "[expect\n",
            [io_lib:format(",{~p,~n  ~p}~n",
                           [Spec,Result]) || {Spec,Result} <- New],
            "].\n"]).

update(Tests) ->
    [{Cmd, catch apply(Mod,Fun,Args)}
     || {{Mod,Fun,Args}=Cmd,_} <- Tests].


    
    


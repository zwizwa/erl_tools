-module(expect).
-export([run/2, run/3]).

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
    {ok, Bin} = file:read_file(FileName),
    Str = binary_to_list(Bin),
    {ok, Tokens, _} = erl_scan:string(Str),  %% Str contains trailing '.'
    {ok, Tests = #{type := expect}} = erl_parse:parse_term(Tokens),
    maps:remove(type, Tests).

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
      fun({Mod,Fun,Args},_) -> catch apply(Mod,Fun,Args) end,
      Tests).

    
    
    


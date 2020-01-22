-module(sqlite3_ckeys).
-export([create/3, find/3, put/4, test/1]).

%% Bridging relational and functional model seems simplest to do by
%% using composite keys and composite values.  We agument that with a
%% schema for Erlang<->String conversion.

%% The need for this arose in exo_net, which has two sides:
%%
%% - Get/Set update to implement available, eventually consistent
%%   distributed key-value store
%%
%% - Read-only queries on the local relational database.
%%
%%
%% To implement the former, we use composite key structure,
%% e.g. [Table,K1,K2], where the first key refers to the table name,
%% and subsequent keys refer to columns in the table.  Positional
%% encoding is used.
%%
%% To allow for more elaborate queries, use a fully relational model
%% where each of the tables has names that are optimized to perform
%% joins.
%%
%% See exo_net:columns/1 for an example schema.
%%
%% This code seems general enough to fork off into a module.
%%



commas(Syms) ->
     tl(lists:append(
          [[",", atom_to_list(Col)] || Col <- Syms])).
wheres(Syms) ->
     tl(lists:append(
          [[" and ", atom_to_list(Col), "=?"] || Col <- Syms])).

sql(DB,Qs) ->
    sqlite3:sql(DB,Qs).

columns(Schema, Table) when is_atom(Table) ->
    {Keys,Vals} = Schema(Table),
    key_list(Keys) ++ key_list(Vals).

%% CREATE/FIND/PUT QUERIES.  All other queries can be implemented by
%% ad-hoc SQL queries against the relational schema.
create(DB, Schema, Table) ->
    SQL = tools:format_binary(
            "create table if not exists '~p' (~s)",
            [Table, commas(columns(Schema, Table))]),
    log:info("~s~n", [SQL]),
    sql(DB, [{SQL,[]}]),
    ok.

find_q(Schema, Table) ->
    {KeyNames, ValNames} = Schema(Table),
    Sql = tools:format_binary(
            "select ~s from '~p' where ~s",
            [commas(key_list(ValNames)), Table,
             wheres(key_list(KeyNames))]),
    Types = type_list(ValNames),
    {Sql, Types}.
find(DB, Schema, [Table | Keys]) ->
    {Sql,Types} = find_q(Schema, Table),
    Args = [encode(K) || K <- Keys],
    %% log:info("find: ~999p~n",[{Sql,Args}]),
    case sql(DB,[{Sql,Args}]) of
        [[]] -> error;
        [[Vals]] -> {ok, [type:decode(TV) || TV <- lists:zip(Types,Vals)]}
    end.

put_q(Schema, Table) ->
    {KeyNames, ValNames} = Schema(Table),
    Sql1 = tools:format_binary(
             "delete from '~p' where ~s",
             [Table, wheres(key_list(KeyNames))]),
    Cols = key_list(KeyNames) ++ key_list(ValNames),
    Sql2 = tools:format_binary(
             "insert into '~p' (~s) values (~s)",
             [Table, commas(Cols), commas(['?' || _ <- Cols])]),
    Types = type_list(ValNames),
    {Sql1,Sql2,Types}.
put(DB, Schema, [Table | Keys], Vals) ->
    {Sql1,Sql2,Types} = put_q(Schema, Table),
    BKeys = [encode(K) || K <- Keys],
    BVals = [type:encode(TV) || TV <- lists:zip(Types,Vals)],
    Q1 = {Sql1,BKeys},
    Q2 = {Sql2,BKeys++BVals},
    %% log:info("put: ~999p~n",[Q1]),
    %% log:info("put: ~999p~n",[Q2]),
    [[],[]] = sql(DB, [Q1,Q2]),
    ok.




%% TOOLS

%% Default encoding is pterm, but this isn't convenient for everything
%% (e.g. mac addresses).
spec(Key) when is_atom(Key) -> {pterm, Key};
spec({Type,Key}=Spec) when is_atom(Type) and is_atom(Key) -> Spec.
key(Spec)  -> {_,Key}  = spec(Spec), Key.
type(Spec) -> {Type,_} = spec(Spec), Type.

    

key_list(PosToNameMap) ->
    [key(TypeAndKey) || {_, TypeAndKey} <- pos_key_list(PosToNameMap)].
type_list(PosToNameMap) ->
    [type(TypeAndKey) || {_, TypeAndKey} <- pos_key_list(PosToNameMap)].
pos_key_list(PosToNameMap) ->
    lists:sort(maps:to_list(PosToNameMap)).
encode(Term) ->
    type:encode({pterm,Term}).
    
test({find,Key}) ->
    find(exo:db_local(),fun exo_net:columns/1, Key);
test({put,Key,Val}) ->
    put(exo:db_local(),fun exo_net:columns/1, Key, Val);
test(Spec) ->
    throw({test,Spec}).


%% (exo@10.1.3.29)40> sqlite3_ckeys:test({put,{exo_net,abc},{<<"0">>, <<"pterm">>, <<"123">>}}).
%% (exo@10.1.3.29)41> sqlite3_ckeys:test({find,{exo_net,abc}}).
%% [[<<"0">>,<<"pterm">>,<<"123">>]]

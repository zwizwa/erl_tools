-module(sqlite3_ckeys).
-export([create/3, find/3, put/4, test/1, to_list/4]).

%% Bridging relational and functional model seems simplest to do by
%% using composite keys and composite values.  We augment that with a
%% typed schema for Erlang Term <-> SQLite Binary conversion.

%% The need for this arose in exo_net, which has two sides:
%%
%% - Key-value store as a backbone for the AP distributed store,
%%   e.g. storing individual config items
%%
%% - Read-only queries on the local relational database, e.g. config
%%   file generation from SQL joins
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



%% CREATE/FIND/PUT QUERIES.  All other queries can be implemented by
%% ad-hoc SQL queries against the relational schema.
create(DB, Schema, Table) ->
    {Keys,Vals} = Schema(Table),
    KeyCols = key_list(Keys),
    VarCols = key_list(Vals),
    Columns = KeyCols ++ VarCols,

    SQL = tools:format_binary(
            "create table if not exists '~p' (~s,primary key(~s))",
            [Table, commas(Columns), commas(KeyCols)]),
    log:info("~s~n", [SQL]),
    sql(DB, [{SQL,[]}]),
    ok.

%% Tables are created on demand, as long as they are in the schema.
%% This makes it easier to flush a cache during development.
with_create_retry(DB, Schema, Table, Fun) ->
    try
        Fun()
    catch
        {sqlite3_errmsg, Msg} ->
            %% Retry once to create the table if it doesn't exist.
            NoSuchTable =
                tools:format_binary("no such table: ~p", [Table]), 
            case Msg of
                NoSuchTable ->
                    log:info("creating table: ~p~n", [Table]),
                    create(DB, Schema, Table),
                    Fun();
                Err ->
                    throw(Err)
            end
    end.


find_q(Schema, Table) ->
    {KeySpecs, ValSpecs} = Schema(Table),
    Sql = tools:format_binary(
            "select ~s from '~p' where ~s",
            [commas(key_list(ValSpecs)), Table,
             wheres(key_list(KeySpecs))]),
    KeyTypes = type_list(KeySpecs),
    ValTypes = type_list(ValSpecs),
    {Sql, KeyTypes, ValTypes}.

find(DB, Schema, [Table | Keys]) ->
    {Sql,KeyTypes, ValTypes} = find_q(Schema, Table),
    BKeys = [type:encode(TV) || TV <- lists:zip(KeyTypes,Keys)],
    %% log:info("find: ~999p~n",[{Sql,Args}]),
    with_create_retry(
      DB, Schema, Table,
      fun() ->
              case sql(DB,[{Sql,BKeys}]) of
                  [[]] -> error;
                  [[Vals]] -> {ok, [type:decode(TV) || TV <- lists:zip(ValTypes,Vals)]}
              end
      end).

put_q(Schema, Table) ->
    {KeySpecs, ValSpecs} = Schema(Table),
    Sql1 = tools:format_binary(
             "delete from '~p' where ~s",
             [Table, wheres(key_list(KeySpecs))]),
    Cols = key_list(KeySpecs) ++ key_list(ValSpecs),
    Sql2 = tools:format_binary(
             "insert into '~p' (~s) values (~s)",
             [Table, commas(Cols), commas(['?' || _ <- Cols])]),
    KeyTypes = type_list(KeySpecs),
    ValTypes = type_list(ValSpecs),
    {Sql1,Sql2,KeyTypes,ValTypes}.

put(DB, Schema, [Table | Keys], Vals) ->
    {Sql1,Sql2,KeyTypes,ValTypes} = put_q(Schema, Table),
    BKeys = [type:encode(TV) || TV <- lists:zip(KeyTypes,Keys)],
    BVals = [type:encode(TV) || TV <- lists:zip(ValTypes,Vals)],
    Q1 = {Sql1,BKeys},
    Q2 = {Sql2,BKeys++BVals},
    %% log:info("put: ~999p~n",[Q1]),
    %% log:info("put: ~999p~n",[Q2]),
    with_create_retry(
      DB, Schema, Table,
      fun() -> [[],[]] = sql(DB, [Q1,Q2]), ok end).

%% Convert to {Key,Val} list + allow projection (e.g. to only get time stamps).
to_list_q(Schema, Table, Cols) ->
    {VarSpecs, ValSpecs} = Schema(Table),
    Sql = tools:format_binary("select ~s from '~p'", [commas(Cols), Table]),
    ColToType = maps:from_list([{C,T} || {T,C} <- ValSpecs ++ VarSpecs]),
    Types = [maps:get(C, ColToType) || C <- Cols],
    {Sql, Types}.

to_list(DB, Schema, Table, Vars) ->
    {Sql, Types} = to_list_q(Schema, Table, Vars),
    %% log:info("to_list: ~999p~n",[Sql]),
    %% log:info("to_list: ~999p~n",[Types]),
    with_create_retry(
      DB, Schema, Table,
      fun() ->
              [BVals] = sql(DB, [{Sql,[]}]),
              Vals =
                  lists:map(
                    fun(Row) -> [type:decode(TV) || TV <- lists:zip(Types, Row)] end,
                    BVals),
              Vals
      end).


%% TOOLS

%% Column specs use {Type,Name}, where Type can be used with type:encode,decode.

key({_,Key})   -> Key.
type({Type,_}) -> Type.
    
key_list(Columns)  -> lists:map(fun key/1, Columns).
type_list(Columns) -> lists:map(fun type/1, Columns).


commas(Syms) ->
     tl(lists:append(
          [[",", atom_to_list(Col)] || Col <- Syms])).
wheres(Syms) ->
     tl(lists:append(
          [[" and ", atom_to_list(Col), "=?"] || Col <- Syms])).

sql(DB,Qs) ->
    sqlite3:sql(DB,Qs).



%% TEST
    
test({find,Key}) ->
    find(exo:db_local(),fun exo_net:columns/1, Key);
test({put,Key,Val}) ->
    put(exo:db_local(),fun exo_net:columns/1, Key, Val);
test({to_list,Table}) ->
    to_list(exo:db_local(),fun exo_net:columns/1, Table, [ts,var]);

test(Spec) ->
    throw({test,Spec}).



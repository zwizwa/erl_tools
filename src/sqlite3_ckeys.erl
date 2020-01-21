-module(sqlite3_ckeys).
-export([columns/2, key_info/2, schema/2, create/3, find/3, test/1]).

%% Bridge composite keyed find/put with a relational model.  The need
%% for this arose in exo_net, which has two sides:
%%
%% - Get/Set update to implement highly available eventually
%%   consistent distributed key-value store
%%
%% - Read-only queries on the local relational database.
%%
%%
%% To implement the former, we use composite key structure,
%% e.g. {Table,K1,K2}, where the first key refers to the table name,
%% and subsequent keys refer to columns in the table.  Positional
%% encoding is used to make updates simple.
%%
%% To allow for more elaborate queries, use a fully relational model
%% where each of the tables has names that are optimized to perform
%% joins.
%%
%% See exo_net:columns/1 for an example schema.
%%
%% This code seems general enough to fork off into a module.
%%
%% Note: this does not perform Erlang type conversion.  That can be
%% built on top.  Also: FIXME: remove tuple interface.  Lists are more
%% convenient to later put type conversion on top.

%% Keep this simple.  Table is the SQLite table name and is required
%% to be a symbol.  The other keys just need to be serializable as
%% pterm.

%% Given a key, return all the schema information necessary to perform
%% a get/set operation: table, key/value pairs for query, and column
%% names for composite value.

key_info(Schema, {list, [Table|Keys]}) when is_atom(Table) ->
    {KeyNames, ValNames} = Schema(Table),
    KeyNVs = [{maps:get(N,KeyNames),encode(K)} || {N,K} <- pos_key_list(KeyNames)],
    ValNs = [ValName || {_, ValName} <- pos_key_list(ValNames)],
    {Table, lists:zip(KeyNVs,Keys), ValNs};
key_info(Schema, Key) when is_tuple(Key) ->
    key_info(Schema, {list, tuple_to_list(Key)}).


columns(Schema, Table) when is_atom(Table) ->
    {Keys,Vals} = Schema(Table),
    key_list(Keys) ++ key_list(Vals).

commas(Syms) ->
     tl(lists:append(
          [[",", atom_to_list(Col)] || Col <- Syms])).
wheres(Syms) ->
     tl(lists:append(
          [[" and ", atom_to_list(Col), "=?"] || Col <- Syms])).

schema(Schema,Table) ->    
    [$(,commas(columns(Schema,Table)), $)].

sql(DB,Sql,Args) ->
    [QRes] = sqlite3:sql(DB, [{Sql,Args}]),
    QRes.
sql(DB,Sql) ->
    sql(DB,Sql,[]).


%% CREATE/FIND/PUT QUERIES.  All other queries can be implemented by
%% ad-hoc SQL queries against the relational schema.
create(DB, Schema, Table) ->
    SQL = tools:format_binary(
            "create table if not exists '~p' ~s",
            [Table, schema(Schema, Table)]),
    log:info("~s~n", [SQL]),
    sql(DB, SQL),
    ok.

find_q(Schema, Table) ->
    {KeyNames, ValNames} = Schema(Table),
    Sql = tools:format_binary(
            "select ~s from '~p' where ~s",
            [commas(key_list(ValNames)), Table,
             wheres(key_list(KeyNames))]),
    Sql.
find(DB, Schema, {table_and_key_list, [Table | Keys]}) ->
    Sql = find_q(Schema, Table),
    Args = [encode(K) || K <- Keys],
    log:info("find: ~999p~n",[{Sql,Args}]),
    sql(DB,Sql,Args);
        
find(DB, Schema, Key) when is_tuple(Key) ->
    find(DB, Schema, {table_and_key_list, tuple_to_list(Key)}).


%%    sql(DB, <<"insert or replace into 'exo_net' (var,val,ts,type) values (?,?,?,?)">>, 
%%        [encode(Var), BinVal, encode(Ts), encode(Type)]).

%% FIXME: This will need primary key, or replace won't work.  Maybe do
%% drop then insert transaction?
put_q(Schema, Table) ->
    {KeyNames, ValNames} = Schema(Table),
    Sql1 = tools:format_binary(
             "delete from '~p' where ~s",
             [Table, wheres(key_list(KeyNames))]),
    Cols = key_list(KeyNames) ++ key_list(ValNames),
    Sql2 = tools:format_binary(
             "insert into '~p' (~s) values (~s)",
             [Table, commas(Cols), commas(['?' || _ <- Cols])]),
    {Sql1,Sql2}.
put(DB, Schema, {table_and_key_list, [Table | Keys]}, {values, Vals}) ->
    {Sql1,Sql2} = put_q(Schema, Table),
    BKeys = [encode(K) || K <- Keys],
    %% Note: Vals are supposed to be binary.  Encoding is not done in this layer.
    log:info("put: ~999p~n",[{Sql1,BKeys}]),
    log:info("put: ~999p~n",[{Sql2,BKeys++Vals}]),
    [] = sql(DB,Sql1,BKeys),
    [] = sql(DB,Sql2,BKeys++Vals),
    ok;
put(DB, Schema, Key, Val) when is_tuple(Key) and is_tuple(Val)->
    put(DB, Schema,
        {table_and_key_list, tuple_to_list(Key)},
        {values, tuple_to_list(Val)});
put(DB, Schema, Key, Val) when is_tuple(Key) ->
    put(DB, Schema, Key, {Val}).


%% find(DB,Key) ->
%%     SQL = tools:format_binary(
%%             "select ~s from '~p' where 
%%     case 
%%         sql(DB, <<"select val,ts,type from 'exo_net' where var = ?">>, 
%%             [encode(Var)])
%%     of
%%         [[]] ->
%%             {error,{not_found, Var}};
%%         [[[BinVal,BinTs,BinType]]] ->
%%             Val = type:decode({decode(BinType),BinVal}),
%%             {ok, {decode(BinTs),Val}}
%%     end.



%% TOOLS

key_list(PosToNameMap) ->
    [Key || {_, Key} <- pos_key_list(PosToNameMap)].
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

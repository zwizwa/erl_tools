-module(db_html).
-export([table/2, sql/2, sql/3, q/2]).

table(DB, Table) ->
    {table,[],
     [{tr,[],
       [{td,[],[[Col]]}
        || Col <- Row]}
      || Row <- q(DB, {table,Table})]}.


    

sql(DB,Sql) ->
    sql(DB,Sql,[]).
sql(DB,Sql,Args) when is_binary(Sql) and is_list(Args) -> 
    try hd(sqlite3:sql(DB, [{Sql,Args}]))
    catch C:E -> throw({C,E,Sql,Args}) end;
sql(DB,Sql,Args) ->
    sql(DB,iolist_to_binary(Sql),Args).
            
%% Some misc high level queries.

q(DB, tables) ->
    sql(DB, "SELECT name FROM sqlite_master WHERE type='table'");
q(DB, views) ->
    sql(DB, "SELECT name FROM sqlite_master WHERE type='view'");
q(DB, {table, Table}) when is_atom(Table)  ->
    sql(DB, ["SELECT * from ", atom_to_list(Table)]).

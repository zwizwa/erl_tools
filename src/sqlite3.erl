%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to the erl_tools
%% library.
%%
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0

-module(sqlite3).
-export([open/1, close/1, query/3,
         select/5, select_m/5, select/4,
         delete/4,
         insert_or_replace/5,
         insert_or_replace_m/3
        ]).

%% Port operations.
open(DbFile) ->
    Cmd = tools:format("priv/sqlite3.elf ~s", [DbFile]),
    open_port({spawn, Cmd}, [use_stdio, {packet,4}, exit_status, binary]).
close(Port) ->
    Port ! {self(), {command, <<>>}},
    close_flush(Port).
close_flush(Port) ->
    receive
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, _}=E} -> exit(E);
        {Port, _} = M -> 
            log:info("close_flush ~p~n",[M]),
            close_flush(Port)
    end.
query(Port, {SQL, Bindings}=Query, Sink) when is_binary(SQL) and is_list(Bindings)->
    Bin = term_to_binary(Query),
    %% log:info("query: ~p~n",[Query]),
    Port ! {self(), {command, Bin}},
    sync(Port, Sink);
query(Port, {SQL, Bindings}, Sink) ->
    query(Port, {iolist_to_binary(SQL), Bindings}, Sink);
query(Port, SQL, Sink) ->
    query(Port, {SQL,[]}, Sink).


sync(Port, Sink) ->
    receive
        {Port, {data, <<>>}} ->
            Sink(eof);
        {Port, {data, Bin}} ->
            Term = binary_to_term(Bin),
            case Term of
                {error, Msg} -> throw({sql_error, Msg});
                _ -> Sink({data, Term})
            end,
            sync(Port, Sink)
    end.
               


%% Formatters
fmt(F,A) -> tools:format(F,A).
cs(AtomList) -> string:join([fmt("~s",[A]) || A <- AtomList],",").
cl(all) -> "";
cl({where, Expr}) -> fmt("where ~s",[ex(Expr)]).
ex({'and', E1, E2}) -> fmt("~s and ~s",[ex(E1),ex(E2)]);
ex({eq, Col, Val}) -> fmt("~s = '~s'", [atom_to_list(Col), Val]).
     

select(DB,Cols,Table,Clause,Sink) ->
    query(DB, fmt("select ~s from ~s ~s",
                 [cs(Cols),Table,cl(Clause)]), Sink).

select(DB,Table,Clause,Sink) ->
    query(DB, fmt("select * from ~s ~s",
                 [Table,cl(Clause)]), Sink).

select_m(DB,Cols,Table,Clause,Sink) ->
    select(DB,Cols,Table,Clause,
           sink:map(
             fun(Vals) -> 
                     maps:put(
                       type, Table,
                       maps:from_list(
                         lists:zip(Cols,Vals)))
             end, Sink)).

delete(DB,Table,Clause,Sink) ->
    query(DB, fmt("delete from ~s ~s",
                 [Table,cl(Clause)]), Sink).
    

%% To keep the database type structure consistent, all inserts into
%% the database need to pass through this command.

%% Add an additional check to sanitize binding annotation before
%% passing it into the query command.  Add warnings here to track down
%% old loosely typed calls.
typed({text, Bin}=B) when is_binary(Bin) -> B;
typed({blob, Bin}=B) when is_binary(Bin) -> B;
typed(Bin) when is_binary(Bin) -> {text,Bin};
typed(Val) -> 
    ConvVal = {text, tools:as_binary(Val)},
    log:info("WARNING: ~p is converted to ~p before insert~n"
            ,[Val,ConvVal]),
    ConvVal.

%% BValues is a list of {blob|text, binary()} | binary().
%% The default binding type is text.
insert_or_replace(DB,Cols,Table,Values,Sink) ->
    %% log:info("insert_or_replace: args: ~p~n", [{DB,Cols,Table,Values}]),
    [true = is_atom(A) || A <- [Table | Cols]], %% Typecheck
    Query = {fmt("insert or replace into ~s (~s) values (~s)",
               [Table,cs(Cols),cs(["?" || _<- Values])]),
             [typed(V) || V <- Values]},
    %% log:info("insert_or_replace: query: ~p~n",[Query]),
    query(DB, Query, Sink).


%% Insert record represent as map with type field corresponding to
%% column name.
insert_or_replace_m(DB,Map,Sink) ->
    Cols = maps:keys(Map)--[type],
    Table = maps:get(type,Map),
    Values = [maps:get(C, Map) || C <- Cols],
    insert_or_replace(DB,Cols,Table,Values,Sink).

   




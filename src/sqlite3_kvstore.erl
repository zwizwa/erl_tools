-module(sqlite3_kvstore).
-export([
         %% Implementation of kvstore on top of sqlite3.erl
         table_op/2,
         table/1, existing_table/1,
         table_init/2, table_delete/2

         ]).


sql(DB,Q,Bs) -> sqlite3:sql(DB,Q,Bs).
transaction(DB,F) -> sqlite3:transaction(DB,F).
    
    

%% Expose operations directly to allow lambda lifting, reducing stored
%% closure size on client side.  As a penalty, query strings are
%% recomputed, but it is still possible to cache them by storing the
%% result of the function lookup.
table_op({table,TypeMod,DB,Table}, find) ->
    QRead = tools:format_binary(
              "select type,val from ~p where var = ?", [Table]),
    fun(Key) ->
            BinKey = type_base:encode_key(Key),
            case sql(DB, QRead, [{text,BinKey}]) of
                [[_,_] = BinTV] ->
                    {ok, type_base:decode_tv(TypeMod, BinTV)};
                [] ->
                    {error, {not_found, Key}}
            end
    end;

table_op({table,TypeMod,DB,Table}, to_list) ->
    QLoad = tools:format_binary(
              "select var,type,val from ~p", [Table]),
    fun() ->
            [type_base:decode_ktv(TypeMod, BinKTV)
             || BinKTV <- sql(DB, QLoad, [])]
    end;

table_op(Spec, to_map) ->
    ToList = table_op(Spec, to_list),
    table_op(Spec, to_map, ToList);

table_op({table,_TypeMod,DB,Table}, keys) ->
    QKeys = tools:format_binary(
              "select var from ~p", [Table]),
    fun() ->
            [type_base:decode_key(BinKey)
             || [BinKey] <- sql(DB, QKeys, [])]
    end;

table_op({table,TypeMod,DB,Table}, put) ->
    QPut = tools:format_binary(
               "insert or replace into ~p (var,type,val) values (?,?,?)", [Table]),
    fun(K,TV) ->
            BinKTV = type_base:encode_ktv(TypeMod, {K,TV}),
            sql(DB, QPut, BinKTV),
            TV %% For chaining
    end;

table_op(Spec, put_list) ->
    Put = table_op(Spec, put),
    table_op(Spec, put_list, Put);

table_op(Spec, put_map) ->
    PutList = table_op(Spec, put_list),
    table_op(Spec, put_map, PutList);

table_op({table,_,DB,Table}, clear) ->
    QPut = tools:format_binary("delete from ~p", [Table]),
    fun() -> sql(DB, QPut, []), ok end.


%% Sharing

table_op(_, to_map, ToList) ->
    fun() ->
            maps:from_list(ToList())
    end;
table_op({table,_,DB,_}, put_list, Put) ->
    fun(List) ->
            case transaction(
                   DB,
                   fun() ->
                           lists:foreach(
                             fun({Key,TypeVal}) -> Put(Key, TypeVal) end, 
                             List), ok
                   end) of
                {ok, RV} -> RV;
                {error, Error} -> throw({put_list,Error})
            end
    end;
table_op(_, put_map, PutList) ->
    fun(Map) -> PutList(maps:to_list(Map)) end.


    
                   
%% Implementation based on database table and type_base.erl type conversion.
table({table,TypeMod,DB,Table}=Args) when is_atom(Table) and is_atom(TypeMod) ->
    %% Make sure it exists.
    table_init(DB,Table),
    existing_table(Args).

%% This form has more sharing and caching but creates large closures,
%% which is really noticable in hmac encoding.  See db.erl in hatd web
%% app for lambda-lifted form.
existing_table(Spec) ->
    Find      = table_op(Spec, find),
    ToList    = table_op(Spec, to_list),
    ToMap     = table_op(Spec, to_map, ToList),
    Keys      = table_op(Spec, keys),
    Put       = table_op(Spec, put),
    PutList   = table_op(Spec, put_list, Put),
    PutMap    = table_op(Spec, put_map, PutList),
    Clear     = table_op(Spec, clear),
    
    {kvstore, 
     fun
         (find)       -> Find;
         (to_list)    -> ToList;
         (to_map)     -> ToMap;
         (keys)       -> Keys;
         (put)        -> Put;
         (put_list)   -> PutList;
         (put_map)    -> PutMap;
         (clear)      -> Clear
     end}.
                   
%% Ad-hoc key value stores.
table_init(DB,Table) when is_atom(Table) ->
    sql(DB,
        tools:format_binary(
          "create table if not exists ~p (var, type, val, primary key (var))",
          [Table]),
       []).

table_delete(DB, Table) when is_atom(Table) ->
    sql(DB,
        tools:format_binary(
          "drop table if exists ~p",
          [Table]),
        []).



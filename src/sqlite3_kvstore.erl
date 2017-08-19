-module(sqlite3_kvstore).
-export([
         %% Implementation of kvstore on top of sqlite3.erl
         table_op/2,
         table/1, existing_table/1,
         table_init/2, table_delete/2,

         %% Canonical representation for erlang tagged terms.
         encode/2, decode/2


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
            BinKey = encode_key(TypeMod, Key),
            case sql(DB, QRead, [BinKey]) of
                [[_,_] = BinTV] ->
                    {ok, decode_type_val(TypeMod, BinTV)};
                [] ->
                    {error, {not_found, Key}}
            end
    end;

table_op({table,TypeMod,DB,Table}, to_list) ->
    QLoad = tools:format_binary(
              "select var,type,val from ~p", [Table]),
    fun() ->
            [decode(TypeMod, BinTV)
             || BinTV <- sql(DB, QLoad, [])]
    end;

table_op(Spec, to_map) ->
    ToList = table_op(Spec, to_list),
    table_op(Spec, to_map, ToList);

table_op({table,TypeMod,DB,Table}, keys) ->
    QKeys = tools:format_binary(
              "select var from ~p", [Table]),
    fun() ->
            [decode_key(TypeMod, BinKey)
             || [BinKey] <- sql(DB, QKeys, [])]
    end;

table_op({table,TypeMod,DB,Table}, put) ->
    QPut = tools:format_binary(
               "insert or replace into ~p (var,type,val) values (?,?,?)", [Table]),
    fun(K,TV) ->
            BinKTV = encode(TypeMod, {K,TV}),
            sql(DB, QPut, BinKTV),
            ok
    end;

table_op(Spec, put_list) ->
    Put = table_op(Spec, put),
    table_op(Spec, put_list, Put);

table_op(Spec, put_map) ->
    PutList = table_op(Spec, put_list),
    table_op(Spec, put_map, PutList).


%% Sharing

table_op(_, to_map, ToList) ->
    fun() ->
            maps:from_list(ToList())
    end;
table_op({table,_,DB,_}, put_list, Put) ->
    fun(List) ->
            transaction(
              DB, fun() ->
                          lists:foreach(Put, List),
                          ok
                  end)
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
    
    {kvstore, 
     fun
         (find)       -> Find;
         (to_list)    -> ToList;
         (to_map)     -> ToMap;
         (keys)       -> Keys;
         (put)        -> Put;
         (put_list)   -> PutList;
         (put_map)    -> PutMap
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


%% Canonical way to represent type-tagged erlang terms as
%% human-readable binary, for db storage and user interfaces.  See
%% type_base.erl

encode_key(TypeMod,Key) -> apply(TypeMod,encode,[{pterm,Key}]).
decode_key(TypeMod,Key) -> apply(TypeMod,decode,[{pterm,Key}]).

encode(TypeMod, {Key, {Type, Val}}) ->
    [encode_key(TypeMod,Key),
     apply(TypeMod,encode_type,[Type]),
     apply(TypeMod,encode,[{Type,Val}])].

decode_type_val(TypeMod, [BinType, BinVal]) ->
    Type = apply(TypeMod,decode_type,[BinType]),
    {Type, apply(TypeMod,decode,[{Type,BinVal}])}.

decode(TypeMod, [BinKey | BinTV]) ->
    {decode_key(TypeMod, BinKey),
     decode_type_val(TypeMod, BinTV)}.


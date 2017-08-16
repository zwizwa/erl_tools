-module(db_base).
-export([db/3,
         test/0,
         query/3,
         sql/3,
         transaction/2,
         %% Canonical representation for erlang tagged terms.
         encode/2, decode/2,
         %% Implementation of kvstore using DB tables
         kv_table_op/2,
         kv_table/1, kv_existing_table/1,
         kv_table_init/2, kv_table_delete/2,
         %% For reloads
         handle/2]).

%% Process wrapper around sqlite3.erl

test() ->
    ok.

db(Atom, DbFile, DbInit) ->
    ParentPid = self(),
    serv:up(Atom,
            {handler,
             fun() -> 
                     DB = self(),
                     unlink(ParentPid),
                     spawn(fun() -> DbInit(DB) end),
                     #{ db => sqlite3:open(DbFile()) }
             end,
             fun db_base:handle/2}).

handle({Pid, {query, FunName, Args}}, #{db := DB} = State) ->
    Pid ! {self(), obj_reply, 
           sink:gen_to_list(
             fun(Sink) ->
                     apply(sqlite3,FunName,[DB]++Args++[Sink])
             end)},
    State;
handle(Msg,State) ->
    obj:handle(Msg,State).

%% Allow for lazy connections.
-type db() :: fun(() -> pid()).
-spec raw_query(db(),_,[_]) -> [[binary()] | {sqlite3_errmsg,binary()}].
raw_query(DB, FunName, Args) ->
    obj:call(DB(), {query, FunName, Args}).

query(DB, FunName, Args) ->
    Rows = raw_query(DB, FunName, Args),

    %% If reply contains errors we throw them into the caller's
    %% process.  Normal results are [[binary()]].
    lists:foreach(
      fun({sqlite3_errmsg,_}=E) -> throw(E); (_) -> ok end, Rows),
    Rows.

%% Shortcut for raw SQL query
sql(DB, SQL,Bindings) when
      is_binary(SQL) and
      is_list(Bindings) ->
    log:info("query: ~p~n",[{DB,SQL,Bindings}]),
    query(DB, query, [{SQL,Bindings}]).




%% Transactions
begin_transaction(DB) ->
    sql(DB, <<"begin transaction">>,[]).
end_transaction(DB) ->
    sql(DB, <<"end transaction">>,[]).
rollback_transaction(DB) ->
    sql(DB, <<"rollback transaction">>,[]).
    
transaction(DB, Fun) ->
    begin_transaction(DB),
    try 
        Rv = Fun(),
        end_transaction(DB),
        {ok, Rv}
    catch
        C:E ->
            %% Rollback can fail apparently.. Log it because it's
            %% likely a bug elsewhere.
            Err0 = {C,E,erlang:get_stacktrace()},
            try
                rollback_transaction(DB),
                {error, {rollback_ok, Err0}}
            catch 
                %% Why does this happen?
                C1:E1 -> {error, {rollback_fail, {Err0, {C1,E1}}}}
            end
    end.




%% Expose operations directly to allow lambda lifting, reducing stored
%% closure size on client side.  As a penalty, query strings are
%% recomputed, but it is still possible to cache them by storing the
%% result of the function lookup.
kv_table_op({table,TypeMod,DB,Table}, find) ->
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

kv_table_op({table,TypeMod,DB,Table}, to_list) ->
    QLoad = tools:format_binary(
              "select var,type,val from ~p", [Table]),
    fun() ->
            [decode(TypeMod, BinTV)
             || BinTV <- sql(DB, QLoad, [])]
    end;

kv_table_op(Spec, to_map) ->
    ToList = kv_table_op(Spec, to_list),
    kv_table_op(Spec, to_map, ToList);

kv_table_op({table,TypeMod,DB,Table}, keys) ->
    QKeys = tools:format_binary(
              "select var from ~p", [Table]),
    fun() ->
            [decode_key(TypeMod, BinKey)
             || [BinKey] <- sql(DB, QKeys, [])]
    end;

kv_table_op({table,TypeMod,DB,Table}, write) ->
    QWrite = tools:format_binary(
               "insert or replace into ~p (var,type,val) values (?,?,?)", [Table]),
    fun(KTV) ->
            BinKTV = encode(TypeMod, KTV),
            sql(DB, QWrite, BinKTV),
            KTV
    end;

kv_table_op(Spec, write_map) ->
    Write = kv_table_op(Spec, write),
    kv_table_op(Spec, write_map, Write).

kv_table_op(_, to_map, ToList) ->
    fun() ->
            maps:from_list(ToList())
    end;
kv_table_op({table,_,DB,_}, write_map, Write) ->
    fun(Map) ->
            List = maps:to_list(Map),
            transaction(
              DB, fun() ->
                          lists:foreach(Write, List),
                          Map
                  end)
    end.
    
                   
%% Implementation based on database table and type_base.erl type conversion.
kv_table({table,TypeMod,DB,Table}=Args) when is_atom(Table) and is_atom(TypeMod) ->
    %% Make sure it exists.
    kv_table_init(DB,Table),
    kv_existing_table(Args).

%% This form has more sharing and caching but creates large closures,
%% which is really noticable in hmac encoding.  See db.erl in hatd web
%% app for lambda-lifted form.
kv_existing_table(Spec) ->
    Find     = kv_table_op(Spec, find),
    ToList   = kv_table_op(Spec, to_list),
    ToMap    = kv_table_op(Spec, to_map, ToList),
    Keys     = kv_table_op(Spec, keys),
    Write    = kv_table_op(Spec, write),
    WriteMap = kv_table_op(Spec, write_map, Write),
    
    {kvstore, 
     fun
         (find)      -> Find;
         (to_list)   -> ToList;
         (to_map)    -> ToMap;
         (keys)      -> Keys;
         (write)     -> Write;
         (write_map) -> WriteMap
     end}.
                   
%% Ad-hoc key value stores.
kv_table_init(DB,Table) when is_atom(Table) ->
    sql(DB,
        tools:format_binary(
          "create table if not exists ~p (var, type, val, primary key (var))",
          [Table]),
       []).

kv_table_delete(DB, Table) when is_atom(Table) ->
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






%% TODO: cache query compilation.  For each query, sqlite will return
%% the pointer to the compiled query together with the response.  This
%% then enables the driver here to cache the query in its process
%% state.  This would need the following changes:

%% new messages
%% - port sends:
%%   - pointer to query
%% - port receives:
%%   - query string or pointer
%%   - delete compiled query

%% Or turn it around, keeping the C side dumb.  We send the query
%% together with a number so it can be stored in a table.  Only thing
%% C side needs to do is to resize the table if needed.


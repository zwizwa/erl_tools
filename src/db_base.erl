-module(db_base).
-export([db/1,
         query/3,
         sql/3,
         transaction/2,
         %% Canonical representation for erlang tagged terms.
         encode/2, decode/2,
         %% Simple key,value store for erlang application state
         kv_read/2, kv_write/2,
         kvstore/1,
         kvstore_init/2, kvstore_delete/2,
         %% For reloads
         handle/2]).

%% Process wrapper around sqlite3.erl

db(DbFile) ->
    ParentPid = self(),
    serv:up(db,
            {handler,
             fun() -> 
                     unlink(ParentPid),
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
    query(DB, query, [{SQL,Bindings}]).




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

%% Simple key-value store for ad-hoc app data.  This can access a
%% table slice.

kv_write({kvstore, TypeMod, Write, _Read}, KeyTypeVal) ->
    Write(decode(TypeMod, KeyTypeVal)).

kv_read({kvstore, TypeMod, _Write, Read}, Key) ->
    [[_,_] = BinTV] = Read(encode_key(TypeMod, Key)),
    decode_type_val(TypeMod, BinTV).


%% Abstract access from arbitrary tables. Maybe not needed?
kvstore({kvstore_spec,TypeMod,DB,Table,{KCol,TCol,VCol}}) ->
    QWrite =
        tools:format_binary(
          "insert or replace into ~s (~s,~s,~s) values (?,?,?)",
          Table, KCol, TCol, VCol),
    QRead =
        tools:format_binary(
          "select (~s,~s) from ~s where ~s = ?",
          [TCol, VCol, Table, KCol]),
    {kvstore,TypeMod,
     fun(BinKTV) -> sql(DB, QWrite, BinKTV) end,
     fun(BinKey) -> sql(DB, QRead, [BinKey]) end};

%% Canonical single 3-column DB table.
kvstore({table,TypeMod,DB,Table}) when is_atom(Table) and is_atom(TypeMod) ->
    %% Make sure it exists.
    kvstore_init(DB,Table),
    QWrite =
        tools:format_binary(
          "insert or replace into ~p (var,type,val) values (?,?,?)",
          [Table]),
    QRead =
        tools:format_binary(
          "select val_type,val from ~p where var = ? and page = '~p'",
          [Table]),
    {kvstore, TypeMod,
     fun(BinKTV) -> sql(DB,QWrite, BinKTV) end,
     fun(BinKey) -> sql(DB,QRead, [BinKey]) end}.
    



%% Canonical kv stores: tables with 3 fixed columns: var,type,val

%% Ad-hoc key value stores.
kvstore_init(DB,Table) when is_atom(Table) ->
    sql(DB,
        tools:format_binary(
          "create table if not exists ~p (var,type,val)",
          [Table]),
       []).

kvstore_delete(DB, Table) when is_atom(Table) ->
    sql(DB,
        tools:format_binary(
          "drop table if exists ~p",
          [Table]),
        []).





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
            rollback_transaction(DB),
            {error, {C,E,erlang:get_stacktrace()}}
    end.

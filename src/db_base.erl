-module(db_base).
-export([db/3,
         query/3,
         sql/3,
         transaction/2,
         %% For reloads
         handle/2]).

%% Process wrapper around sqlite3.erl

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
    %% log:info("query: ~p~n",[{DB,SQL,Bindings}]),
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


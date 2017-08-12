-module(db_base).
-export([db/1,
         query/3,
         sql/3,
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



-spec raw_query(pid(),_,[_]) -> [[binary()] | {sqlite3_errmsg,binary()}].
raw_query(Db, FunName, Args) ->
    obj:call(Db, {query, FunName, Args}).

query(Db, FunName, Args) ->
    Rows = raw_query(Db, FunName, Args),

    %% If reply contains errors we throw them into the caller's
    %% process.  Normal results are [[binary()]].
    lists:foreach(
      fun({sqlite3_errmsg,_}=E) -> throw(E); (_) -> ok end, Rows),
    Rows.

%% Shortcut for raw SQL query
sql(Db, SQL,Bindings) when
      is_binary(SQL) and
      is_list(Bindings) ->
    query(Db, query, [{SQL,Bindings}]).


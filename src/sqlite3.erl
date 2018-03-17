%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to the erl_tools
%% library.
%%
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0

-module(sqlite3).
-export([%% PORT
         port_open/1, port_close/1, port_query/3,

         %% SERVER
         db/3, sql/2, sql_transaction/2,

         %% Internal, for reloads
         db_handle/2
        ]).

%% PORT

%% Port operations.
port_open(DbFile) ->
    Priv =
        case code:priv_dir(erl_tools) of
            {error,bad_name} -> 
                %% Allow files to be copied into other application's
                %% source tree.  In that case an environment variable
                %% needs to be set to recover the location of the
                %% binary.
                case os:getenv("ERL_TOOLS_APP") of
                    false -> throw(no_env_ERL_TOOLS_APP);
                    AppStr -> code:priv_dir(list_to_atom(AppStr))
                end;
            Dir -> Dir
        end,
    Cmd = tools:format("~s/sqlite3.elf ~s", [Priv,DbFile]),
    open_port({spawn, Cmd}, [use_stdio, {packet,4}, exit_status, binary]).
port_close(Port) ->
    Port ! {self(), {command, <<>>}},
    port_close_flush(Port).
port_close_flush(Port) ->
    receive
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, _}=E} -> exit(E);
        {Port, _} = _M -> 
            %% log:info("close_flush ~p~n",[_M]),
            port_close_flush(Port)
    end.
port_query(Port, {SQL, Bindings}=Query, Sink) when is_binary(SQL) and is_list(Bindings)->
    Bin = term_to_binary(Query),
    %% log:info("query: ~p~n",[Query]),
    Port ! {self(), {command, Bin}},
    port_sync(Port, Sink);
port_query(Port, {SQL, Bindings}, Sink) ->
    port_query(Port, {iolist_to_binary(SQL), Bindings}, Sink);
port_query(Port, SQL, Sink) ->
    port_query(Port, {SQL,[]}, Sink).


port_sync(Port, Sink) ->
    receive
        {Port, {data, <<>>}} ->
            Sink(eof);
        {Port, {data, Bin}} ->
            Term = binary_to_term(Bin),
            case Term of
                {error, Msg} ->
                    %% Push error through
                    Sink({data,{error,Msg}}),
                    Sink(eof);
                _ ->
                    Sink({data, Term}),
                    port_sync(Port, Sink)
            end
    end.
               

   



%% SERVER

%% Server process to interact with the sqlite port process.

%% Process wrapper around sqlite3.erl

db(Atom, DbFile, DbInit) ->
    ParentPid = self(),
    Pid = 
        serv:up(Atom,
                {handler,
                 fun() -> 
                         DB = self(),
                         unlink(ParentPid),
                         spawn(fun() -> DbInit(DB) end),
                         #{ db => sqlite3:port_open(DbFile()) }
                 end,
                 fun sqlite3:db_handle/2}),
    #{ pid => Pid, timeout => 5000 }.


%% Base routine performs multiple queries.
%% This allows serializing transactions.
db_handle({Pid, {queries, Queries}}, #{db := DB} = State) ->
    Results =
        [sink:gen_to_list(
           fun(Sink) -> sqlite3:port_query(DB, Query, Sink) end)
         || Query <- Queries],
    Pid ! {self(), obj_reply, Results},
    State;

%% db_handle({Pid, {query, Query}}, #{db := DB} = State) ->
%%     Pid ! {self(), obj_reply, 
%%            sink:gen_to_list(
%%              fun(Sink) -> sqlite3:port_query(DB, Query, Sink) end)},
%%     State;
db_handle(Msg,State) ->
    obj:handle(Msg,State).


-type binding() :: {text,binary()} | {blob,binary()}.
-type query() :: {binary(),[binding()]}.
%% -spec query(pid(),query()) -> [[binary()] | {sqlite3_errmsg,binary()}].
%%query(DbPid, Query) ->
%%    obj:call(DbPid, {query, Query}).
-spec queries(pid(),[query()],infinity | integer()) -> [[[binary()] | {sqlite3_errmsg,binary()}]].
queries(DbPid, Queries, Timeout) ->
    obj:call(DbPid, {queries, Queries}, Timeout).
    


%% Thunk allows for lazy DB connections.
%% -type timeout() :: infinity | integer().
-type db() :: fun(() -> #{ 'pid' => pid(), 'timeout' => timeout()}).

%% Lazy retrieval of DB connection + raise errors in caller's thread.
-spec sql(db(), [{binary(), [binding()]}]) -> [[binary()]].  %% FIXME: or exception
sql(DB, Queries) ->
    #{pid := Pid, timeout := Timeout} = DB(),

    Tables = queries(Pid, Queries, Timeout),
    %% If reply contains errors we throw them into the caller's
    %% process.  Normal results are [[binary()]].
    lists:foreach(
      fun({{SQL,Bindings},Rows}) ->
              lists:foreach(
                fun({sqlite3_errmsg,_}=E) -> throw({sqlite3,E,{DB,SQL,Bindings}});
                   (_) -> ok
                end,
                Rows)
      end,
      lists:zip(Queries, Tables)),
    Tables.
    
%% FIXME: single query. remove?
%%sql(DB,SQL,Bindings) ->
%%    [Rows] = sql(DB,[{SQL,Bindings}]),
%%    Rows.
    


%% FIXME: It is currently possible to race transactions, which is
%% likely where the "cannot start.." error comes from.  Redesign it
%% such that this is no longer possible.  I.e. create a second level
%% of sql access to serialize transactions.

%% Transactions always go through this functions.
%% Do not expose begin, end, rollback separately.
%% The idea is that is function will not leave the DB in an inconsistent state.

sql_transaction(DB, Queries) ->
    try 
        %% Can fail with: <<"cannot start a transaction within a transaction">>
        %% so place it inside of the try block.
        Transaction =
            lists:append(
              [[{<<"begin transaction">>, []}],
               Queries,
               [{<<"end transaction">>, []}]]),
        Rv = sql(DB, Transaction),
        {ok, Rv}
    catch
        C:E ->
            %% Rollback can fail apparently.. Log it because it's
            %% likely a bug elsewhere.
            Err0 = {C,E,erlang:get_stacktrace()},
            try
                [] = sql(DB, [{<<"rollback transaction">>, []}]),
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


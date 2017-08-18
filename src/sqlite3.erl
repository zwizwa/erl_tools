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
         db/3, sql/3, transaction/2,

         %% Internal, for reloads
         db_handle/2
        ]).

%% PORT

%% Port operations.
port_open(DbFile) ->
    Priv = code:priv_dir(erl_tools),
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
    serv:up(Atom,
            {handler,
             fun() -> 
                     DB = self(),
                     unlink(ParentPid),
                     spawn(fun() -> DbInit(DB) end),
                     #{ db => sqlite3:port_open(DbFile()) }
             end,
             fun sqlite3:db_handle/2}).

db_handle({Pid, {query, Query}}, #{db := DB} = State) ->
    Pid ! {self(), obj_reply, 
           sink:gen_to_list(
             fun(Sink) -> sqlite3:port_query(DB, Query, Sink) end)},
    State;
db_handle(Msg,State) ->
    obj:handle(Msg,State).


-type binding() :: {text,binary()} | {blob,binary()}.
-type query() :: {binary(),[binding()]}.
-spec query(pid(),query()) -> [[binary()] | {sqlite3_errmsg,binary()}].
query(DbPid, Query) ->
    obj:call(DbPid, {query, Query}).

%% Thunk allows for lazy DB connections.
-type db() :: fun(() -> pid()).
%% Shortcut for raw SQL query
-spec sql(db(), binary(), [binding()]) -> [[binary()]].  %% FIXME: or exception
sql(DB, SQL, Bindings) when
      is_binary(SQL) and
      is_list(Bindings) ->


    %% log:info("query: ~p~n",[{DB,SQL,Bindings}]),
    Rows = query(DB(), {SQL,Bindings}),

    %% If reply contains errors we throw them into the caller's
    %% process.  Normal results are [[binary()]].
    lists:foreach(
      fun({sqlite3_errmsg,_}=E) -> throw(E);
         (_) -> ok
      end,
      Rows),
    Rows.





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


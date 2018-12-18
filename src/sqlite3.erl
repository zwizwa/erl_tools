%% (c) 2018 Tom Schouten -- see LICENSE file


-module(sqlite3).
-export([%% PORT
         port_open/1, port_shutdown/1, port_query/3,

         %% SERVER
         db/3, sql/2, sql_transaction/2, close/1,

         %% Internal, for reloads
         db_handle/2
        ]).

-export_type([binding/0]).

-type binding() :: {text,binary()} | {blob,binary()}.
-type query() :: {binary(),[binding()]}.

%% Queries return values, or raise exceptions.
-type result_cell()  :: binary().
-type result_row()   :: [result_cell()].
-type result_table() :: [result_row()].

%% PORT

%% Port operations.
port_open(DbFile) ->

    %% FIXME: remove this hack
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
    Port = open_port({spawn, Cmd}, [use_stdio, {packet,4}, exit_status, binary]),
    %% log:info("port: ~p~n",[Port]),
    Port.
port_shutdown(Port) ->
    Port ! {self(), {command, <<>>}},
    port_shutdown_flush(Port).
port_shutdown_flush(Port) ->
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, _}=E} ->
            exit(E);
        {Port, _} = _M -> 
            %% log:info("close_flush ~p~n",[_M]),
            port_shutdown_flush(Port)
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
    #{ pid => Pid,
       timeout => {warn, 3000} }.


%% Base routine performs multiple queries.
%% This allows serializing transactions.

%% Note: this was transformed from a map into a "reversing foldl", to
%% allow interpersing code between transactions without having to
%% rewrite a lot of the logic.  Maybe clean this up later?.  To undo,
%% remove the 'check' clause, and translate it back into a map that
%% computes Results directly.
db_handle({Pid, {queries, Queries}}, #{db := DB} = State) ->
    Results =
        run_transaction(  %% early stop on error.
          fun(Query) ->
                  sink:gen_to_list(
                    fun(Sink) -> sqlite3:port_query(DB, Query, Sink) end)
          end,
          Queries),
    Pid ! {self(), obj_reply, Results},
    State;



db_handle({Port, {exit_status, _}=E}, #{db := Port}) ->
    log:info("ERROR: unexpected db port exit: ~p~n", [E]),
    exit(E);

%% The idea here is to have a synchronous call that ensures a best
%% effort has been made to close the db in an orderly manner such that
%% the function performing the obj:call can terminate the process.
db_handle({Pid, close}, #{db := Port} = State) ->
    Rv = port_shutdown(Port),
    obj:reply(Pid, Rv),
    State;

db_handle(Msg,State) ->
    obj:handle(Msg,State).


run_transaction(RunQuery, Queries) ->
    %% Always rollback first in case DB got into a bad state.
    RunQuery({<<"rollback transaction">>, []}),
    RunQuery({<<"begin transaction">>, []}),
    Rv = run_until_error(RunQuery, Queries, []),
    RunQuery({<<"end transaction">>, []}),
    Rv.

run_until_error(_, [], Acc) -> lists:reverse(Acc);
run_until_error(RunQuery, [Q|Qs], Acc) ->
    case Q of
        {check, Check} ->
            case Check(Acc) of
                ok ->
                    run_until_error(RunQuery, Qs, Acc);
                Msg ->
                    RunQuery({<<"rollback transaction">>, []}),
                    {sqlite3_abort, Msg}
            end;
         _ ->
            Rows = RunQuery(Q),
            case (try throw_if_error(Rows), ok
                  catch {_,_}=E -> E end) of
                ok -> run_until_error(RunQuery, Qs, [Rows|Acc]);
                Err -> Err
            end
    end.

%% Any row can be an error instead.
throw_if_error(Rows) ->
    lists:foreach(
      fun({_,_}=E) -> throw(E);
         (_) -> ok
      end,
      Rows).



%% -spec query(pid(),query()) -> [[binary()] | {sqlite3_errmsg,binary()}].
%%query(DbPid, Query) ->
%%    obj:call(DbPid, {query, Query}).
-spec queries(pid(),[query()],infinity | integer()) -> [[[binary()]]] | {sqlite3_errmsg,binary()} | {sqlite3_abort,any()}.
queries(DbPid, Queries, Timeout) ->
    obj:call(DbPid, {queries, Queries}, Timeout).


%% Thunk allows for lazy DB connections.
%% -type timeout() :: infinity | integer().
-type db_spec() :: #{ 'pid' => pid(), 'timeout' => {'warn',timeout()} }.
-type db() :: db_spec() | fun(() -> db_spec()).

%% Lazy retrieval of DB connection + raise errors in caller's thread.
-spec sql(db(), [{binary(), [binding()]}]) -> [result_table()].
sql(DB, Queries) when is_function(DB) ->
    sql(DB(), Queries);
sql(#{pid := Pid, timeout := Timeout}, Queries) ->
    case queries(Pid, Queries, Timeout) of
        {_,_}=E -> throw(E);
        Rv -> Rv
    end.

%% This ensures it is serialized and won't interrupt a transaction.
close(#{pid := Pid}) ->
    Rv = obj:call(Pid, close),
    exit(Pid, kill),
    Rv.
    
%% Alterative using ok/error for sqlite3_errmsg errors
sql_transaction(DB, Queries) ->
    try {ok, sql(DB, Queries)}
    catch
        {sqlite3_errmsg,_}=E -> {error, E};
        {sqlite3_abort,_}=E  -> {error, E}
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


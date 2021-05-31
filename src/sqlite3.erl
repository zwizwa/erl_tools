%% (c) 2018 Tom Schouten -- see LICENSE file


-module(sqlite3).
-export([%% PORT
         port_open/1, port_shutdown/1, port_query/3,

         %% SERVER
         db_registered/3, sql/2, sql_transaction/2, close/1,

         %% TOOLS
         column_type/1,

         %% Internal, for reloads
         db_handle/2
        ]).

-export_type([binding/0, query/0, result_tables/0, db_spec/0]).

-type binding() :: {text,binary()} | {blob,binary()}.
-type query() :: {binary(),[binding()]}.

%% Queries return values, or raise exceptions.
-type result_cell()   :: binary().
-type result_row()    :: [result_cell()].
-type result_table()  :: [result_row()].
-type result_tables() :: [result_table()].

-type query_error()   :: {sqlite3_errmsg,binary()} | {sqlite3_abort,any()}.
-type query_ok()      :: [result_table()].
-type query_timeout() :: obj:obj_timeout().

-type db_spec() :: #{ 'pid' => pid(), 'timeout' => query_timeout() }.


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
    %% log:info("DbFile = ~p~n", [DbFile]),
    Cmd = tools:format("~s/sqlite3.elf ~s", [Priv,DbFile]),
    %% log:info("Cmd = ~p~n", [Cmd]),
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

-spec db_registered(atom(), fun(() -> string()), fun((db_spec()) -> ok)) -> db_spec().
db_registered(Atom, DbFile, DbInit) ->
    ParentPid = self(),
    Pid = 
        serv:up(Atom,
                {handler,
                 fun() -> 
                         DB = self(),
                         unlink(ParentPid),
                         spawn(fun() -> DbInit(#{ pid => DB, timeout => {warn, 3000} }) end),
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
db_handle({Pid, {queries, Queries, QState}}, #{db := DB} = State) ->
    Results =
        run_transaction(  %% early stop on error.
          fun(Query) ->
                  sink:gen_to_list(
                    fun(Sink) -> sqlite3:port_query(DB, Query, Sink) end)
          end,
          Queries,
          QState),
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


run_transaction(RunQuery, Queries, QState) ->
    %% Always rollback first in case DB got into a bad state.
    RunQuery({<<"rollback transaction">>, []}),
    RunQuery({<<"begin transaction">>, []}),
    Rv = run_until_error(RunQuery, Queries, [], QState),
    RunQuery({<<"end transaction">>, []}),
    Rv.

run_until_error(_, [], Acc, _) ->
    lists:reverse(Acc);
run_until_error(RunQuery, [Q|Qs], Acc, QState) ->
    case Q of
        %% FIXME: Remove in favor of pfold_fn
        {check, Check} ->
            case Check(Acc) of
                ok ->
                    run_until_error(RunQuery, Qs, Acc, QState);
                Msg ->
                    RunQuery({<<"rollback transaction">>, []}),
                    {sqlite3_abort, Msg}
            end;
        %% FIXME: Not tested.  This was made for something that could
        %% be done using straight queries.
        {pfold_fn, Fn} ->
            case Fn(Acc, QState) of
                {next, QStateNext} ->
                    run_until_error(RunQuery, Qs, Acc, QStateNext);
                {stop, _}=Msg ->
                    RunQuery({<<"rollback transaction">>, []}),
                    {sqlite3_abort, Msg}
            end;
        _ ->
            Rows = RunQuery(Q),
            case (try throw_if_error(Rows), ok
                  catch {_,_}=E -> E end) of
                ok -> run_until_error(RunQuery, Qs, [Rows|Acc], QState);
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



-spec queries(pid(),
              [query()],
              query_timeout(),
              any()) ->
                     query_ok() | query_error().


queries(DbPid, Queries, Timeout, State) ->
    obj:call(DbPid, {queries, Queries, State}, Timeout).


%% Note that explicit 'text' and 'blob' tags are necessary.
-spec sql(db_spec(), [query()]) -> [result_table()].
sql(DB, Queries) ->
    sql(DB, Queries, no_state).
sql(#{pid := Pid, timeout := Timeout}, Queries, State) ->
    case queries(Pid, Queries, Timeout, State) of
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
    sql_transaction(DB, Queries, no_state).
sql_transaction(DB, Queries, State) ->
    try {ok, sql(DB, Queries, State)}
    catch
        {sqlite3_errmsg,_}=E -> {error, E};
        {sqlite3_abort,_}=E  -> {error, E}
    end.
        

%% Tools

%% Map type_base.erl encodings to SQLITE3 encodings.
%% FIXME: DATETIME
column_type(binary) -> <<"BINARY">>;
column_type(_) -> <<"TEXT">>.
    


    
%% Typed databases.  This requires storage of types, possibly in a
%% separate table or kvstore, and a wrapper around queries.  It
%% doesn't seem worth abstracting.  Do it at the application side.





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


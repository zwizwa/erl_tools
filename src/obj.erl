%% (c) 2018 Tom Schouten -- see LICENSE file

-module(obj).
-export([init/0, handle/2, call/2, call/3, reply/2,
         get/2, get/3, set/3, gets/2, values/1,
         update/3, update/2, find/2, dump/1, replace/2, merge/2,
         update/4, remove/2,
         kvstore/1, kvstore/2]).

%% Simple async object.

%% How to pick a representation of process state?  If it's not
%% obvious, and efficiency is not a concern, pick a Map.

%% Doing so it makes sense to also expose generic getter/setter
%% operations, It is useful for debugging.

%% Note that while convenient, this is somewhat of an anti-pattern as
%% it doesn't punish the construction of processes with complicated
%% state.  Keep it small!

-export_type([handle/1,obj_timeout/0]).


init() -> #{}.

%% Allow replies to be disabled to implement casts. for set, replace, update.

reply(no_reply, _) -> ok;
reply(Pid, Val) when is_pid(Pid) -> Pid ! {self(), obj_reply, Val}, ok;
reply({Tag,Pid}, Val) when is_pid(Pid) -> Pid ! {{Tag,self()}, obj_reply, Val}, ok.

-type rpc_message(M) :: dump | {replace, M} | {merge, M} | {remove, _} | {find, _} | {set, _, _} | {update, _, fun((_) -> _)}.
-type handle_message(M) :: shutdown | {pid(), rpc_message(M)}.

%% Curious... Can the spec use the handle/1 type?
-type handle(M) :: fun((handle_message(M),M) -> M).
-spec handle(handle_message(M), M) -> M.

handle({From, dump}, Map)           -> ok=reply(From, Map), Map;
handle({From, {replace, M}}, _)     -> ok=reply(From, ok), M;
handle({From, {merge, M1}}, M0)     -> ok=reply(From, ok), maps:merge(M0,M1);
handle({From, {remove, K}}, Map)    -> Map1 = maps:remove(K, Map), ok=reply(From, ok), Map1;
handle({From, {find, K}}, Map)      -> ok=reply(From, maps:find(K, Map)), Map;
handle({From, {set, K, V}}, Map)    -> reply(From, ok), maps:put(K, V, Map);
handle({From, {update, K, F}}, Map) -> V = F(maps:get(K, Map)), ok=reply(From, V), maps:put(K, V, Map);
handle({From, {update, F}}, Map)    -> {V,S} = F(Map), ok=reply(From, V), S;
handle(shutdown, _)                 -> exit(shutdown).

%% DON'T DO THIS: catch-all clauses limit dialyzer view
%% handle(Msg, State) ->
%%     %% tools:info("obj:handle: bad request ~p~n",[Msg]), State.
%%     throw({obj_handle, {Msg, State}}).


resolve(Pid) when is_pid(Pid) ->
    Pid;
resolve(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> exit({obj_call_undefined, Name});
        From when is_pid(From) -> From
    end;
resolve({Name,Node}=NN) when is_atom(Name) and is_atom(Node) ->
    case rpc:call(Node, erlang, whereis, [Name]) of
        undefined -> exit({obj_call_undefined, Name});
        {badrpc, nodedown} -> exit({obj_call_nodedown, NN});
        From when is_pid(From) -> From
    end;
resolve({tagged,Tag,Obj}) ->
    {tagged,Tag,resolve(Obj)}.


call(Dst, Req, Timeout, Warn) ->
    %% log:info("call: ~p~n",[{Dst, Req, Timeout, Warn}]),
    case resolve(Dst) of
        {tagged, Tag, Pid} when is_pid(Pid) ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {{Tag,self()}, Req},
            wait_reply({{Tag, Pid}, Pid, Req, Timeout, Warn, Ref});
        Pid when is_pid(Pid) ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Req},
            wait_reply({Pid, Pid, Req, Timeout, Warn, Ref})
    end.


wait_reply({Tagged, Pid, Req, Timeout, Warn, Ref}=Env) ->
    %% log:info("wait_reply: ~p~n", [Env]),
    receive
        {'DOWN',Ref,process,Pid,Reason} ->
            erlang:demonitor(Ref, [flush]),
            throw({obj_call_monitor, {Pid, Req, Reason}});
        {Tagged, obj_reply, Val} ->
            erlang:demonitor(Ref, [flush]),
            Val
    after
        Timeout ->
            Warn(),
            wait_reply(Env)
    end.







-type obj_timeout() :: {'warn', timeout()} | timeout().

call(Obj, Req, {warn, Timeout}) ->
    call(Obj, Req, Timeout,
         fun() -> tools:info("WARNING: obj:call(~p,~p) busy.~n", [Obj, Req])  end);
    
call(Obj, Req, Timeout)  ->
    call(Obj, Req, Timeout,
         fun() ->
                 %% Indirect, to avoid dialyzer warning.
                 apply(erlang,exit,[{timeout,Timeout,Req}])
         end).

call(Obj, Req) ->
    call(Obj, Req, {warn, 3000}).


%% %% DEBUG
%% call(Obj, Req) ->
%%     call(Obj, Req, 1000,
%%          fun() -> 
%%                  throw({timeout, Obj, Req})
%%          end).


dump   (From)           -> call(From, dump).
get    (From, Key)      -> case find(From, Key) of {ok, Val} -> Val; _ -> throw({obj_get_not_found, Key}) end.
get    (From, Key, Def) -> case find(From, Key) of {ok, Val} -> Val; _ -> Def end.
find   (From, Key)      -> call(From, {find, Key}).
set    (From, Key, Val) -> call(From, {set, Key, Val}).
update (From, Key, Fun) -> call(From, {update, Key, Fun}).
update (From, Fun)      -> call(From, {update, Fun}).
replace(From, D)        -> call(From, {replace, D}).
merge  (From, D)        -> call(From, {merge, D}).
remove (From, Key)      -> call(From, {remove, Key}).
     

gets   (From, [Key])    -> get(From,Key);
gets   (From, [K|Ks])   -> gets(get(From,K),Ks).

%% FIXME: the others should have timeouts as well.
update (From, Key, Fun, TO) -> call(From, {update, Key, Fun}, TO).

values(From) -> maps:values(dump(From)).
    

%% Abstract object as kvstore.
kvstore(Obj) ->
    {kvstore, fun(Method) -> kvstore_obj(Obj,Method) end}.
    
kvstore_obj(Obj,Method) ->        
    case Method of
        find     -> fun(Key)      -> find(Obj,Key) end;
        put      -> fun(Key,Val)  -> set(Obj,Key,Val), Val end;
        to_map   -> fun()         -> dump(Obj) end;
        to_list  -> fun()         -> maps:to_list(dump(Obj)) end;
        put_map  -> fun(Map)      -> merge(Obj,Map) end;
        put_list -> fun(List)     -> merge(Obj,maps:from_list(List)) end
    end.


%% Similar, but as an object field.
kvstore(Obj, FieldName) ->                             
    {kvstore, fun(Method) -> kvstore_obj(Obj,FieldName,Method) end}.

kvstore_obj(Obj,FieldName,Method) ->        
    Map = fun() -> obj:get(Obj, FieldName) end,
    Update = fun(Fun) -> obj:update(Obj,FieldName,Fun) end,

    case Method of
        find     -> fun(Key)      -> maps:find(Key, Map()) end;
        put      -> fun(Key,Val)  -> Update(fun(M) -> maps:put(Key,Val,M) end), Val end;
        to_map   -> fun()         -> Map() end;
        to_list  -> fun()         -> maps:to_list(Map()) end;
        put_map  -> fun(M1)       -> Update(fun(M0) -> maps:merge(M0,M1) end) end;
        put_list -> fun(List)     -> Update(fun(M0) -> maps:merge(M0,maps:from_list(List)) end) end
    end.

                       
                           
                           
             
            

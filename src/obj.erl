-module(obj).
-export([init/0, handle/2, call/2, call/3, reply/2,
         get/2, get/3, set/3, gets/2,
         update/3, find/2, dump/1, replace/2, merge/2,
         update/4,
         kvstore/1, kvstore/2]).

%% Simple async object.

%% To the extent possible under law, Tom Schouten has waived all
%% copyright and related or neighboring rights to obj.erl
%% Code:    http://zwizwa.be/git/erl_tools
%% License: http://creativecommons.org/publicdomain/zero/1.0


%% How to pick a representation of process state?  If it's not
%% obvious, and efficiency is not a concern, pick a Map.

%% Doing so it makes sense to also expose generic getter/setter
%% operations, It is useful for debugging.

%% Note that while convenient, this is somewhat of an anti-pattern as
%% it doesn't punish the construction of processes with complicated
%% state.  Keep it small!

-export_type([handle/1]).


init() -> #{}.

%% Allow replies to be disabled to implement casts. for set, replace, update.
reply(no_reply, _) -> ok;
reply(Pid, Val) -> Pid ! {self(), obj_reply, Val}, ok.

-type rpc_message(M) :: dump | {replace, M} | {merge, M} | {remove, _} | {find, _} | {set, _, _} | {update, _, fun((_) -> _)}.
-type handle_message(M) :: shutdown | {pid(), rpc_message(M)}.

%% Curious... Can the spec use the handle/1 type?
-type handle(M) :: fun((handle_message(M),M) -> M).
-spec handle(handle_message(M), M) -> M.

handle({Pid, dump}, Map)           -> ok=reply(Pid, Map), Map;
handle({Pid, {replace, M}}, _)     -> ok=reply(Pid, ok), M;
handle({Pid, {merge, M1}}, M0)     -> ok=reply(Pid, ok), maps:merge(M0,M1);
handle({Pid, {remove, K}}, Map)    -> Map1 = maps:remove(K, Map), ok=reply(Pid, ok), Map1;
handle({Pid, {find, K}}, Map)      -> ok=reply(Pid, maps:find(K, Map)), Map;
handle({Pid, {set, K, V}}, Map)    -> reply(Pid, ok), maps:put(K, V, Map);
handle({Pid, {update, K, F}}, Map) -> V = F(maps:get(K, Map)), ok=reply(Pid, V), maps:put(K, V, Map);
handle(shutdown, _)                -> exit(shutdown);

handle(Msg, State) ->
    %% tools:info("obj:handle: bad request ~p~n",[Bad]),
    throw({obj_handle, {Msg, State}}).

call(Pid, Req, Timeout) when is_pid(Pid) ->
    Pid ! {self(), Req},
    receive 
        {Pid, obj_reply, Val} -> Val
    after
        Timeout -> exit({timeout,Timeout,Req})
    end;
call(Name, Req, Timeout) ->
    case whereis(Name) of
        undefined -> exit({obj_call_undefined, Name});
        Pid -> call(Pid, Req, Timeout)
    end.
call(Obj, Req) ->
    call(Obj, Req, 3000).

dump   (Pid)           -> call(Pid, dump).
get    (Pid, Key)      -> {ok, Val} = find(Pid, Key), Val.
get    (Pid, Key, Def) -> case find(Pid, Key) of {ok, Val} -> Val; _ -> Def end.
find   (Pid, Key)      -> call(Pid, {find, Key}).
set    (Pid, Key, Val) -> call(Pid, {set, Key, Val}).
update (Pid, Key, Fun) -> call(Pid, {update, Key, Fun}).
replace(Pid, D)        -> call(Pid, {replace, D}).
merge  (Pid, D)        -> call(Pid, {merge, D}).
    

gets   (Pid, [Key])    -> get(Pid,Key);
gets   (Pid, [K|Ks])   -> gets(get(Pid,K),Ks).

%% FIXME: the others should have timeouts as well.
update (Pid, Key, Fun, TO) -> call(Pid, {update, Key, Fun}, TO).



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

                       
                           
                           
             
            

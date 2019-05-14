-module(kvstore).
-export([%% Simple key,value store interface
         get/3, get/2, get_or_make/3,
         find/2, put/3, clear/1, to_list/1, to_map/1,
         remove/2,
         to_map/2, to_list/2,
         put_list/2, put_list_cond/3,
         put_map/2, put_map_cond/3,
         update/4, update_val/4,
         update/3, update_val/3,
         keys/1,
         init/2,
         zero/0,
         with_default/2,
         read_only/1, read_only_from_map/1,
         get_type_bin_val/2,
         prefixed/2,
         combined/2,
         method/2,
         to_obj/1, to_obj_handle/2]).


%% tools:apply/2 supports erl_tools' canonical lambda-lifted form.
method({kvstore,F},Method) ->
    tools:apply(F, [Method]).

%% Simple abstract key-value store interface.
put           (K, Key, TypeVal)  -> (method(K,put))(Key, TypeVal).
find          (K, Key)           -> (method(K,find))(Key).
to_list       (K)                -> (method(K,to_list))().
to_map        (K)                -> (method(K,to_map))().
put_map       (K, Map)           -> (method(K,put_map))(Map).
put_map_cond  (K, Map, Check)    -> (method(K,put_map_cond))(Map, Check).
put_list      (K, Map)           -> (method(K,put_list))(Map).
put_list_cond (K, Lst, Check)    -> (method(K,put_list_cond))(Lst, Check).
keys          (K)                -> (method(K,keys))().
clear         (K)                -> (method(K,clear))().
remove        (K, Key)           -> (method(K,remove))(Key).

get_or_make(KVStore, Key, Make) ->
    case find(KVStore, Key) of
        {ok, RV} -> RV;
        _ -> Make()
    end.
-spec not_found(_) -> fun(() -> no_return()).
not_found(Key) ->
    fun() -> throw({error,{not_found,Key}}) end.
get(KVStore, Key) ->
    get_or_make(
      KVStore, Key, not_found(Key)).
      
get(KVStore, Key, Default) ->
    get_or_make(
      KVStore, Key,
      fun() -> Default end).

%% FIXME: make this into a separate method so it can be a transaction.
update(KVStore, Key, Fun, Make) ->
    put(KVStore, Key, Fun(get_or_make(KVStore, Key, Make))).
update(S,K,F) ->
    update(S,K,F,not_found(K)).
    

update_val(KVStore, Key, Fun, Make) ->
    {_, NewVal} =
        update(KVStore, 
               Key,
               fun({Type,Val}) -> {Type, Fun(Val)} end,
               Make),
    NewVal.  %% For chaining
update_val(S,K,F) ->
    update_val(S,K,F,not_found(K)).
             
init(KVStore, Defaults) when is_map(Defaults) ->  
    %% Do not write if nothing changed.  Useful in case writes are
    %% slow (e.g. sync after each transaction on slow SD card).
    %% FIXME: there must be a smarter way to do this.
    Old  = to_map(KVStore),
    New  = maps:merge(Defaults,Old),
    case New == Old of
        false -> put_map(KVStore, New); %% Returns {ok,_} | {error,_}
        true -> ok
    end,
    New.

    
zero() ->
    {kvstore,
     fun(find) -> fun(_) -> {error, zero} end;
        (to_list)-> [];
        (to_map) -> [];
        (keys) -> [];
        %% Can't write
        (_) -> throw(kvstore_zero)
     end}.
                   
%% Wrap a kvstore with default generator or map
%% FIXME: this isn't all that useful.  See also init_with_default
with_default({kvstore, _}=Parent, DefaultFind) ->     
    {kvstore,
     fun(find) ->
             fun(Key) ->
                     case find(Parent, Key) of
                         {ok, _} = Found -> Found;
                         _ -> DefaultFind(Key)
                     end
             end;
        (Method) ->
             %% Delegate all the rest.
             method(Parent,Method)
     end}.
             
             
     

%% Prefix all names.
%% FIXME: only partially implemented
prefixed(Prefix, {kvstore,_}=Kvstore) ->
    ToList = fun() -> [{{Prefix,K},TV} || {K,TV} <- (method(Kvstore,to_list))()] end,
    {kvstore,
     fun(to_map) ->
             fun() -> maps:from_list(ToList()) end;
        (to_list) ->
             ToList;
        (put_list) ->
             fun(Lst) ->
                     %% FIXME: assert prefix
                     (method(Kvstore,put_list))(
                       [{K,TV} || {{_Prefix, K}, TV} <- Lst])
             end;
        (find) ->
             fun(Key0={Pfx,Key}) -> 
                     case Pfx of
                         Prefix -> (method(Kvstore,find))(Key);
                         _ -> {error, {not_found, Key0}}
                     end

             end
     end}.
             
                            
                           
            
                          
             
%% Combine two stores.  This is useful to make different keys have
%% different back-ends.  E.g in GUI:
%% - viewmodel in RAM: fast, don't care if state gets lost
%% - rest is persistent store: persistent but slow, infrequent edits

combined({kvstore,F0},{kvstore,F1}) ->
    {kvstore, combined_impl(F0,F1)}.
            
combined_impl(F0,F1) ->
    fun(Method) ->
            %% Dispatch based on whether the first store has the key.
            M = fun(Key) ->
                        case (F0(find))(Key) of
                            {ok,_} -> F0(Method);
                            _      -> F1(Method)
                        end
                end,
            case Method of
                find -> fun(Key)     -> (M(Key))(Key)     end;
                put  -> fun(Key,Val) -> (M(Key))(Key,Val) end;
                %% FIXME: not all methods are supported yet
                _    -> throw({kvstore_combined,Method})
            end
    end.


%% Subset
to_list(KVStore, Keys) ->
    [{Key,kvstore:get(KVStore,Key)} || Key <- Keys].
to_map(KVStore, Keys) ->
    maps:from_list(to_list(KVStore, Keys)).
    

%% Get type, binary representation, and Erlang value
get_type_bin_val(KVStore, Key) ->
    {Type,Val}=TV=kvstore:get(KVStore, Key),
    {Type,type:encode(TV),Val}.


%% Limit to read-only access.
read_only({kvstore, F}) ->
    {kvstore, fun(find) -> F(find) end}.
                      
read_only_from_map(Map) ->
    {kvstore,
     fun(find) ->
             fun(Key) -> maps:find(Key, Map) end;
        (to_map)  ->
             fun() -> Map end;
        (to_list)  ->
             fun() -> maps:to_list(Map) end
     end}.


%% Map obj api to kvstore, saving all values as pterm.
to_obj(KVStore) ->
    serv:start(
      {handler,
       fun() -> KVStore end,
       fun kvstore:to_obj_handle/2}).

tag(M) when is_map(M) -> maps:map(fun(_,V) -> {pterm, V} end, M).

untag(M) when is_map(M) -> maps:map(fun(_,{_, V}) -> V end, M);
untag({ok,{_,V}}) -> {ok, V};
untag({_,V}) -> V;
untag(error) -> error.

to_obj_handle({Pid, dump}, S)           -> ok=obj:reply(Pid, untag(to_map(S))), S;
to_obj_handle({Pid, {remove, K}}, S)    -> remove(S, K), ok=obj:reply(Pid, ok), S;
to_obj_handle({Pid, {find, K}}, S)      -> ok=obj:reply(Pid, untag(find(S, K))), S;
to_obj_handle({Pid, {set, K, V}}, S)    -> put(S, K, {pterm, V}), obj:reply(Pid, ok), S;
to_obj_handle({Pid, {replace, M}}, S)   -> clear(S), put_map(S, tag(M)), obj:reply(Pid, ok), S;
to_obj_handle({Pid, {merge, M}}, S)     -> put_map(S, tag(M)), obj:reply(Pid, ok), S;
to_obj_handle(shutdown, _)              -> exit(shutdown).
                                           
%% TODO:
%% to_obj_handle({Pid, {update, K, F}}, S) -> V = F(maps:get(K, Map)), ok=obj:reply(Pid, V), maps:put(K, V, Map);
%% to_obj_handle({Pid, {update, F}}, S)    -> {V,S} = F(Map), ok=obj:reply(Pid, V), S;
               

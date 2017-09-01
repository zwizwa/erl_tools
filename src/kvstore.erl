-module(kvstore).
-export([%% Simple key,value store interface
         get/3, get/2, get_or_make/3,
         find/2, put/3, clear/1, to_list/1, to_map/1,
         remove/2,
         to_map/2, to_list/2,
         put_list/2, put_map/2,
         update/4, update_val/4,
         update/3, update_val/3,
         keys/1, init/2, zero/0, with_default/2,
         read_only/1, read_only_from_map/1,
         get_type_bin_val/2,
         combined/2]).

%% FIXME: rename these to the interface of obj.erl

%% Simple abstract key-value store interface.
put        ({kvstore, F}, Key, TypeVal)  -> (F(put))(Key, TypeVal).
find       ({kvstore, F}, Key)           -> (F(find))(Key).
to_list    ({kvstore, F})                -> (F(to_list))().
to_map     ({kvstore, F})                -> (F(to_map))().
put_map    ({kvstore, F}, Map)           -> (F(put_map))(Map).
put_list   ({kvstore, F}, Map)           -> (F(put_list))(Map).
keys       ({kvstore, F})                -> (F(keys))().
clear      ({kvstore, F})                -> (F(clear))().
remove     ({kvstore, F}, Key)           -> (F(remove))(Key).

get_or_make(KVStore, Key, Make) ->
    case find(KVStore, Key) of
        {ok, RV} -> RV;
        _ -> Make()
    end.
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
             
init(KVStore, Init) when is_map(Init) ->  
    %% Do not write if nothing changed.  Useful in case writes are
    %% slow (e.g. sync after each transaction on slow SD card).
    %% FIXME: there must be a smarter way to do this.
    Old  = to_map(KVStore),
    New  = maps:merge(Init,Old),
    case New == Old of
        false -> put_map(KVStore, New); %% Returns {ok,_} | {error,_}
        true -> ok
    end.
    
zero() ->
    {kvstore,
     fun(find) -> fun(_) -> {error, zero} end;
        (to_list)-> [];
        (to_map) -> [];
        (keys) -> [];
        %% Can't write
        (_) -> throw(kvstore_zero)
     end}.
                   
%% Wrap a kvstore with default generator
with_default({kvstore, F}=Parent, DefaultFind) ->     
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
             F(Method)
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

                                   
                      

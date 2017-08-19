-module(kvstore).
-export([%% Simple key,value store interface
         get/3, get/2, find/2, put/3, to_list/1, to_map/1,
         put_list/2, put_map/2,
         keys/1, init/2, zero/0, with_default/2]).

%% FIXME: rename these to the interface of obj.erl

%% Simple abstract key-value store interface.
put        ({kvstore, F}, Key, TypeVal)  -> (F(put))(Key, TypeVal).
find       ({kvstore, F}, Key)           -> (F(find))(Key).
to_list    ({kvstore, F})                -> (F(to_list))().
to_map     ({kvstore, F})                -> (F(to_map))().
put_map    ({kvstore, F}, Map)           -> (F(put_map))(Map).
put_list   ({kvstore, F}, Map)           -> (F(put_list))(Map).
keys       ({kvstore, F})                -> (F(keys))().

get(KVStore, Key) ->
    case find(KVStore, Key) of
        {ok, RV} -> RV;
        _ -> throw({error,{not_found,Key}})
    end.
get(KVStore, Key, Default) ->
    case find(KVStore, Key) of
        {ok, RV} -> RV;
        _ -> Default
    end.
             
init(KVStore, Init) when is_map(Init) ->  
    %% Do not write if nothing changed.  Useful in case writes are
    %% slow (e.g. sync after each transaction on slow SD card).
    %% FIXME: there must be a smarter way to do this.
    Old  = to_map(KVStore),
    New  = maps:merge(Init,Old),
    case New == Old of
        false -> put_map(KVStore, New); %% Returns {ok,_} | {error,_}
        true -> {ok, already_initialized}
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
             
             
     
                          
             

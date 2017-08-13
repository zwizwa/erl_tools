-module(kvstore).
-export([%% Simple key,value store interface
         find/2, read/2, write/2, to_list/1, to_map/1,
         write_map/2, keys/1]).

%% Simple abstract key-value store interface.
write     ({kvstore, F}, KeyTypeVal)  -> (F(write))(KeyTypeVal).
find      ({kvstore, F}, Key)         -> (F(find))(Key).
to_list   ({kvstore, F})              -> (F(to_list))().
to_map    ({kvstore, F})              -> (F(to_map))().
write_map ({kvstore, F}, Map)         -> (F(write_map))(Map).
keys      ({kvstore, F})              -> (F(keys))().

read(KVS, Key) ->
    {ok, RV} = find(KVS, Key),
    RV.

            
    
  

-module(kv).
-export([%% Simple key,value store interface
         find/2, read/2, write/2, load/1, save/2, keys/1]).

%% Simple abstract key-value store interface.
write({kvstore, F}, KeyTypeVal)  -> (F(write))(KeyTypeVal).
find ({kvstore, F}, Key)         -> (F(find))(Key).
load ({kvstore, F})              -> (F(load))().
save ({kvstore, F}, Dict)        -> (F(save))(Dict).

keys(KVS) ->
    maps:keys(load(KVS)). %% FIXME: push inside
read(KVS, Key) ->
    {ok, RV} = find(KVS, Key),
    RV.
  

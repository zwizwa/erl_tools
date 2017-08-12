-module(kv).
-export([%% Simple key,value store interface
         read/2, write/2, load/1, save/2]).

%% Simple abstract key-value store interface.
write({kvstore, F}, KeyTypeVal)  -> (F(write))(KeyTypeVal).
read ({kvstore, F}, Key)         -> (F(read))(Key).
load ({kvstore, F})              -> (F(load))().
save ({kvstore, F}, Dict)        -> (F(save))(Dict).


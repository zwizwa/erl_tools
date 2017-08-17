-module(kvstore).
-export([%% Simple key,value store interface
         find/2, read/2, write/2, to_list/1, to_map/1,
         write_list/2, write_map/2,
         keys/1, init/2, zero/0, with_default/2]).

%% Simple abstract key-value store interface.
write      ({kvstore, F}, KeyTypeVal)  -> (F(write))(KeyTypeVal).
find       ({kvstore, F}, Key)         -> (F(find))(Key).
to_list    ({kvstore, F})              -> (F(to_list))().
to_map     ({kvstore, F})              -> (F(to_map))().
write_map  ({kvstore, F}, Map)         -> (F(write_map))(Map).
write_list ({kvstore, F}, Map)         -> (F(write_list))(Map).
keys       ({kvstore, F})              -> (F(keys))().

read(KVStore, Key) ->
    {ok, RV} = find(KVStore, Key),
    RV.

            
    
init(KVStore, Init) ->  
    %% Do not write if nothing changed.  Useful in case writes are
    %% slow (e.g. sync after each transaction on slow SD card).
    %% FIXME: there must be a smarter way to do this.
    Old  = to_map(KVStore),
    New  = maps:merge(Init,Old),
    case New == Old of
        false -> write_map(KVStore, New); %% Returns {ok,_} | {error,_}
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
             
             
     
                          
             

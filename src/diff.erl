-module(diff).
-export([diff/2, diff/3, as_map/1]).

%% Encode data structure differences as edit commands.  To simplify,
%% use only maps.  Encode all other data in a separate, unchanging
%% environment.  

%% To keep the protocol simple, only maps are diffed.  Diffable lists
%% should be represented as maps.

-type key()  :: atom().
-type tree() :: leaf()  | #{ key() => tree() }.
-type leaf() :: _.  
-type path() :: [key()].
-type edit() :: {add, path(), leaf()} |
                {del, path()} |
                {set, path(), leaf()}.


-spec diff(tree(),tree()) -> [edit()].
diff(A,B) ->
    sink:gen_to_list(
      fun(Sink) -> diff(Sink, A, B) end).
diff(Sink,A,B) ->
    diff([],fun(D) -> Sink({data,D}) end, A, B),
    Sink(eof).
diff(ParentPath,SaveEdit,OldMap,NewMap) ->
    ForKeys =
        fun(EditType, Keys) ->
            lists:foreach(
              fun(K) ->
                  Path = ParentPath ++ [K],
                  case EditType of
                      del ->
                          SaveEdit({del, Path});
                      ins ->
                          NewVal = maps:get(K,NewMap),
                          SaveEdit({ins, Path, NewVal});
                      set ->
                          OldVal = maps:get(K,OldMap),
                          NewVal = maps:get(K,NewMap),
                          case NewVal of
                              OldVal -> nop;
                              _ when not(is_map(NewVal)) ->
                                  SaveEdit({set, Path, NewVal});
                              #{ diff := leaf } ->
                                  SaveEdit({set, Path, 
                                            maps:remove(diff, NewVal)});
                              _ ->
                                  %% Subtree
                                  diff(Path,SaveEdit,OldVal,NewVal)
                          end
                  end
              end,
              Keys)
        end,
    Old = maps:keys(OldMap),
    New = maps:keys(NewMap),
    Del = lists:subtract(Old,New),
    Ins = lists:subtract(New,Old),
    Common = lists:subtract(New,Ins),
    ForKeys(del, Del),
    ForKeys(ins, Ins),
    ForKeys(set, Common),
    ok.


%% E.g. for JSON encoding.    
as_map({del, P})    -> #{op => del, path => P};
as_map({ins, P, V}) -> #{op => ins, path => P, val => V};
as_map({set, P, V}) -> #{op => set, path => P, val => V}.


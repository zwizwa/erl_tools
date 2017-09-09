-module(diff).
-export([diff/2, diff/3, as_map/1]).

%% Encode data structure differences as edit commands.  To keep the
%% protocol simple, only maps are diffed as this gives a simple "paths
%% as key lists" output.

-type key()  :: atom().
-type tree() :: leaf()  | #{ key() => tree() }.
-type leaf() :: _.  
-type path() :: [key()].
-type edit() :: {insert, path(), leaf()} |
                {delete, path()} |
                {update, path(), leaf(), leaf()}.


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
                      delete ->
                          SaveEdit({delete, Path});
                      insert ->
                          NewVal = maps:get(K,NewMap),
                          SaveEdit({insert, Path, NewVal});
                      update ->
                          OldVal = maps:get(K,OldMap),
                          NewVal = maps:get(K,NewMap),
                          case NewVal of
                              OldVal -> nop;
                              _ when not(is_map(NewVal)) ->
                                  SaveEdit({update, Path, OldVal, NewVal});
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
    ForKeys(delete, Del),
    ForKeys(insert, Ins),
    ForKeys(update, Common),
    ok.


%% E.g. for JSON encoding.    
as_map({del, P})    -> #{op => del, path => P};
as_map({ins, P, V}) -> #{op => ins, path => P, val => V};
as_map({set, P, V}) -> #{op => set, path => P, val => V}.




%% The central idea here is the correspondence between a hierarchical
%% structure (nested dictionaries) and a path list to value map.

%% These are isomorphic representations, but they both have their
%% advantages for code and data structuring.  Hierarchical structure
%% maps better to functional data processing code, while flat
%% structuring maps better to data storage and communication,
%% e.g. allowing for the easy difference encoding above.


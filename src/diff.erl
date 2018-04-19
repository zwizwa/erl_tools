-module(diff).
-export([diff/2, diff/3, as_map/1, split/1, diff_split/2]).

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


%% The default diff operation produces {insert,path(),tree()} in case
%% the entire subtree is new.  The split/1 function will refine that
%% into 'variable' operations that create the containers, and a
%% 'value' operation that will specify the value.  This allows
%% struture and value to be completely separated.

split(Op) ->
    case Op of
        {insert,Path,Tree} when is_map(Tree) ->
            [{env,Path}|
             lists:append(
               [split({insert,Path++[K],V}) || {K,V} <- maps:to_list(Tree)])];
        {insert,Path,Node} ->
            [{var,Path},    %% Create hole
             {bind,Path,Node}];  %% Fill hole
        {update,Path,_,Value} ->
            {bind,Path,Value};
        _ ->
            [Op]
    end.


diff_split(A,B) ->
    lists:append(
      lists:map(fun split/1, diff(A,B))).
                


%% E.g. for JSON encoding.    
as_map({delete, P})    -> #{op => delete, path => P};
as_map({insert, P, V}) -> #{op => insert, path => P, val => V};
as_map({update, P, V}) -> #{op => update, path => P, val => V}.




%% The central idea here is the correspondence between a hierarchical
%% structure (nested dictionaries) and a path list to value map.

%% These are isomorphic representations, but they both have their
%% advantages for code and data structuring.  Hierarchical structure
%% maps better to functional data processing code, while flat
%% structuring maps better to data storage and communication,
%% e.g. allowing for the easy difference encoding above.


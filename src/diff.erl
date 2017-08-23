-module(diff).
-export([diff/2, diff/3]).

%% Encode data structure differences as edit commands.  To simplify,
%% use only maps.  Encode all other data in a separate, unchanging
%% environment.  

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
diff(ParentPath,Edit,Old,New) ->
    KOld = maps:keys(Old),
    KNew = maps:keys(New),
    KDel = lists:subtract(KOld,KNew),
    KIns = lists:subtract(KNew,KOld),
    KCommon = lists:subtract(KNew,KIns),
    
    ForKeys =
        fun(EditType,
            Keys) ->
                lists:foreach(
                  fun(K) ->
                          Path = ParentPath ++ [K],
                          case EditType of
                              del -> Edit({del,Path});
                              ins -> Edit({ins,Path,maps:get(K,New)});
                              set ->
                                  VOld = maps:get(K,Old),
                                  VNew = maps:get(K,New),
                                  case VNew of
                                      VOld -> ok;
                                      VNew when not(is_map(VNew)) ->
                                          Edit({set, Path, VNew});
                                      _ ->
                                          %% Recursion point.
                                          diff(Path,Edit,VOld,VNew)
                                  end
                          end
                  end,
                  Keys)
        end,
    ForKeys(del, KDel),
    ForKeys(ins, KIns),
    ForKeys(set, KCommon),
    ok.


    

%% Incremental UI rendering ala react can be implemented on top of the
%% memoizing evaluator by introducing a side effect and modifying the
%% propagation of change.

%% Basically, an incremental evaluator is the same as a
%% non-incremental evaluator except for one change: "smart containers"
%% can either send incremantal updates via a side-channel, or
%% re-render and let their parent container make a similar decision.

%% It is surprising to me that this is simple to express with a
%% pruning memoizing evaluator, but was completely non-trivial to
%% discover.  In fact the problem seemed "upside down": we DON'T just
%% propagate if there is change!

-module(react).
-export([test/1]).

do_render(_Nodes) ->
    ok.

smart_list(OutNode, Nodes, SideChannel) ->
    fun (Eval) ->
            Map = redo:changed(Eval, Nodes),
            ChangeList = maps:values(Map),
            false = lists:member(error, ChangeList),
            case list:member(false, ChangeList) of
                false ->
                    %% If they all changed, just re-render.  That also
                    %% handles the initial rendering problem!
                    redo:put_val(Eval, OutNode, do_render(Nodes)),
                    true;
                true ->
                    %% If there is one that did not change, we know
                    %% that there was no render before.
                    lists:foreach(
                      fun({_,false}) ->
                              %% Ignore the ones that did not change.
                              ok;
                         ({Key,true}) ->
                              %% Update the others in place.
                              Val = redo:need_val(Eval, Key),
                              SideChannel(Key,Val)
                      end,
                      maps:to_list(ChangeList)),
                    %% And signal upstream that no more changes are
                    %% necessary.
                    false
            end
    end.

test({smart_list,Ns}) ->
    smart_list(out_node, Ns, fun(_,_) -> ok end);
test(Spec) ->
    throw({?MODULE,bad_test,Spec}).


-module(ws_layout).
-export([button/2, button/3, clickable/3,
         tagged_table/1,
         table/2, input/2]).

%% Layout functions for ws_widget style web widgets.

%% First argument is Env that is passed to a widget's layout function
%% as {layout, Env}.


%% FIXME: Why is this here?  Replaced with button/3 below
%% button(#{ send := Send }, ID, Text) ->
%%     ID_enc = ws:encode_id(ID),
%%     {button,
%%      [{onclick, Send},
%%       {'data-decoder', button},  %% Type conversion for js->erl messages.
%%       {'data-mixin', cell},      %% DOM behavior for erl->js messages
%%       {id, ID_enc}],             %% erl<->js messages
%%      [[Text]]}.

%% For use in widget startup.
button(Env, Tag) ->
    Text = tools:format_binary("~p",[Tag]),
    button(Env, Tag, Text).

%% FIXME: This changed.  Don't prefix the path here.  Widgets will
%% have to use absolute IDs, otherwise it gets too confusing because
%% absolute IDs are necessary in other places.

%% button(#{path := Path}=Env, Tag, Text) ->
%%     {button,
%%      [{onclick, exml:get_send(Env)},
%%       {'data-decoder', button},  %% Type conversion for js->erl messages.
%%       {'data-mixin', cell},      %% DOM behavior for erl->js messages
%%       id({Path,Tag})],           %% erl<->js messages
%%      [[Text]]}.

%% FIXME: write button in terms of clickable
button(Env, Tag, Text) ->
    {button,
     [{onclick, exml:get_send(Env)},
      {'data-decoder', button},  %% Type conversion for js->erl messages.
      {'data-mixin', cell},      %% DOM behavior for erl->js messages
      id(Tag)],                  %% erl<->js messages
     [[Text]]}.

clickable(Env, Tag, {T, As, Es}) ->
    {T,
     exml:attr_merge(
       [{onclick, exml:get_send(Env)},
        {'data-decoder', button},  %% Type conversion for js->erl messages.
        {'data-mixin', cell},      %% DOM behavior for erl->js messages
        id(Tag)],                  %% erl<->js messages
       As), Es}.
    

%% Table, using kvstore for initialization.
table(Env, TableList) ->
    {table, [],
     lists:map(
       fun({Key, Name}) when is_binary(Name) ->
               {tr,[],
                [{td,[],[[Name]]},
                 {td,[],[input(Env, Key)]}]}
       end,
       TableList)}.
%% FIXME: this indirection to the "input" renderer should be removed.
%% It is here to gradually refactor application code.
input(Env = #{kvstore := KVStore, input := Input}, Key) ->
    %% Initialize from KVStore, set callback to 'handle' method.
    Input(Env, {Key, kvstore:get(KVStore, Key), handle});

%% Newer code can leave out "input" and use the default renderer that
%% supports the newer Env approach.
input(Env = #{kvstore := KVStore}, Key) ->
    input_set_id(Key, exml:input(Env, {Key, kvstore:get(KVStore, Key)})).

%% All widget elements have a unique id and have 'cell' behavior so
%% they can be updated.  The exml:input library only uses 'name'.
input_set_id(ID,{Tag,As,Es}) ->
    {Tag,
     exml:attr_merge(
       As,
       [{'id',ws:encode_id(ID)},
        {'data-mixin','input'}]),
     Es}.

%% Throughout the application, id attributes are assumed to be
%% printable terms.  See ws.erl and type_base.erl pterm
id(PTerm) ->
    {id, ws:encode_id(PTerm)}.



%% Using the widget Env convention.
%% Move this to ws_layout.erl
tagged_table(#{ get_table := GetTable,
                send      := Send,
                event     := {T,V},
                path      := Path }) ->
    %% Take first column to be ID
    {table,[],
     [{tr,[{onclick, Send},
           {'data-decoder', T},
           {'data-value', type_base:encode({T,V})},
           {id, ws:encode_id({Path, hd(Row)})}],
       [{td,[],[[Col]]}
        || Col <- Row]}
      || Row <- GetTable()]}.

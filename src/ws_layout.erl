-module(ws_layout).
-export([button/2, button/3,
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

button(#{path := Path, send := Send}, Tag, Text) ->
    {button,
     [{onclick, Send},
      {'data-decoder', button},  %% Type conversion for js->erl messages.
      {'data-mixin', cell},      %% DOM behavior for erl->js messages
      id({Path,Tag})],           %% erl<->js messages
     [[Text]]}.


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

%% Newer code can leave out "input"
input(_Env = #{kvstore := _KVStore}, _Key) ->
    throw('FIXME_implement_input').



%% Throughout the application, id attributes are assumed to be
%% printable terms.  See ws.erl and type_base.erl pterm
id(PTerm) ->
    {id, ws:encode_id(PTerm)}.



%% Using the widget Env convention.
%% Move this to ws_layout.erl
tagged_table(#{ get_table := GetTable,
                send      := Send,
                path      := Path }) ->
    %% Take first column to be ID
    {table,[],
     [{tr,[{onclick, Send},
           {'data-decoder', atom},
           {'data-value', "click"},
           {id, ws:encode_id({Path, hd(Row)})}],
       [{td,[],[[Col]]}
        || Col <- Row]}
      || Row <- GetTable()]}.

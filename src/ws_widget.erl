-module(ws_widget).
-export([
         %% Convert collection of widgets to initial page template
         layout/2,
         supervisor/2,
         %% Layout renderers
         button/2, table/2, input/2,
         %% Widgets
         kvstore_edit/1,
         repl/1,
         example/1]).

%% Widgets are constructed out of HTML layout, client side behavior
%% implemented in JavaScript (mixins), and a server side process.
%%
%% Widgets do not know where they are located in a page.  They are
%% associated with a Path.  This is a tag that is used for creating
%% HTML id attributes, and, and dispatching data back to Erlang (by
%% using the tag to look up a child inside a supervisor).
%%
%% Widgets follow the 2-phase startup process used in single page
%% applications:
%%
%% - A 'layout' phase creates an initial EXML data structure
%%   representing the visual representation of the widget inside a
%%   page request.
%%
%% - A 'serv_spec' phase creates a serv.erl specification structure
%%   describing how to start the widget's controller process.  This is
%%   then transformed into a standard OTP data structure, to be used
%%   by the OTP supervisor that manages all the widgets in a single
%%   page application.
%%
%% Both of these phases are parameterized by an environment.
%%


%% The two phases in the init/1 method of a single page application
%% would call into these two functions.

%% Create initial layout.
layout(Widgets, Env) ->
    maps:map(
      fun(Name, Init) ->
              Env1 = maps:put(path, Name, Env),
              Init({layout, Env1})
      end,
      Widgets).

%% Convert widget's serv_spec into an OTP supervisor init/1 response.
%% All the widgets run under a single supervisor.
supervisor(Widgets, Env = #{ module := Module, ws := _Ws }) ->
    %% log:info("ws_widget:supervisor: ~p~n", [{Widgets,Env}]),
    Rv =
        serv:supervisor_init(
          [Module],
          maps:to_list(
            maps:map(
              fun(Name, Init) ->
                      Env1 = maps:put(path, Name, Env),
                      Init({serv_spec, Env1})
              end,
              Widgets))),
    %% log:info("ws_widget:supervisor: ~p~n", [Rv]),
    Rv.
        



%% Example widget.

%% The example widget is bundled in a single function, to illustarte
%% that this is possible.  For more complex widgets it might be easier
%% to factor it out.

example(Cmd = {_, #{path := Path}}) ->
    %% IDs _must_ be prefixed with Path.  This determines routing
    %% between the JavaScript code and the Erlang process identified
    %% by the same name inside the supervisor structure.
    ID = {Path, button123},
    case Cmd of
        {layout, Env} ->
            {'div',[],
             [{pre,[],[[<<"Example Widget">>]]},
              button(Env, ID, <<"Example123">>)]};
        {serv_spec, #{ws := Ws} = Env} ->
            {handler,
             fun() -> maps:put(count, 1, Env) end,
             fun(Msg, State = #{ count := Count }) ->
                     log:info("ws_widget:example: ~p~n", [Msg]),
                     Text = tools:format_binary("count=~p", [Count]),
                     ws:call(Ws, {Path, button123}, set, Text),
                     maps:put(count, Count+1, State)
             end}
    end.



%% Layout.  Move this somewhere else.
button(#{ send := Send }, ID, Text) ->
    ID_enc = ws:encode_id(ID),
    {button,
     [{onclick, Send},
      {'data-decoder', button},  %% Type conversion for js->erl messages.
      {'data-mixin', cell},      %% DOM behavior for erl->js messages
      {id, ID_enc}],             %% erl<->js messages
     [[Text]]}.


%% FIXME: just a doodle
%% Program interaction
repl(Cmd = {_, #{path := Path}}) ->
    %% IDs _must_ be prefixed with Path.  This determines routing
    %% between the JavaScript code and the Erlang process identified
    %% by the same name inside the supervisor structure.
    ID = {Path, repl},
    case Cmd of
        {layout, #{ send := Send } = _Env} ->
            {'div',[],
             [{input,
               [{id,ws:encode_id(ID)},
                {name,ws:encode_id(ID)},
                {onchange, Send},
                {'data-decoder',binary},
                {'data-mixin',input}],
               []}]};
        {serv_spec, #{ws := Ws} = Env} ->
            {handler,
             fun() ->
                     ws:call(Ws, ID, set, <<"">>),
                     Env 
             end,
             fun(Msg, State) ->
                     log:info("ws_widget:repl: ~p~n", [Msg]),
                     %%ws:call(Ws, ID, set_attribute, [value,<<"">>]),
                     ws:call(Ws, ID, set, <<"">>),
                     State
             end}
    end.
   



%% Layout templates for widgets.

%% First argument is Env that is passed to a widget's layout function
%% as {layout, Env}.

%% For use in widget startup.
button(#{path := Path, send := Send}, Tag) ->
    Text = tools:format_binary("~p",[Tag]),
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
%% FIXME: this indirection to the "input" renderer should be rempoved.
%% It is here to gradually refactor application code.
input(Env = #{kvstore := KVStore, input := Input}, Key) ->
    %% Initialize from KVStore, set callback to 'handle' method.
    Input(Env, {Key, kvstore:get(KVStore, Key), handle});

%% Newer code can leave out "input"
input(_Env = #{kvstore := _KVStore}, _Key) ->
    throw('FIXME_implement_input').
    

         

%% Edit a KVStore as a table
%% Note: all the keys in the store need to be prefixed: {Path, _}
kvstore_edit({layout, 
              #{path     := Path,
                defaults := Defaults,
                labels   := Labels,
                kvstore  := KVStore }=Env}) ->
    _ = kvstore:init(KVStore, Defaults),
    Body = 
        {'div',[{style, <<"width: 100%; display: table;">>}],
         [{'div',[{id, ws:encode_id({Path,error})},
                  {'data-mixin',cell}],
           [] %% [[<<"<error goes here>">>]]
          },
          {'div', [], [table(Env, Labels)]}]},
    %% exml:validate(Body),  %% Assertion
    Body;
kvstore_edit({serv_spec, 
              #{ kvstore := KVStore,
                 ws := Ws,
                 path := Path }=Env}=_Msg) ->
    {handler,
     fun() ->
             log:set_info_name({?MODULE,Path}),
             %% Browser reload does not update the values of number
             %% boxes, so be sure to set them to the internal values
             %% here.
             lists:foreach(
               fun({Key,{_Type, _Val}=TV}=_KTV) ->
                       %% log:info("init: ~p~n",[_KTV]),
                       ws:call(Ws, Key, set, type:encode(TV))
               end,
               kvstore:to_list(KVStore)),
             Env
     end,
     fun kvstore_edit_handle/2}.
kvstore_edit_handle([{{Path, _Control}, {_Type, _Val}}] = KTVList,
                    State = #{ path    := Path, 
                               kvstore := KVStore,
                               ws      := Ws }) ->
    case maps:find(check_constraints, State) of
        {ok, CheckConstraints} ->
            %% Update KVStore atomically with constraint check.  Values will
            %% be retreived in start_recording/1
            kvstore:put_list_cond(
              KVStore, KTVList,
              fun(Lst) -> CheckConstraints(State, Lst) end);
        _ ->
            kvstore:put_list(
              KVStore, KTVList)
    end,
    %% Reset error message
    ws:call(Ws, {Path, error}, set, <<"">>),
    log:info("update: ~p~n",[KTVList]),
    State;
kvstore_edit_handle({bad_value, {Path, Control}, Error}=E,
                    State = #{ ws := Ws }) ->
    %% Router encountered a problem during value decoding.
    ErrorMsg =
        case Error of
            {_Type, _Entry, ErrorText} ->
                tools:format_binary("~p: ~s",[Control, ErrorText]);
            _ ->
                <<"Error">>
        end,
    ws:call(Ws, {Path, error}, set, ErrorMsg), 
    log:info("error: ~p~n", [E]),
    State.



%% Throughout the application, id attributes are assumed to be
%% printable terms.  See ws.erl and type_base.erl pterm
id(PTerm) ->
    {id, ws:encode_id(PTerm)}.

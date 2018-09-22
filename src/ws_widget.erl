-module(ws_widget).
-export([
         %% Convert collection of widgets to initial page template
         layout/2,
         supervisor/2,
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
    serv:supervisor_init(
      [Module],
      maps:to_list(
        maps:map(
          fun(Name, Init) ->
                  Env1 = maps:put(path, Name, Env),
                  Init({serv_spec, Env1})
          end,
          Widgets))).


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

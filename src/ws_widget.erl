-module(ws_widget).
-export([
         %% Convert collection of widgets to initial page template
         layout/2,
         supervisor/2,
         %% Lib code
         sync/1,
         ktv_set_cmd/1,

         %% Widgets
         kvstore_init/1,
         kvstore_edit/1,
         repl/1,
         config/2,
         %% table/1,
         example/1]).

%% Widgets are constructed out of HTML layout, client side
%% functionality implemented in JavaScript (associating behavior to
%% DOM nodes through mixins), and a server side process.
%%
%% Widgets refer to their place in a page through a Path variable.
%% This is a tag that is used for creating HTML id attributes, and
%% dispatching data back to Erlang (by using the tag to look up a
%% child of a supervisor associated to the websocket process).
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
              ws_layout:button(Env, ID, <<"Click Me">>)]};
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
        {serv_spec, #{ ws := Ws } = Env} ->
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
          {'div', [], [ws_layout:table(Env, Labels)]}]},
    %% exml:validate(Body),  %% Assertion
    Body;
kvstore_edit({serv_spec, 
              #{ kvstore := KVStore,
                 ws := Ws,
                 path := Path }=Env}=_Msg) ->
    {handler,
     fun() ->
             log:set_info_name({kvstore_edit,Path}),
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
    try
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
        log:info("update: ~p~n",[KTVList])
    catch
        {put_list_cond,{sqlite3_abort,FailInfo}} ->
            log:info("Condition check failed: ~p~n", [FailInfo]),
            ws:call(Ws, {Path, error}, set, FailInfo)
    end,
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


    

%% Create a new widget by extending the environment.  Note that while
%% this appears a a little inefficient -- a merge operation is
%% executed for each widget call -- this is not really a problem since
%% the function is called only twice: once for layout andonce to
%% start the event handler.
config(Fun,Env1) ->
    fun({Cmd,Env}) -> Fun({Cmd,maps:merge(Env,Env1)}) end.
            

%% Persistent state is supported through the kvstore interface.  This
%% is used for initializing widget state.
kvstore_init(_Env = #{defaults := Defaults, kvstore := KVStore}) ->
    {ok, kvstore:init(KVStore, Defaults)}.

    





%% Sync all inputs with current store state.  This is necessary
%% because the browser keeps its local input state across reloads,
%% which is likely stale.
sync(_Env = #{ kvstore := KVStore, ws := Ws, path := Path }) ->
    Cmds =
        lists:flatten(
          lists:map(
            fun({{P,_},_}=KTV) when P == Path ->
                    [ktv_set_cmd(KTV)];
               (_) ->
                  []
            end,
            kvstore:to_list(KVStore))),
    log:info("Cmds: ~p~n", [Cmds]),
    %% ws:call_sequence(Ws,Cmds),
    ok.

%% Another attempt in simplifying data input handling.  Maybe a rehash
%% of the principles:
%% - Each single page application is associated to a websocket and a kvstore
%% - A page may contain several instances of widgets.
%% - Each widget's input ids are prefixed with a unique path


%% A wrapper around 'set' that can be used on all input widgets.  This
%% handles some special-casing that is hard to do on the javscript end
%% -- see the 'input' export in widgets.js.  This corresponds to how
%% exml:input encodes KTVs.  In retrospect, it makes more sense to
%% always send encoded values, and do the special-casing at the
%% JavaScript end.
ktv_set_cmd({K,{T,V}}) ->
    {K, set,
     case T of
         {finite,_} ->
             %% Select widgets need the encoded items
             type_base:encode({T,V});
         _ ->
             %% By default it is assumed that JSON or BERT layer
             %% translates the decoded values to the correct JavaScript
             %% type.
             V
     end}.
    

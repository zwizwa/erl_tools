-module(exml).
-export([input/2,
         attr_decoder/1,
         input_set_callback/2,
         attr_get/2,
         attr_get_integer/2,
         attr_find/2,
         attr_put/3,
         attr_remove/2,
         attr_merge/2,

         map_attr/2,

         to_binary/1,

         cell/2,

         json/2,

         get_send/1,

         validate/1,

         exml/1


]).


%% XML embedded in Erlang primitives.
-type exml_el() :: {exml_tag(), [exml_attr()], [exml_node()]}.
-type exml_tag() :: atom().
-type exml_text() :: [binary()] | string().
-type exml_node() :: exml_el() | exml_text().
-type exml_attr() :: {atom(), exml_attr_val()}.
-type exml_attr_val() :: binary() | string() | atom().
-export_types([exml_el/0, exml_tag/0, exml_text/0, exml_node/0, exml_attr/0, exml_attr_val/0]).

%% NOTE: exml does not support naked binaries in an element list.
%% exml({p,[],[<<"foo">>,<<"bar">>]}). -> error
%% exml({p,[],[[<<"foo">>],[<<"bar">>]]}). -> ok

%% call into xmerl
-spec exml(exml_el()) -> iolist().
exml({Tag,Attrs,Children}) ->
    %% log:info("xmerl:export_simple start~n"),
    [_, Rv] = xmerl:export_simple(
                [{Tag,Attrs,Children}],
                xmerl_xml,[]),
    %% log:info("xmerl:export_simple end~n"),
    Rv.

%% As an export we only provide one routione: convert a list of
%% elements to a binary.
-spec to_binary([exml_el()]) -> binary().
to_binary(Es) ->
    Bin = iolist_to_binary([exml(E) || E <- Es]),
    List = binary_to_list(Bin),
    UTF8 = unicode:characters_to_binary(List,latin1,utf8),
    UTF8.

%% Special input types.


%% FIXME: Change this to the "Env" model.

%% Any kind of clickable templated element.  This behaves similar to
%% button, but is encoded differently as it is not an <input/> node.
%% See widget.js

%% Keep supporting the old interface for a while.
input(TypeMod, Spec) when is_atom(TypeMod) ->
    input(#{ type_mod => TypeMod }, Spec);

%% The new interface uses environment passing.

%% Note that no action (e.g. as part of a form) needs to be configured
%% explicitly.  The default is to use the default 'handle' routing.
input(Env, {Key, Value}) when is_map(Env) ->
    Input = input_(Env, {Key, Value}),
    input_with_send(Env, Input).
%% FIXME: Yet another routine... End this proliferation!
input_with_send(Env, Input) ->
    case get_send(Env) of
        none -> Input;
        Action -> input_set_callback(Input, Action)
    end.

get_send(Env) ->
    %% By default, use the widget router.
    JS = case maps:find(send, Env) of
             {ok, none} -> none;
             {ok, Send} -> Send;
             _ -> "app." ++ ws:js_send_input(handle)
         end,
    JS.

input_(_Env, {Key, {clickable, {Tag,As,Es}}})  ->
    {Tag,
     attr_merge(
       As,[{'data-name',encode_key(Key)},
           {'data-value',"_"},  %% Value-less event
           {'name',encode_key(Key)},
           {'data-mixin',cell},
           attr_decoder(button)]),
     Es};

%% On buttons: I've found it useful to change the payload of the
%% button without changing the state, so there are two kinds here: the
%% "value-less" button that doesn't change meaning and only carries
%% the key as information, ...
input_(_Env, {Key, {button, Label}}) ->
    true = is_binary(Label),
    {button,
     [{name,encode_key(Key)},
      {value, "_"}, %% Value-less event 
      attr_decoder(button)],
     [[Label]]};

%% ... and the value button that passes on its current label as value.
input_(_Env, {Key, {value_button, Label}}) ->
    true = is_binary(Label),
    {button,
     [{name,encode_key(Key)},
      {value, Label}, %% Value-less event 
      attr_decoder(button)],
     [[Label]]};

input_(_Env, {Key, {boolean, Value}}) ->
    {input,
     [{name,encode_key(Key)},
      {type, checkbox},
      attr_decoder(boolean)]
     ++ checked(Value),
     []};

input_(Env, {Key, {{float, Min, Max}=Type, _Value}=TaggedValue}) ->
    TypeMod = maps:get(type_mod, Env, type_base),
    BinEncoded = TypeMod:encode(TaggedValue),
    {input,
     [{name, encode_key(Key)},
      {style, "width: 100%"}, %% Control it with container div.
      {'data-mixin',input},
      {min, TypeMod:encode({float,Min})},
      {max, TypeMod:encode({float,Max})},
      {step, TypeMod:encode({float,(Max-Min)/2000.0})}, %% FIXME: how to specify?
      {type, range},
      attr_decoder(Type)],
     [[BinEncoded]]};

input_(_Env, {Key, {password, Value}}) -> 
    {input,[{name,encode_key(Key)},
            {type, password},
            {value, Value},
            attr_decoder(binary)],
     [[Value]]};

input_(Env, {Key, {Type, InitTerm}=TaggedValue}) ->
    TypeMod = maps:get(type_mod, Env, type_base),
    BinEncoded = TypeMod:encode(TaggedValue),
    Attrs = [{name, encode_key(Key)},
             attr_decoder(Type)],
    case type_spec(TypeMod,Type) of
        {finite, Vals} -> %% Option box for finite types
            {select,
             Attrs,
             [{option,
               case Term of
                   InitTerm -> [{selected,""}];
                   _ -> []
               end,
               [[Option]]} || {Option,Term} <- Vals]};
        _ -> %% Other types use text input
            {input,
             Attrs ++ [{value,BinEncoded}],
             [[BinEncoded]]}
    end.



%% Edit: ideally, the callback would be rendered together with the
%% input/2 code above, but in practice this is hard to refactor in the
%% application.  For now, leave this a 2-step process.

%% The element is a EXML structure as generated by input/1.  Here we
%% patch in an opaque JavaScript callback based on what makes sense
%% for the type of input.
input_set_callback({T,As,Es},{AttrName,JavaScript}) ->
    %% Attribute specified explicitly
    {T,attr_put(AttrName, JavaScript, As),Es};
input_set_callback({Tag,As,_}=El, JavaScript) ->
    %% Derive attribute from element type.
    AttrName =
        case Tag of
            select -> onchange;
            button -> onclick;
            input  -> 
                case maps:from_list(As) of
                    %% NOT in IE10!
                    #{type := range} -> oninput;
                    _ -> onchange
                end;
            %% Clickable elements can be anything.
            _ -> onclick
        end,
    input_set_callback(El, {AttrName, JavaScript}).



%% TOOLS
attr_decoder(Type) ->
    {'data-decoder',  %% collected by ws.js code
     type_base:encode_type(Type)}.

encode_key(Key) ->
    type_base:encode_key(Key).
type_spec(TypeMod, Type) ->
    apply(TypeMod,type_spec,[Type]).

checked(true)  -> [{checked,checked}];
checked(false) -> [].

%% Some operations on attribute lists.
attr_remove(Key, Attrs) -> lists:keydelete(Key,1,Attrs).
attr_put(Key,Val,Attrs) -> [{Key,Val} | attr_remove(Key,Attrs)].
attr_get(Key,Attrs)     -> proplists:get_value(Key, Attrs).
attr_find(Key,Attrs)    -> case proplists:lookup(Key, Attrs) of
                               {_, Value} -> {ok, Value};
                               none -> {error,{not_found,Key}}
                           end.
attr_merge(InitAttrs, PutAttrs) ->                                   
    lists:foldl(
      fun({Attr,Val},As) -> attr_put(Attr,Val,As) end,
      InitAttrs, PutAttrs).
      
attr_get_integer(Key,Attrs) ->
    list_to_integer(attr_get(Key,Attrs)).
              






%% Simpler to write it as explicit recursion.
map_attr(F, {T,As,Es}) ->
    {T,F(T,As), lists:map(fun(E) -> map_attr(F, E) end, Es)};
map_attr(_, Other) ->
   Other.

    
    
cell(Key,InitEl) ->
    {'div',
     [{'data-behavior',cell},
      {'id', type:encode({pterm,Key})}],
     [InitEl]}.



%% Embed JSON in web page
json(ID,Data) ->
    case apply(json,encode,[Data]) of
        {ok, JSON} ->
            {script,[{type,"application/json"},
                     {id,encode_key(ID)}],
             [[JSON]]};
        {error, json_not_supported=E} ->
            throw({error, {E, Data}})
    end.



%% Single element validator.  Reason: better error messages + explore
%% possibility of static checking.

validate([Bin]) when is_binary(Bin) -> ok;
validate({Tag,As,Es}) when is_atom(Tag) ->
    lists:foreach(fun validate_a/1, As),
    lists:foreach(fun validate/1, Es).
validate_a({Tag,Value}) when is_atom(Tag) and is_binary(Value) -> ok;
validate_a({Tag,Value}) when is_atom(Tag) and is_atom(Value) -> ok.

    
    
    


%% FIXME: sort out old ideas



%% Convert exml to react instantiation code.  FIXME: just an idea -
%% remains to be seen if this is at all useful.  The main reason would
%% be to compose widgets on a page, directed by Erlang code.
%% rexml(El) ->
%%     rexml("React.createElement",El).
%% rexml(CreateEl, {Tag,Attrs,Children}) ->
%%     io_lib:format(
%%       "~s('~s',~s,~s)",
%%       [CreateEl, Tag,
%%        rexml_list([rexml_attr(Attr) || Attr <- Attrs]),
%%        rexml_list([rexml(CreateEl, Child) || Child <- Children])]);
%% rexml(_, Text) ->
%%     io_lib:format("'~s'",[Text]).
%% rexml_attr({_Tag,_Value}) -> "".
%% rexml_list([]) -> "null";
%% rexml_list([H|T]) -> ["[",H,[[",",E]||E<-T],"]"].

%% td_cell({ID,Content}) ->
%%     td_cell({ID,Content,[]});
%% td_cell({ID,Content,ExtraAttrs}) ->
%%     {td,
%%      [{'data-behavior','cell'},
%%       {id, ID}] ++ ExtraAttrs,
%%      [[Content]]}.

%% A generalized right fold for EXML elements.  Constructors are
%% optionally picked from the dictionary if they exist, otherwise the
%% element is left intact.


%% %% Default constructor: preserve structure and recurse on elements.
%% exml_cons_rec(Constructors) ->
%%     fun(T,As,Es) ->
%%             {T,As,lists:map(
%%                     fun(E) -> exml_gfoldr(Constructors, E) end,
%%                     Es)}
%%     end.

%% %% Generic: if a constructor is specified, it is assumed to perform
%% %% the recursion.  This allows more control over the iteration.
%% exml_gfoldr(Constructors, {Tag, Attrs, Children}) ->
%%     Cons =
%%         case maps:find(Tag, Constructors) of
%%             {ok, Fun} -> Fun;
%%             _ ->
%%                 case maps:find('_', Constructors) of
%%                     {ok, Fun} -> Fun;
%%                     _ -> exml_cons_rec(Constructors)
%%                 end
%%         end,
%%     Cons(Tag, Attrs, Children);
%% exml_gfoldr(_Constructors, Other) ->
%%     %% FIXME: leaf nodes?
%%     Other.




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

         %% HMAC for encoding binary terms in JavaScript strings.
         hmac_key/0, hmac/2, hmac_encode/2, hmac_decode/2

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
    [_, Rv] = xmerl:export_simple(
                [{Tag,Attrs,Children}],
                xmerl_xml,[]),
    Rv.

%% As an export we only provide one routione: convert a list of
%% elements to a binary.
-spec to_binary([exml_el()]) -> binary().
to_binary(Es) -> iolist_to_binary([exml(E) || E <- Es]).


%% Special input types.


%% Any kind of clickable templated element.  This behaves similar to
%% button, but is encoded differently as it is not an <input/> node.
%% See widget.js
input(_TypeMod, {Key, {clickable, {Tag,As,Es}}}) ->
    {Tag,
     attr_merge(
       As,[{'data-name',encode_key(Key)},
           {'data-value',"_"},  %% Value-less event
           attr_decoder(button)]),
     Es};

input(_TypeMod, {Key, {button, Label}}) ->
    true = is_binary(Label),
    {button,
     [{name,encode_key(Key)},
      {value, "_"}, %% Value-less event 
      attr_decoder(button)],
     [[Label]]};

input(_TypeMod, {Key, {boolean, Value}}) ->
    {input,
     [{name,encode_key(Key)},
      {type, checkbox},
      attr_decoder(boolean)]
     ++ checked(Value),
     []};

input(TypeMod, {Key, {{float, Min, Max}=Type, _Value}=TaggedValue}) ->
    BinEncoded = TypeMod:encode(TaggedValue),
    {input,
     [{name, encode_key(Key)},
      {min, TypeMod:encode({float,Min})},
      {max, TypeMod:encode({float,Max})},
      {step, TypeMod:encode({float,(Max-Min)/100.0})}, %% FIXME: how to specify?
      {type, range},
      attr_decoder(Type)],
     [[BinEncoded]]};

input(_TypeMod, {Key, {password, Value}}) -> 
    {input,[{name,encode_key(Key)},
            {type, password},
            {value, Value},
            attr_decoder(binary)],
     [[Value]]};

input(TypeMod, {Key, {Type, InitTerm}=TaggedValue}) ->
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
              


%% Key used for term authentication (e.g. closures).
%% Limit time-validity of key to one gw boot session.
%% Failed keys will cause websocket processes to die, disconnecting
%% socket which causes client to reconnect.
hmac_key() ->
    Pid = serv:up(
            hmac_key,
            {handler,
             fun() -> #{ key => crypto:strong_rand_bytes(32) } end,
             fun obj:handle/2}),
    unlink(Pid),
    obj:get(Pid, key).
%% hmac_key() -> <<"oT8LGqAtMTGKyBHqoA7ky3PCzjTN5L">>.
hmac(GetKey,Bin) when is_binary(Bin) -> 
    crypto:hmac(sha256,GetKey(),Bin).

%% Encode/decode for tunneling through JSON, cookies, embedded JS,
%% URLs, ...  Use base64 encoding.

hmac_encode(GetKey,Obj) ->
    Bin = term_to_binary(Obj),
    Hmac = hmac(GetKey,Bin),
    base64:encode(term_to_binary({Bin,Hmac})).

hmac_decode(GetKey,Base64) ->
    {Bin,Hmac} = binary_to_term(base64:decode(Base64)),
    case hmac(GetKey,Bin) of
        Hmac  -> {ok, binary_to_term(Bin)};
        Hmac1 -> {error, {hmac_fail, Hmac, Hmac1}}
    end.




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




-module(type_base).
-export([
         %% Convert values between binary and terms.
         encode/1, decode/1,

         %% Convert type tags between binary and terms.
         encode_type/1, decode_type/1,

         %% In case of human input, error handling is application
         %% level responsibility.
         decode_try/1,

         %% Perform encode->decode roundtrip and compare.
         valid/1,

         %% Type specification , mostly useful for finite types.
         type_spec/1, finite/1,
         encoder/1, decoder/1,

         %% Convert error message to user-readable message.
         format_error/1,

         %% Serialization. Parameterized by module.
         encode_key/1, decode_key/1,
         decode_tv/2, encode_ktv/2, decode_ktv/2,
         
         %% Misc tools
         atom/1, int/1, rotate_finite/1, skip_finite/1, si_power/1,
         
         %% Row processing, e.g. for db
         decode_row/1, encode_row/1,
         decode_table/2, encode_table/2
         
        ]).

%% FIXME: How to enable dialyzer to infer this?  Currently it just
%% infers atom() instead of the sum of specific atoms.
-type name() ::
        chanrep | edge | binary | device | int | atom | pterm | term | 
        boolean | pc_serial | hex | mac | ip | ip_nm |
        {int,integer(),integer()} | 
        %% {abstract,fun(( finite_values | encode | decoder ) -> any())} |
        finite().

-type finite() :: {finite, [{binary(), any()}]}.

-type encoder() :: fun((_) -> binary()).
-type decoder() :: fun((binary()) -> any()).


%% Types can be extended.  User should create a module with
%% encode/decode and type_encode/type_decode functions calling into
%% this module, providing impl().
-type impl() :: {impl, name(), finite()|{encoder(),decoder()}}.

-spec encoder(name() | impl()) -> encoder().
encoder({impl, TypeSpec, {finite, Alist}}) ->
    InvAlist = [{B,A} || {A,B} <- Alist],
    convert_finite(TypeSpec, InvAlist);
encoder({impl, _, {Encode, _}}) ->
    Encode;
%% No implementation provided, use base types from this module.
encoder(TypeSpec) ->
    encoder({impl, TypeSpec, type_spec(TypeSpec)}).
    


-spec decoder(name() | impl()) -> decoder().
decoder({impl, TypeSpec, {finite, Alist}}) ->
    convert_finite(TypeSpec, Alist);
decoder({impl, _, {_, Decode}}) ->
    Decode;
decoder(TypeSpec) ->
    decoder({impl, TypeSpec, type_spec(TypeSpec)}).
    




    


%% Some design notes:
%%
%% - Parse failures are handled with exceptions.  A decode_try
%%   function is provided to convert to ok/error format.
%%
%% - Decoders can assume binary input for simplicity.  IOLists are
%%   converted to binary at the top.
%%
%% - Encoders can produce IOLists.  Output is converted to binary at
%%   the top.



 

-spec decode_try({name(),iolist()}) -> {ok,_} | {error,_}.
decode_try(Arg) ->
    try decode(Arg) of
        Rv -> {ok, Rv}
    catch
        error:{case_clause,Type} -> 
            {error, {{bad_type, Type}, fb("bad type: ~p",[Type])}};
        {type, Info} ->
            {error, Info}
    end.

    
-spec encode({name()|impl(),_}) -> binary().
encode({Type, Val}) -> iolist_to_binary((encoder(Type))(Val)).

-spec decode({name()|impl(),binary()}) -> _.
decode({Type, Bin}) when is_binary(Bin) -> (decoder(Type))(Bin).

%% decode({Type, IOList}) -> decode({Type,iolist_to_binary(IOList)}). %% convenient

-spec valid({name(),_}) -> _.            
valid({Type,Val}=TV) ->    
    {ok,Val} == decode({Type, encode(TV)}).

-spec stop(name(),_,_) -> no_return().
stop(Type, Val, {Fmt, List}) ->
    throw({type,{Type, Val, fb(Fmt,List)}});
stop(Type, Val, Msg) ->
    throw({type,{Type, Val, iolist_to_binary(Msg)}}).


    
%% Reusable encode/decode primitives.
fb(Fmt,List) when is_list(List) -> tools:format_binary(Fmt,List).
%%fb_w(Obj) -> fb("~w",[Obj]).
fb_p(Obj) -> fb("~99999p",[Obj]).

id(X) -> X.
    
enc_atom(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom,utf8).
    
atom(Bin) ->
    try binary_to_atom(Bin, utf8)
    catch _:_ -> stop(atom, Bin, <<"Bad Atom">>) end.
            
int(Bin) ->
    try binary_to_integer(Bin)
    catch _:_ -> stop(int, Bin, <<"Bad Integer">>) end.

float(Bin) ->
    try binary_to_float(Bin)
    catch _:_ -> 
            try binary_to_integer(Bin) + 0.0
            catch _:_ -> stop(float, Bin, <<"Bad Float">>) end
    end.
             
term(Bin) ->    
    try erlang:binary_to_term(Bin)
    catch _:_ -> stop(term, Bin, <<"Bad Term">>) end.

%% Note that not all terms are serializable through ~p printing.
%% pterm encoder checks if decode(encode(Term)) == Term.
pterm(Bin) ->
    try
        Str = binary_to_list(Bin),
        {ok, Tokens, _} = erl_scan:string(Str ++ "."),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch _:_ -> stop(pterm, Bin, <<"Syntax Error">>) end.
            

-spec enc_pterm(_) -> _.
enc_pterm(Term) ->
    try
        Bin = fb_p(Term),
        %% Check if it is reversible.
        Term = decode({pterm,Bin}),
        Bin
    catch
        _:_ ->
            %% log:info("Can't represent ~p~n",[Term]),
            stop(pterm, Term, <<"Can't represent">>)
    end.
    

                  


              
%% -spec type_spec(name()) -> finite() | {encoder(), decoder()}.
type_spec(Type) ->
    case Type of
        %% {abstract,Obj} ->
        %%     case Obj(finite_values) of
        %%         {ok, Alist} -> {finite, Alist};
        %%         error -> {Obj(encoder), Obj(decoder)}
        %%     end;
        si_prefix -> 
            {finite,
             [{<<"n">>,-3},
              {<<"u">>,-2},
              {<<"m">>,-1},
              {<<"">>,0},
              {<<"k">>,1},
              {<<"M">>,2},
              {<<"G">>,3}
             ]};

        {finite, _} = Spec -> Spec;
        binary  -> {fun id/1,   fun id/1};
        button    -> {fun id/1,   fun id/1}; %% see web.erl input/1 button type
        clickable -> {fun id/1,   fun id/1}; %% see web.erl input/1 clickable type
        int     -> {fun fb_p/1, fun int/1};
        float   -> {fun fb_p/1, fun float/1};
        atom    -> {fun enc_atom/1, fun atom/1};
        pterm   -> {fun enc_pterm/1, fun pterm/1};
        term    -> {fun term_to_binary/1, fun term/1};
        boolean ->
            {fun fb_p/1,
             fun(Val) ->
                     case atom(Val) of
                         true -> true;
                         false -> false;
                         _ -> stop(boolean, Val, "Bad boolean")
                     end
             end};
        {int,Min,Max} ->
            {fun fb_p/1,
             fun(Val) ->
                     Int = decode({int, Val}),
                     case (Int >= Min) and (Int =< Max) of
                         true -> Int;
                         false -> stop({int,Min,Max}, Int, {"Outside of range: [~p,~p]",[Min,Max]})
                     end
             end};
        {float,Min,Max} ->
            {fun fb_p/1,
             fun(Val) ->
                     Float = decode({float, Val}),
                     case (Float >= Min) and (Float =< Max) of
                         true -> Float;
                         false -> stop({float,Min,Max}, Float, {"Outside of range: [~p,~p]",[Min,Max]})
                     end
             end};
        hex ->
            {fun fb_p/1, %% FIXME: wrong!
             fun(Bin) ->
                     try [V] = tools:unhex(binary_to_list(Bin)), V
                     catch _:_ -> stop(hex, Bin, "Bad HEX value")
                     end
             end};
        datetime ->
            %% SQLITE3 DATETIME format <-> format used by
            %% calendar:local_time().
            {fun({{Year,Month,Day},
                  {Hour,Minute,Second}}) ->
                     tools:format_binary(
                       "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                       [Year,Month,Day,Hour,Minute,Second])
             end,
             fun(Bin) ->
                     {match, [_|Bins]} =
                         re:run(Bin,
                                "(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+):(\\d+)",
                                [{capture,all,binary}]),
                     [Year,Month,Day,Hour,Minute,Second] =
                         lists:map(fun binary_to_integer/1, Bins),
                     {{Year,Month,Day},
                      {Hour,Minute,Second}}
             end
            };
        mac ->
            {fun({A,B,C,D,E,F}) ->
                     H = fun(V) -> tools:hex8(V) end,
                     fb("~s:~s:~s:~s:~s:~s",
                        [H(V) || V <- [A,B,C,D,E,F]])
             end,
             fun(Val) ->
                     X=fun(Bin) -> decode({hex,Bin}) end,
                     case re:split(Val,":") of
                         [A,B,C,D,E,F] -> {X(A),X(B),X(C),X(D),X(E),X(F)};
                         _ -> stop(mac, Val, "Bad MAC")
                     end
             end};
        ip ->
            {fun({A,B,C,D}) ->
                     fb("~p.~p.~p.~p", [A,B,C,D])
             end,
             fun(Val) ->
                     X=fun(Bin) -> decode({{int,0,255},Bin}) end,
                     case re:split(Val,"\\.") of
                         [A,B,C,D] -> {X(A),X(B),X(C),X(D)};
                         _ -> stop(ip, Val, "Bad IP")
                     end
             end};
        ip_nm ->
            {fun({IP,NM}) ->
                     fb("~s/~p", [encode({ip,IP}),NM])
             end,
             fun(Val) ->
                     case re:split(Val,"/") of
                         [IP,NM] ->
                             {decode({ip,IP}),
                              decode({{int,0,24},NM})};
                         _ ->
                             stop(ip_nm, Val, "Bad IP/NM pair")
                     end
             end}
    end.



%% Types constructed at run time are all finite types.  Provide a
%% shortcut. 
%%abstract_finite(BinToValList) ->
%%     {abstract, fun(finite_values) -> {ok, BinToValList} end}.

finite(BinToValList) -> {finite, BinToValList}.
    

convert_finite(Type, PL) ->    
    fun(Val) ->
            case proplists:get_value(Val,PL) of
                undefined ->
                    Vals = [s(K) || {K,_} <- PL],
                    Allowed = [hd(Vals), [[",",V] || V <- tl(Vals)]],
                    stop(Type, Val, {"Bad ~p, options are: ~s", [Type,Allowed]});
                El -> El
            end
    end.
  
%% For non-quoted printing of binaries and other objects.
s(Bin) when is_binary(Bin) -> Bin;
s(P) -> io_lib:format("~p",[P]).


%% Type specs need to be printable Erlang terms.
encode_type(Term) -> encode({pterm,Term}).
decode_type(Bin)  -> decode({pterm,Bin}).


format_error({type,{_Type, Input,Error}}) ->
    %% User is not interested in rep, leave out _Type
    tools:format_binary("~s in '~s'",[Error,Input]);
format_error(Error) ->
    %% Falback in case of bugs
    tools:format_binary("~p",[Error]).
    

%% A type is:

%% - validator from representation (text) to internal data type that
%%   has guaranteed constraints.
%%   {ok, Value} or {error,{Atom,HumanError}}
%%
%% - converter from internal representation to text that converts back
%%   to the original form.

%% Additional context:
%%
%% - Database (sqlite) contains text representation
%% - Configuration database has annotation var -> type




%% Some tools



    
rotate_finite(Dir) -> 
    fun(V) -> next_finite(V, Dir, true) end.
skip_finite(Dir) ->
    fun(V) -> next_finite(V, Dir, false) end.

%% Generic routine
next_finite({{finite,Name2Val}=Type,Current}, Dir, Extend) ->
    Vals0 = [Val || {_,Val} <- Name2Val],
    Vals = case Dir of
               next -> Vals0;
               prev -> lists:reverse(Vals0)
           end,
    Next = next_el(
             Current, 
             case Extend of
                 true -> Vals ++ Vals;
                 false -> Vals
             end),
    {Type, Next}.

%% Find the next element in line.
next_el(Key,[Key,Next|_]) -> Next;
next_el(Key,[_|List]) -> next_el(Key,List);
next_el(Key,[]) -> Key.



%% Canonical way to represent type-tagged erlang terms as
%% human-readable binary triplets, for db storage and user interfaces.
%% See type_base.erl

encode_key(Key) -> encode({pterm,Key}).
decode_key(Key) -> decode({pterm,Key}).

encode_ktv(TypeMod, {Key, {Type, Val}}) ->
    [encode_key(Key),
     encode_type(Type),
     TypeMod:encode({Type,Val})].

decode_tv(TypeMod, [BinType, BinVal]) ->
    Type = decode_type(BinType),
    {Type, TypeMod:decode({Type,BinVal})}.

decode_ktv(TypeMod, [BinKey | BinTV]) ->
    {decode_key(BinKey),
     decode_tv(TypeMod, BinTV)}.

%% Pick a power of 1000 that fits in the si_prefix range to give 1,2,3
%% digit mantissas.
si_power(Float) ->
    Grid = trunc(math:log10(Float)/3 + 3),
    max(0, min(6, Grid)) - 3.
    
%% Curried so it can be used on tables, e.g:
%% lists:map(
%%   type_base:decode_row([binary,pterm,pterm,binary]),
%%   sql_query(<<"select * from table")).
    
decode_row(Types) ->
    fun(Row) ->
            lists:map(
              fun type:decode/1,
              lists:zip(Types,Row))
    end.
encode_row(Types) ->
    fun(Row) ->
            lists:map(
              fun type:encode/1,
              lists:zip(Types,Row))
    end.

decode_table(Types, Table) -> lists:map(decode_row(Types), Table).
encode_table(Types, Table) -> lists:map(encode_row(Types), Table).

-module(parse).
-export([bm_tok/2,test/0]).

%% Parse utils
test() ->
    Fold = 
        bm_tok(
          #{ %% Used by tokenizer
             $" => quote,
             $\ => escape,
             %% Left in output stream
             ${ => open,
             $} => close,
             $, => comma,
             $= => equal },
          "download,{section=\".text\",section-sent=\"1440\",section-size=\"34032\",total-sent=\"1676\",total-size=\"625300\""),
    fold:to_list(Fold).

%% Bi-modal quote/escape tokenizer with single-character controls.
%% This structure seems quite common for ad-hoc languages.
bm_tok(Control, List) ->
    fun(Fun, Init) ->
            tok_fld(Control, normal, [], List, Fun, Init)
    end.

%% Left fold core routine.
tok_fld(_,normal,Stack,[],F,S) -> atm(Stack,F,S);
tok_fld(C,normal,Stack,[Char|Rest], F, S) ->
    case maps:find(Char, C) of
        {ok, escape} -> error(bad_escape);
        {ok, quote}  -> tok_fld(C, quote, Stack, Rest, F, S);
        {ok, Token}  -> tok_fld(C, normal, [], Rest, F, F(Token,atm(Stack,F,S)));
        _            -> tok_fld(C, normal, [Char | Stack], Rest, F, S)
    end;
tok_fld(C,quote,Stack,[Char|Rest], F, S) ->
    case maps:find(Char, C) of
        {ok, escape} ->
            case Rest of
                [Char1|Rest1] -> tok_fld(C,quote,[Char1|Stack],Rest1,F,S);
                _ ->             error(bad_quote)
                    
            end;
        {ok, quote} -> tok_fld(C,normal,Stack,Rest,F,S);
        _ ->           tok_fld(C,quote,[Char|Stack],Rest,F,S)
            
    end.
atm([], _, S) -> S;
atm(Stack, F, S) -> F({atom,lists:reverse(Stack)},S).

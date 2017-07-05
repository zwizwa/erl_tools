-module(parse).
-export([bm_tok/2,test/0]).

%% Parse utils
test() ->
    bm_tok(
      #{ %% Used by tokenizer
         $" => quote,
         $\ => escape,
         %% Left in output stream
         ${ => open,
         $} => close,
         $, => comma,
         $= => equal },
      "download,{section=\".text\",section-sent=\"1440\",section-size=\"34032\",total-sent=\"1676\",total-size=\"625300\"").

%% Bi-modal quote/escape tokenizer with single-character controls.
%% This structure seems quite common.
bm_tok(Control, List) ->
    bm_tok(Control,normal,[],List).
bm_tok(_,normal,Stack,[]) -> atm(Stack);
bm_tok(C,normal,Stack,[Char|Rest]) ->
    case maps:find(Char, C) of
        {ok, escape} -> error(bad_escape);
        {ok, quote}  -> bm_tok(C, quote, Stack, Rest);
        {ok, Token}  -> atm(Stack) ++ [Token | bm_tok(C, normal, [], Rest)];
        _            -> bm_tok(C, normal, [Char | Stack], Rest)
    end;
bm_tok(C,quote,Stack,[Char|Rest]) ->
    case maps:find(Char, C) of
        {ok, escape} ->
            case Rest of
                [Char1|Rest1] -> bm_tok(C,quote,[Char1|Stack],Rest1);
                _ ->             error(bad_quote)
                    
            end;
        {ok, quote} -> bm_tok(C,normal,Stack,Rest);
        _ ->           bm_tok(C,quote,[Char|Stack],Rest)
            
    end.
atm([]) -> [];
atm(Stack) -> [{atom, lists:reverse(Stack)}].

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
          source:from_list(
            "download,{section=\".text\",section-sent=\"1440\",section-size=\"34032\",total-sent=\"1676\",total-size=\"625300\"")),
    fold:to_list(Fold).

%% Bi-modal quote/escape tokenizer with single-character controls.
%% This structure seems quite common for ad-hoc languages.
%% Token stream structured as a left fold.
%% Input can be a list or a pair/eof generator.
bm_tok(Control, InStream) ->
    fun(Fun, Init) ->
            tok_fld(Control, normal, [], upk(InStream), Fun, Init)
    end.

%% Input is a source.erl outer iterator.
upk(Stream) -> source:unpack(Stream).

%% Left fold core routine.
tok_fld(_,normal,Stack,eof,F,S) -> atm(Stack,F,S);
tok_fld(C,normal,Stack,{Char,Rest},F,S) ->
    case maps:find(Char, C) of
        {ok, escape} ->
            error(bad_escape);
        {ok, quote} ->
            tok_fld(C, quote, Stack, upk(Rest), F, S);
        {ok, Token} ->
            tok_fld(C, normal, [], upk(Rest), F, F(Token,atm(Stack,F,S)));
        _ ->
            tok_fld(C, normal, [Char | Stack], upk(Rest), F, S)
    end;
tok_fld(C,quote,Stack,{Char,Rest},F,S) ->
    case maps:find(Char, C) of
        {ok, escape} ->
            {Char1,Rest1} = upk(Rest),
            tok_fld(C,quote,[Char1|Stack],upk(Rest1),F,S);
        {ok, quote} ->
            tok_fld(C,normal,Stack,upk(Rest),F,S);
        _ ->
            tok_fld(C,quote,[Char|Stack],upk(Rest),F,S)
            
    end.
atm([], _, S) -> S;
atm(Stack, F, S) -> F({atom,lists:reverse(Stack)},S).

-module(parse).
-export([bimodal_tokenize/2, csv_tok/1, csv/1]).


%% Bi-modal quote/escape tokenizer with single-character controls.
%% This structure seems quite common for ad-hoc languages.
%% Token stream structured as a left fold.

%% Input is either an igen.erl impure generator, or a source.erl lazy
%% stream.  
bimodal_tokenize(Config, {igen,_,_}=IGen) ->
    %% upk/1 is only called once for each stream head, so
    %% to_source_leaky/1 can be used as long as we close properly.
    try bimodal_tokenize(Config, igen:to_source_leaky(Config,IGen))
    catch C:E -> igen:close(IGen), throw({C,E})
    end;

bimodal_tokenize(Config, InSrc) ->
    fun(Fun, Init) ->
            tok_fld(Config, normal, [], upk(InSrc), Fun, Init)
    end.

%% Input uses the source.erl interface, with the added promise to only
%% unpack each head once, meaning it can operate on igen as well.


%% Input is an igen.erl impure generator.
upk(Src) -> source:unpack(Src).

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
            CharTx = 
                case maps:find({escape,Char1},C) of
                    {ok, CharEsc} -> CharEsc;
                    _ -> Char1
                end,
            tok_fld(C,quote,[CharTx|Stack],upk(Rest1),F,S);
        {ok, quote} ->
            tok_fld(C,normal,Stack,upk(Rest),F,S);
        _ ->
            tok_fld(C,quote,[Char|Stack],upk(Rest),F,S)
            
    end.
%%atm([],    _, S) -> S; %% Skip empty?
atm(Stack, F, S) -> F({atom,lists:reverse(Stack)},S).



csv_tok({list, List}) ->
    csv_tok(source:from_list(List));
csv_tok({file, FileName}) ->
    {ok, Data} = file:read_file(FileName),
    csv_tok({list, binary_to_list(Data)});
csv_tok(InSrc) ->
    bimodal_tokenize(
      #{ %% Used by tokenizer
         $"  => quote,
         $\\ => escape,
         %% Left in output stream
         $,  => comma,
         $\n => lf,
         $\r => cr,
         %% Escaped characters
         {escape, $r} => 13,
         {escape, $n} => 10
       },
      InSrc).

csv(In) ->
    Tokens = fold:to_list(csv_tok(In)),
    csv_p(Tokens, [], []).

r(Q) -> lists:reverse(Q).
b(L) -> list_to_binary(L).

%% Not really a parser: doesn't need a stack to perform recursion.
%% Compare e.g. to gdb.erl msg_parse
    
%% I: input
%% C: colum state
%% R: row state

csv_p([{atom,""}],  [], R)   -> r(R); %% empty atom between last lf and eof.
csv_p([{atom,A}|I], C,  R)   -> csv_p(I, [b(A)|C], R);
csv_p([comma   |I], C,  R)   -> csv_p(I, C,        R); %% only used for tokenizing
csv_p([cr      |I], C,  R)   -> csv_p(I, C,        R); %% ignored, support CRLF as well
csv_p([lf      |I], C,  R)   -> csv_p(I, [],       [r(C)|R]); 

csv_p(Input, Queue, Stack) -> error({parse,Input,Queue,Stack}).

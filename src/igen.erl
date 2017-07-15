-module(igen).
-export([read/1,close/1,
         from_fold/1,to_fold/1,to_list/1,to_rlist/1,from_list/1,

         to_source_leaky/1]).

%% IMPURE GENERATORS
%%
%% A stream is represented as an effectful generator that produces the
%% elements on subsequent calls.


%% Because of side effects, gens need to be closed to free all
%% associated resources.
read({igen,Read,_}) -> Read().
close({igen,_,Close}) -> Close().

to_fold(Gen) ->
    fun(F,S) -> to_fold(Gen,F,S) end.
to_fold(Gen,F,S) ->
    case read(Gen) of
        {data, Val} -> to_fold(Gen,F,F(Val,S));
        eof -> S
    end.


    
slurp(Gen) ->
    (to_fold(Gen))(fun(_,_)->ok end, none).

to_rlist(Gen) ->
    (to_fold(Gen))(fun(H,T)->[H|T] end, []).
to_list(Gen) ->
    lists:reverse(to_rlist(Gen)).
    

%% The most obvious application is to turn a fold inside out: an
%% internal iterator into an external.

%% FIXME: pfold.erl
from_fold(Fold) ->
    Sync = fun(Val) ->
                   receive 
                       {Pid, read} -> obj:reply(Pid, Val)
                   end
           end,
    Serv = spawn_link(
             fun() ->
                     Fold(fun(Val,_) ->
                                  Sync({data,Val})
                          end,
                          nostate),
                     Sync(eof)
             end),
    Read  = fun() -> obj:call(Serv, read) end,
    Close = fun() -> slurp({igen,Read,none}) end,
    {igen, Read, Close}.


%% Allow for a conversion of a generator into source.erl stream, with
%% the caveat that it is a leaky abstraction:

%% 1. The stream needs to be used up until eof, or in some other way
%%    the original igen:close/1 needs to be called.
%%
%% 2. source:unpack/1 can only be called once on each head.

%% This is still useful if those guarantees can be met inside an
%% implementation, e.g. see parse:bimodal_tokenize

to_source_leaky({igen,Read,_Close}=Gen) ->
    fun() ->
            case Read() of
                {data, Val} -> {Val, to_source_leaky(Gen)};
                eof -> eof
            end
    end.


%% This needs some place to store the state!  Use a process.
from_list(List) ->
    from_fold(fold:from_list(List)).

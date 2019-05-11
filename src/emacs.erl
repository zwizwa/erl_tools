-module(emacs).
-export([revert/1, lisp/1, eval/1]).

%% There are two main ways to send stuff to emacs:
%% - emacsclient -e <lisp>
%% - distel

%% I've been using the former for a while, but it is quite cumbersome.
%% Since exo can be assumed to be always tied to an emacs session,
%% let's use some distel functionality to so this.

revert(File) ->
    eval(
      ['save-current-buffer',
       ['set-buffer', ['get-buffer', iolist_to_binary(File)]],
       ['revert-buffer', t, t]]).

eval(Lisp) ->
    %% FIXME: If there is an Erlang connection into distel, use that
    %% instead.  Otherwise fall back on assuming emacs is running
    %% locally, and we can use emacsclient.
    emacsclient_eval(Lisp).
emacsclient_eval(Lisp) ->
    Cmd = tools:format("emacsclient -e '~s'", [lisp(Lisp)]),
    %% log:info("emacs: ~s~n",[Cmd]),
    os:cmd(Cmd).
    

%% This is an approximate mapping.
lisp([]) ->
    "()";
lisp([A]) ->
    [$(,lisp(A),$)];
lisp([A0|As]) ->
    [$(,lisp(A0),
     [[32,lisp(A)] || A <- As],
     $)];
lisp(Atom) when is_atom(Atom) ->
    io_lib:format("~s", [atom_to_list(Atom)]);
lisp(Bin) when is_binary(Bin) ->
    io_lib:format("~p", [binary_to_list(Bin)]);
lisp(Any) -> 
    io_lib:format("~p", [Any]).
    



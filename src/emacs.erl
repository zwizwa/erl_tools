-module(emacs).
-export([revert/1, lisp/1, send_lisp/1,
         emacsclient_eval/1,
         distel_send_lisp/1,
         distel_node/0]).

%% There are two main ways to send stuff to emacs:
%% - emacsclient -e <lisp>
%% - distel

%% I've been using the former for a while, but it is quite cumbersome.
%% Since exo can be assumed to be always tied to an emacs session,
%% let's use some distel functionality to so this.

revert(File) ->
    send_lisp(
      ['save-current-buffer',
       ['set-buffer', ['get-buffer', iolist_to_binary(File)]],
       ['revert-buffer', t, t]]).

%% Unidirectional send.
send_lisp(Lisp) ->
    %% emacsclient_eval(Lisp).
    distel_send_lisp(Lisp),
    ok.

-define(DEVNODE,'exo@10.1.3.29').

emacsclient_eval(Lisp) ->
    %% FIXME: Emacsclient is only assumed to run on the dev node
    case node() of
        ?DEVNODE ->
            Cmd = tools:format("emacsclient -e '~s'", [lisp(Lisp)]),
            %% log:info("emacs: ~s~n",[Cmd]),
            os:cmd(Cmd);
        _ ->
            rpc:call(?DEVNODE,emacs,emacsclient_eval,[Lisp])
    end.

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


%% Naming scheme is  distel_<emacspid>@<hostname>
%% So we see someting like distel_995@panda 
distel_node() ->
    distel_node(nodes(hidden)).
distel_node([]) -> error;
distel_node([N|Ns]) ->
    case lists:sublist(atom_to_list(N),1,6) of
        "distel" -> {ok,N};
        _ -> distel_node(Ns)
    end.
    
distel_send_lisp(Lisp) ->
    log:info("elisp: ~999p~n", [Lisp]),
    distel_send({eval, Lisp}).
distel_send(Msg) ->
    case {distel_node(), node()} of
        {{ok, Node}, _} -> 
            {exo_handle, Node} ! Msg;
        {error, ?DEVNODE} ->
            exit('no_distel_on_29');
        {error, _} ->
            rpc:call(?DEVNODE, emacs, distel_send_lisp, Msg)
    end.


%% E.g. emacs:distel_send_lisp([message, [format, "%s", self()]]).


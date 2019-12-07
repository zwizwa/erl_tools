-module(emacs).
-export([revert/1, message/1,
         send_lisp/1,
         %% Internals
         lisp/1,
         connect/0,
         hop_node/0,
         emacsclient_send_lisp/1,
         distel_send_lisp/1,
         distel_node/0,
         start_link/0,
         handle/2
]).


%% Unidirectional send.
%%
send_lisp(Lisp) ->
    %% There are two main ways to send stuff to emacs:
    %% - emacsclient -e <lisp>
    %% - distel
    %%
    %% The default here is to use distel, as it is better contained,
    %% and we don't have to serialize s-expressions to textual form.
    %%
    distel_send_lisp(Lisp),
    %% emacsclient_send_lisp(Lisp).
    ok.


message(Bin) when is_binary(Bin) ->
    send_lisp(['message', Bin]);
message(Term) ->
    message(tools:format_binary("~p",[Term])).


revert(File) ->
    send_lisp(
      ['save-current-buffer',
       ['set-buffer', ['get-buffer', iolist_to_binary(File)]],
       ['revert-buffer', t, t]]).


-define(DEVNODE,'exo@10.1.3.29').


%%%% DISTEL



%% Naming scheme is distel_<emacspid>@<hostname>
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
    %% log:info("elisp: ~999p~n", [Lisp]),
    distel_send({eval, Lisp}).
distel_send(Msg) ->
    case {distel_node(), node()} of
        {{ok, Node}, _} -> 
            {exo_handle, Node} ! Msg;
        {error, ?DEVNODE} ->
            exit('no_distel_on_29');
        {error, _} ->
            case connect() of
                {ok, Node} -> 
                    {exo_handle, Node} ! Msg;
                error ->
                    error
            end
    end.


connect() ->
    case distel_node() of
        error ->
            %% Attempt to set up a connection via another node that is
            %% connected.
            case hop_node() of
                error -> error;
                {ok, HopNode} ->
                    rpc:call(HopNode, emacs, distel_send_lisp,
                             [['erl-connect', ['quote', node()]]]),
                    wait_connect(50,100)
            end;
        Ok -> Ok
    end.
wait_connect(0,_) -> error;
wait_connect(N,T) -> 
    case distel_node() of
        error ->
            timer:sleep(T),
            wait_connect(N-1,T);
        Ok -> Ok
    end.

hop_node() ->
    hop_node(nodes()).
hop_node([]) -> error;
hop_node([Node|Nodes]) ->
    case rpc:call(Node, emacs, distel_node, []) of
        {ok, _} -> {ok, Node};
        _Error -> hop_node(Nodes)
    end.

            
%% exo_handle proxy.  It seemms simpler to do it this way, since the
%% emacs node is hidden, and we can re-establish connection if one of
%% the other nodes has it.
start_link() ->                     
    {ok,
     serv:start(
       {handler,
        fun() -> #{} end,
        fun ?MODULE:handle/2})}.
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    {ok, Node} = distel_node(),
    {exo_handle, Node} ! Msg,
    State.



                




%% E.g. emacs:distel_send_lisp([message, [format, "%s", self()]]).




%%%% EMACSCLIENT

%% Emacsclient requires serialization to textual form.

emacsclient_send_lisp(Lisp) ->
    %% FIXME: Emacsclient is only assumed to run on the dev node
    case node() of
        ?DEVNODE ->
            Cmd = tools:format("emacsclient -e '~s'", [lisp(Lisp)]),
            %% log:info("emacs: ~s~n",[Cmd]),
            os:cmd(Cmd);
        _ ->
            rpc:call(?DEVNODE,emacs,emacsclient_send_lisp,[Lisp])
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




%% Protocol wrappers for TAG_U32-based RPC protocol.

%% Some generic notes.
%%
%% - At it's core, the TAG_U32 protocol is a message protocol using
%%   composite addressing: an address consists of an array of u32,
%%   together with an opaque binary payload.
%%
%% - It is just a message container format. It does not have any
%%   intrinsic meaning.  The interpretation of the composite address
%%   and the binary payload is completely arbitrary.
%%
%% - However, in most cases we interpret the leftmost part of the
%%   array as a path in a nested map structure.  The rightmost part of
%%   the array is interpreted as function call arguments containing
%%   u32 identifiers that can be interpreted by the receiver.  The
%%   meaning of the binary payload is disambiguated by this path and
%%   the function call arguments.
%%
%% - The hierarchical interpretation allows the protocol to be used as
%%   a "source routing" protocol.  E.g. a message handler running on
%%   machine A only needs to know how to handle the first element in
%%   the array.  It can then pop the first element and either pass it
%%   on to a local procedure on node A, or forward the message to
%%   another machine B in the network for which the object acts as a
%%   router.
%%
%% - The only "binary" part of the protocol (where integer identifiers
%%   have a fixed meaning) is the name resolution protocol.  All other
%%   identifiers can be either discovered at run time, or cached at
%%   compile time in a dictionary.  The important part here is that
%%   the dictionary can be placed where it makes most sense, and that
%%   the protocol specification can be kept symbolic.
%%
%% - The structure can only represent trees.  However, sometimes you
%%   do want an acyclic graph structure.  E.g. for "classes".  There
%%   might be more reasons to have shared nodes later.
%%
%% - Any "classes" that appear in the directory tree (i.e. duplicated
%%   method interfaces), can be abstracted by exposing them under
%%   [type].  This allows two kinds of directory queries: shallow,
%%   which exposes those nodes as abstract types, and deep, which
%%   expands all methods.

%% This module provides functionality to binary to/from symbolic
%% mapping of the basic message protocol in Erlang, and also defines
%% some higher level protocols on top of this basic protocol.  Those
%% higher level protocols are defined completely symbolically.


-module(tag_u32).
-export([call/2, call/3, send/3,
         find/3, resolve/2,
         dir/3, dir/2, dir/1,
         foldl/3,
         dump/1,
         apply/3,
         mixin/3,
         %% Uses pid-to-path encoding.
         tag_u32/3,
         req_u32/3, req_u32_reply/3,
         %% Optionally expose low level code for debugging
         name_rev/2, type_rev/2

]).

-include("tag_u32.hrl").


%% 1. Generic RPC + Name Resolution
%% --------------------------------
%%
%% In this module we use reverse path notation, bottom-to-tom,
%% i.e. zipper or stack view, which is easier to work with.  TAG_U32
%% uses top-to-bottom order.
call_rev(Pid, RPath) ->
    call(Pid, lists:reverse(RPath)).

call(Pid, Path) ->
    call(Pid, Path, <<>>).

call(Pid, Path, Bin) ->
    %% FIXME: Later, use only resolved paths.
    NPath = resolve(Pid, Path),
    %% log:info("tag_u32:call ~p~n", [{Path,Bin}]),
    obj:call(Pid, {req_u32, NPath, Bin}, 2000).

%% This is mostly to plug into epid, so we do some automatic wrapping
%% here in case of single numbers.
send(Pid, Path, Num) when is_number(Num) ->
    send(Pid, Path, [Num]);
send(Pid, Path, Msg) when is_list(Msg) ->
    %% Just use synchronous calls for now.  It crashes earlier...
    Path1 = Path ++ Msg,
    %% log:info("tag_u32:send ~p~n", [Path1]),
    call(Pid, Path1).
    

%% Incremental resolve
find(Pid, Path, Atom) when is_atom(Atom) ->
    Bin = atom_to_binary(Atom,utf8),
    case call(Pid, Path ++ [?TAG_U32_CTRL,?TAG_U32_CTRL_NAME_ID], Bin) of
        {[0,Id],<<>>} -> {ok, Id};
        _ -> error
    end.
resolve(Pid, Path) ->
    resolve(Pid,[],Path).
resolve(_,_,[]) -> [];
resolve(Pid, Upper, [X|Rest]) ->
    Id = 
        if is_number(X) -> X;
           true ->
                case find(Pid, Upper, X) of
                    {ok, N} -> N;
                    error -> throw({tag_u32_resolve,Upper,[X|Rest]})
                end
        end,
    [Id|resolve(Pid, Upper++[Id], Rest)].


%% %% Resolve based on dictionary.
%% resolve(_Dict, []) -> [];
%% resolve(Dict, [Tag|Path]) ->
%%     case maps:find(Tag, Dict) of
%%         {ok, {N, Sub}} when is_map(Sub) ->
%%             [N | resolve(Sub, Path)];
%%         {ok, {N, Type}} when is_atom(Type) ->
%%             %% The remainder is opaque.
%%             %% FIXME: Check that it consists of integers?
%%             [N | Path]
%%     end.


%% Metadata commands
maybe_named_rev(Pid, [N|RPath0], Cmd) ->
    Ctrl = [N, Cmd, ?TAG_U32_CTRL | RPath0],
    case call_rev(Pid, Ctrl) of
        {[0], <<>>} ->
            %% Empty string means the node is there, but doesn't have
            %% a name.  So we return the number instead.
            {ok, N};
        {[0], Name} ->
            {ok, binary_to_atom(Name,utf8)};
        {[1], ErrorName} ->
            {error, binary_to_atom(ErrorName,utf8)}
    end.

%% Map identifier to name string.
name_rev(Pid, RPath) -> maybe_named_rev(Pid, RPath, ?TAG_U32_CTRL_ID_NAME).
type_rev(Pid, RPath) -> maybe_named_rev(Pid, RPath, ?TAG_U32_CTRL_ID_TYPE).
    
    

%% Retreive the whole dictionary.

%% Directory is represented as lists, where index in list corresponds
%% to the id used in the protocol.  The core routine has a logging
%% sink to observe the node sequence and perform some side effect.

%% Memoization is used to gather shared structure, which is also used
%% in fold to expand that structure again.

    
dir(Pid) ->
    dir(Pid,[]).
dir(Pid, Path) ->
    dir(Pid, Path, fun(_) -> ok end).
dir(Pid, Path, Log) ->
   Meta =
       case find(Pid, [], type) of
           {ok, Tag} ->
               {Dir,_} = dir(Pid, [Tag], Log, #{}),
               maps:from_list(Dir);
           _ ->
               #{}
       end,
    dir(Pid, Path, Log, Meta).

dir(Pid, Path, Log, Types) ->
    %% FIXME: Later support typed maps.  For now only one type is supported.
    Env = #{ pid => Pid, sink => Log, types => Types },
    {Rv, Meta} = dir_list_rev(Env, 0, lists:reverse(Path), #{}),
    Log(eof),
    {Rv, Meta}.


%% The server exposes a full tree that might contain duplicated
%% structure.  It can give hints that some of the substructure is
%% duplicated by providing interface types.

%% The intention here is to recurse into concrete maps and return the
%% substructure, but not recurse into those interface maps, and
%% instead to gather their substructure in Meta.

dir_list_rev(Env = #{pid := Pid, sink := Log, types := Types},
              N, RPath, Meta0) ->
    RPath1 = [N|RPath],
    MaybeName = name_rev(Pid,RPath1),
    case MaybeName of
        {error, bad_map_ref} ->
            %% It is a directory, but the subpath does not exist.
            {[], Meta0};
        {error, Error} ->
            throw({Error, RPath1, Error});
        {ok, Name} -> 
            {ok, Type} = type_rev(Pid,RPath1),
            Log({data,{RPath1,Name,Type}}),

            {SubDir, Meta2} =
                case Type of
                    map ->
                        dir_list_rev(Env, 0, RPath1, Meta0);
                    _ ->
                        SKey = {sub, Type},
                        case maps:is_key(SKey, Meta0) of
                            true ->
                                {Type, Meta0};
                            false ->
                                case maps:find(Type, Types) of
                                    {ok, sub} ->
                                        {Sub, Meta1} =
                                            dir_list_rev(Env, 0, RPath1, Meta0),
                                        {Type,
                                         maps:put(SKey, Sub, Meta1)};
                                    _ ->
                                        {Type, Meta0}
                                end
                        end
                end,

            %% The other leg recursively handles elements at this
            %% level.  Continue inside this directory.
            {RestDir, Meta3} = dir_list_rev(Env, N+1, RPath, Meta2),

            %% Return complete tree upstream.
            {[{Name,SubDir} | RestDir], Meta3}
    end.

%% Iterate over the path structure produced by dir/1.  The fold
%% routine is pure.  If bindings to the object are necessary this can
%% be hidden in the folded function.

%% Inner routine has path, represented as a stack (zipper).  It seems
%% more convenient to keep the symbolic and numeric stacks separate.

foldl(Fun, State, Node) ->
    foldl(Fun, State, Node, #{}).
foldl(Fun, State, Node, Meta) ->
    Env = #{ function => Fun, recursive => false, meta => Meta },
    foldl_env(Env, State, Node, [], []).

foldl_env(Env = #{ function  := Fun,
                   recursive := Recursive,
                   meta  := Meta },
          State, Node0, KeyStack, TagStack) ->

    %% Possibly expand substructure if it's in env.
    Node1 =
        case is_atom(Node0) of
            true ->
                case maps:find({sub,Node0}, Meta) of
                    {ok, Sub} -> Sub;
                    error -> Node0
                end;
            false ->
                Node0
        end,

    case Node1 of
        Type when is_atom(Type) ->
            %% A leaf node is an atom describing the type of the node.
            Fun({KeyStack,TagStack,Type},State);
        Dir ->
            State0 =
                case Recursive of
                    true  -> Fun({KeyStack,TagStack,map},State);
                    false -> State
                end,

            %% A directory is a list of atom to node pairs.  We
            %% perform a zip step here, moving the current key to the
            %% stack.
            lists:foldl(
              fun({Index,{Key1,Node2}}, State1) ->
                      foldl_env(Env, State1, Node2, [Key1|KeyStack], [Index|TagStack])
              end,
              State0,
              tools:enumerate(Dir))
    end.


%% 2. High level RPC protocols
%% ---------------------------

%% 2.1 Serialization
%% -----------------
%%
%% Given a directory tree, every node that has both get and set is
%% serializable.
%%
%% Two methods are provided here.  One performs get for all nodes that
%% expose a get method and collects the results in a dictionary.  This
%% serves as an example for using foldl/2.

dump(Pid) ->
    %% A deep directory is necessary to expose all abstract interface
    %% nodes.
    {Dir, Types} = dir(Pid), 
    dump(Pid, Dir, Types).
dump(Pid, Node, Types) ->
    Fun =
        fun(_Env={[set|_],RTags=[_|RTags1],_Type}, List) ->
                %% The corresponding set is on the same level.
                {[0],Bin} = call_rev(Pid, [get|RTags1]),
                %% Save it in the form that is needed to send back to
                %% the device.
                [{tag,{[],lists:reverse(RTags),Bin}} | List];
           (_, State) ->
                State
        end,
    lists:reverse(
      foldl(Fun, [], Node, Types)).



%% The corresponding load is implemented by exposing the file as a fold.
%% load(Fold) -> Fold(Fun,nostate) end.

    
%% 2.2 Object Instantiation
%% ------------------------
%%
%% This is uses for dataflow graph construction.  The input arguments are node IDs previously

%%
%% Note that this generalizes to any kind of object graph instantiation with the

%% FIXME: API is not stable.
apply(Pid, Proc, Nodes) ->
    {[Node],<<>>} = call(Pid,[class,Proc,apply] ++ Nodes),
    Node.





%% 3. Erlang mixin + low level tools
%% ---------------------------------

%% Mixing wrapper over the handle/2 form.
%% For now just ignore messages that are already handled.
mixin(true, _Msg, State) ->
    {false, State};
%% The inband error | State handle/2 protocol is easier to work with.
mixin(false, Msg, State) ->
    case handle(Msg, State) of
        error  -> {false, State};
        State1 -> {true, State1}
    end.



%% Communication mixin.  Used by gdbstub_hub, rai, ...

%% Messages coming from other Erlang processes.
handle(_Msg={Caller,{req_u32, U32List, Data}}, State) ->
    %% log:info("~p~n",[_Msg]),
    case 0 of
        0 ->
            %% Use pid-to-path encoding
            Bin = req_u32(Caller, U32List, Data),
            send_packet(Bin, State);
        1 ->
            %% Use broker encoding
            {Bin, State1} = req_u32_indirect(Caller, U32List, Data, State),
            send_packet(Bin, State1)
    end;

handle({Caller,{req_u32, U32List}}, State) ->
    handle({Caller,{req_u32, U32List, <<>>}}, State);


%% Originally this code came from gdbstub_hub, which probably needs to
%% be cleaned up and written in terms of this mixin.
handle({Port, {data, <<?TAG_U32:16, _NbFrom:8, NbArgs:8, Bin/binary>>=_Msg}}, 
       #{ port := Port } = State) ->
    %% FIXME: currently no support for RPC originating at other side.
    0 = _NbFrom, 
    BArgsLen = NbArgs*4,
    BArgs = binary:part(Bin, 0, BArgsLen),
    BTail = binary:part(Bin, BArgsLen, size(Bin)-BArgsLen),
    Args = [Arg || <<Arg:32>> <= BArgs],
    %% log:info("TAG_U32 ~p~n", [{Args,BTail}]),
    case Args of
        [?TAG_U32_INDIRECT,ID|Rest] ->
            req_u32_indirect_reply(ID, Rest, BTail, State);
        [Tag|Rest] ->
            case Tag >= 16#FFFFFF00 of
                %% pid-to-path addressing
                true ->
                    req_u32_reply(Tag, Rest, BTail);
                %% all the rest are interpreted as epid process names.
                false ->
                    epid:dispatch(Tag, Rest, State)
            end;
        _ ->
            ok
    end,
    State;

%% Use in-band error to signal to mixin/3 that handle/2 didn't handle.              
handle(_Msg,_State) ->
    error.

%% Send binary packet to the port.  Keep it simple for now and assume
%% that the port uses the correct protocol, and that we can access it
%% via 'port' tag.
send_packet(IOList, #{ port := Port } = State) ->
    %% I always forget... does this need to send the size?
    true = port_command(Port, IOList),
    %%Bin = iolist_to_binary(IOList),
    %%N = size(Bin),
    %%true = port_command(Port, [<<N:32>>,Bin]),
    State.
    
%% Later, abstract...

%% %% Send binary packet to the port.  We support direct function
%% %% dispatch (which preserves message order), and self re-queue in case
%% %% that is not available.
%% send_packet(Bin, State) ->
%%     case maps:find(send_packet, State) of
%%         {ok, SendPacket} ->
%%             SendPacket(Bin, State);
%%         error ->
%%             self() ! {send_packet, Bin},
%%             State
%%     end.



%% Two things about this:

%% - The TAG_U32 reply address mechanism is abstract, and uses the
%%   same addressing mechanism as all other messages: an array of u32
%%   bigendian ints.
%%
%% - Typically you would want a broker mechanism in the proxy that
%%   maps some u32 sequence to pids that are waiting for an RPC reply.
%%   However, we can just as well convert the pid to a binary term,
%%   and map that to an u32 array, such that we do not need to manage
%%   broker state.  This does assume that the other end is
%%   trustworthy.

req_u32(Pid, U32List, Data) ->
    {Nr, From} = term_to_binary_u32(Pid),
    Nr1 = Nr + 1,
    N = length(U32List),
    Tag = 16#FFF5,
    FromPid = 16#FFFFFF00 + Nr,
    [[<<Tag:16, Nr1, N, FromPid:32>>],
     From,
     [<<W:32>> || W<-U32List],
     Data].

req_u32_reply(Tag, Rest ,BTail) ->
    NbWords = Tag - 16#FFFFFF00,
    EncPid = lists:sublist(Rest, NbWords),
    EncPidBin = iolist_to_binary([<<W:32>> || W<-EncPid]),
    %% log:info("EncPidBin = ~p~n", [EncPidBin]),
    Pid = (catch binary_to_term(EncPidBin)),
    %% log:info("Pid = ~p~n",[Pid]),
    case is_pid(Pid) of
        true ->
            Rest1 = lists:sublist(Rest, NbWords+1, length(Rest)-NbWords),
            obj:reply(Pid, {Rest1,BTail}),
            ok;
        false ->
            %% What does it mean if this fails?  Sender is no longer
            %% available?  Just log.
            log:info("bad EncPidBin = ~p~n", [EncPidBin])
    end.
    

%% Map pid to u32 array for RPC reply addresses.
term_to_binary_u32(Term) ->
    EncPidBin = term_to_binary(Term),
    %% log:info("EncPidBin = ~p~n", [EncPidBin]),
    NbU32 = ((size(EncPidBin)-1) div 4)+1,
    {NbU32, [EncPidBin,binary:copy(<<0>>, 4*NbU32 - size(EncPidBin))]}.



%% Alternatively, an implementation that performs pid-to-index
%% translation.  This is a bit easier to debug since it doesn't
%% involve digging into binary messages, and it keeps a record of
%% calls that are in-flight.
req_u32_indirect(Pid, U32List, Data, State) ->
    ID = maps:get({tag_u32,next}, State, 0),
    State1 = maps:merge(
               State,
               #{{tag_u32,next} => ID+1,
                 {tag_u32,ID}   => Pid}),
    NbFromArgs = 2,
    From = <<?TAG_U32_INDIRECT:32, ID:32>>,
    NbArgs = length(U32List),
    Tag = ?TAG_U32,
    %% log:info("indirect ~p~n", [From]),
    IOL = [<<Tag:16, NbFromArgs, NbArgs>>,
           From,
           [<<W:32>> || W<-U32List],
           Data],
    {IOL,State1}.

req_u32_indirect_reply(ID, Rest, BTail, State) ->
    Pid = maps:get({tag_u32,ID}, State),
    obj:reply(Pid, {Rest,BTail}),
    maps:remove({tag_u32,ID}, State).
    

tag_u32(Tag,U32List,Data) ->
    Nr = 0, %% For RPC, later
    N = length(U32List),
    [[<<Tag:16, Nr, N>> | [<<W:32>> || W<-U32List]],
     Data].

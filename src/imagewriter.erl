%% FIXME: This can probably just be removed by adding epid interface
%% to erlcat?

-module(imagewriter).
-export([start_link/0, handle/2]).
start_link() ->
    {ok,
     serv:start(
       {handler,
        fun() -> #{} end,
        fun ?MODULE:handle/2})}.

%% Continuing on the idea of "connection" of epids: I want to model
%% writing an SD card image.  This is interesting because it has:

%% - practical significance.  This has resisted automation for a while
%% - network device management: where is the SD card located?
%% - it has a "commutation" property: what side does the decompressor run?

%% The big idea here is what I've been calling "commutation", as in
%% "commuting diagrams".  It is a pattern that is very common when
%% mapping a functional / data flow porgramming model to
%% implementation on a network.  The idea is that the links are not
%% free, so there is a cost associated with the path taken through the
%% network.

%% Interestingly, the epid approach makes this fairly explicit.  See
%% below

%% To give a practical example: lz4-compressed image file on one node,
%% sd card writer on another node.  In almost all cases the network
%% link is going to be more expensive than the decompression CPU load,
%% so it makes sense to decompress at the receiving end.



%% Another element is that we need to choose between a "push" or a
%% "pull" operation.  Because of the symmetry of the epid
%% representation, it is straightforard to convert one into the other!

%% In our case we decide to implement it as a push operation, so
%% translate the pull to a push.  See epid:push/2 and epid:pull/2 for
%% protocol.
%%
%% epid:send(Src, {push, Dst}) == epid:send(Dst, {pull, Src}).

handle({epid_send, DstSubId, {pull, Src}}, State) ->
    Dst = {epid, self(), DstSubId},
    _ = epid:send(Src, Dst),
    State;

handle({epid_send, {SrcType, SrcLoc} = _SrcSubId,
        {push, {epid, DstPid, {DstType,DstLoc} = _DstSubId} = _DstEpid}}, State) ->

    %% Commutation.

    %% The location of the decompressor is encoded in the source and
    %% destination types.  I.e. we convert the channel type to normal
    %% form such that we can just serve the file as is.
    ChanType = {SrcType, DstType},
    {image, DstType1} =
        case ChanType of
            {{image,Type}, device} -> {image, {device,Type}};
            {image,_} -> ChanType
        end,

    %% Perform the transfer in a process.
    TxId = {SrcLoc, DstPid, DstType1, DstLoc},
    Parent = self(),
    Transfer = spawn_link(fun() -> transfer(Parent, TxId) end),
    maps:put({transfer, Transfer}, TxId, State);


handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);
handle(Msg, State) ->
    info:log("~p~n",[Msg]),
    State.

%% Set up both ends, perform transfer, tear down.  Note that it might
%% actually be possible to just tranfer the file handle to the other
%% node, but we opt for binary tranfer here to have a more general
%% example

%% FIXME: Set up a generic unix data pipe with proper teardown over an
%% Erlang network.

transfer(_Parent, {_SrcLoc, _DstPid, _DstType1, _DstLoc} = TxId) ->
    log:set_info_name(TxId),
    ok.
    
%%             {image_lz4, _} -> "lz4 -d | dd of=%s";
%%            {image, _}     -> "dd of=%s"


    
%% The general problem is to set up a "socat pipe" through the Erlang
%% network.  So do this in socat.erl


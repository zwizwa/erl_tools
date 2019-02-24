-module(player).
-export([start_link/1, handle/2,
         ensure_index/2, ensure_index/1,
         take/2,
         convert/3,
         %% RPC calls
         spans/1, tree/1, lookup/2]).

%% Player for recorder.erl structures.
%%
%% Two levels of indexing are used:
%%
%% - Each chunk is accompanied by a flat file with packet offsets
%%   stored as 32/little.  The recorder can produce these, but if
%%   necessary we can re-generate.
%%
%% - Chunk spans are stored in a binary search tree in memory,
%%   reconstructed at startup.
%%

%% FIXME: With the index tree, some methods are no longer necessary.

start_link(Init = #{ dir := Dir, chunk := N }) ->
    {ok,
     serv:start(
       {handler,
        fun() -> 
                %% Keep the spans tree as a constant.  To rebuild it,
                %% restart the player.
                Spans = spans(Dir),
                Tree = tree(Spans),
                State = maps:put(tree, Tree, Init),
                %% Always have a file open.
                handle({open, N}, State)
        end,
        fun ?MODULE:handle/2})}.

 
handle({open, N}, #{ dir := Dir } = State) ->
    case maps:find(data, State) of
        {ok, OldDataFile} -> _ = file:close(OldDataFile);
        _ -> ok
    end,
    case maps:find(data, State) of
        {ok, OldIndexFile} -> _ = file:close(OldIndexFile);
        _ -> ok
    end,
    IndexFileName = Dir ++ recorder:num_to_filename(index, N),
    DataFileName = Dir ++ recorder:num_to_filename(data, N),
    {ok, IndexFile} = file:open(IndexFileName, [raw,read,binary]),
    {ok, DataFile} = file:open(DataFileName, [raw,read,binary]),
    %% log:info("playback: ~s~n", [FileName]),
    maps:merge(
      State,
      #{ chunk => N,
         index => IndexFile,
         data  => DataFile });

handle({Pid, next}, #{ data := DataFile, chunk := Chunk  }=State) ->
    case file:read(DataFile, 4) of
        {ok, <<Size:32>>} ->
            {ok, Bin}         = file:read(DataFile, Size),
            {ok, <<Size:32>>} = file:read(DataFile, 4), %% Assert
            obj:reply(Pid,{ok,binary_to_term(Bin)}),
            State;
        _ ->
           case try {ok, handle({open, Chunk+1}, State)}
                 catch C:E -> {C,E} end of
                {ok, State1} ->
                    handle({Pid, next}, State1);
                Error ->
                   obj:reply(Pid,{error,Error}),
                   State1 = handle({open, Chunk}, State),
                   handle(end_of_chunk, State1)
           end
    end;

handle({Pid, prev}, #{ data := DataFile, chunk := Chunk }=State) ->
    case file:position(DataFile, {cur,-4}) of
        {ok, SizePos} ->
            {ok, <<Size:32>>} = file:read(DataFile, 4),
            PrevPos           = SizePos - Size - 4,
            {ok, _}           = file:position(DataFile, PrevPos + 4),
            {ok, Bin}         = file:read(DataFile, Size), 
            {ok, <<Size:32>>} = file:read(DataFile, 4), %% Assert
            {ok, PrevPos}     = file:position(DataFile, PrevPos),
            obj:reply(Pid,{ok,binary_to_term(Bin)}),
            State;
        {error, einval} ->
            case try {ok, handle({open, Chunk-1}, State)}
                 catch C:E -> {C,E} end of
                {ok, State1} ->
                    State2 = handle(end_of_chunk, State1),
                    handle({Pid, prev}, State2);
                Error ->
                    obj:reply(Pid,{error,Error}),
                    handle({open, Chunk}, State)
            end
    end;

%% WARNING: Very slow.  See read_scan/1
%% If you want to do this, use the Rust mmap-based implementation.
handle({Pid, {sync, Chunk, Offset, Scan}}, State) ->
    State1 = handle({open, Chunk}, State),
    #{ data := DataFile } = State1,
    obj:reply(
      Pid, 
      case read_scan(DataFile, Offset, Offset + Scan) of
          {ok, Bin} -> {ok, binary_to_term(Bin)};
          Error -> Error
      end),
    State1;

handle({Pid, position}, #{ data := DataFile, chunk := N }=State) ->
    {ok,Pos} = file:position(DataFile, cur),
    obj:reply(Pid, {N,Pos}),
    State;

handle(end_of_chunk, #{ data := DataFile }=State) ->
    {ok, _} = file:position(DataFile, eof),
    State;

handle(beginning_of_chunk, #{ data := DataFile }=State) ->
    {ok, _} = file:position(DataFile, 0),
    State;

handle({Pid, span}, #{ tree := Tree }=State) ->
    obj:reply(Pid,tree_span(Tree)),
    State;

            

%% Indexed reference.  Perform seek and retrieval in one atomic
%% operation.  Two levels are supported: global and curent chunk.
handle({Pid, {ref, {C, N}}},
       #{ chunk := Chunk }=State) ->
    case C of
        Chunk ->
            handle({Pid, {ref_current, N}}, State);
        _ ->
            try
                State1 = handle({open, C}, State),
                handle({Pid, {ref_current, N}}, State1)
            catch
                _:_ ->
                    obj:reply(Pid, {error, {bad_index, {C,N}}}),
                    handle({open, Chunk}, State)
            end
    end;
handle({Pid, {ref, N}},
       #{ tree := Tree }=State) when is_number(N) ->
    case lookup(N, Tree) of
        {ok, {C, {Start,_}}} ->
            handle({Pid, {ref, {C, N - Start}}}, State);
        error ->
            obj:reply(Pid, error), State
    end;
    
handle({Pid, {ref_current, N}},
       #{ index := IndexFile,
          chunk := Chunk,
          data  := DataFile}=State) when is_number(N)->
    try
        {ok, _} = file:position(IndexFile, N*4),
        {ok, <<Offset:32/little>>} = file:read(IndexFile, 4),
        {ok, Offset} = file:position(DataFile, Offset),
        handle({Pid, next}, State)
    catch
        _:_ -> 
            obj:reply(Pid, {error, bad_index, {Chunk, N}})
    end,
    State;

%% Debug.
handle(Msg, State) ->
    obj:handle(Msg,State).
   


take(_Pid, N) when N =< 0 -> [];
take(Pid, N) ->
    {ok, Msg} = obj:call(Pid, next),
    [Msg | take(Pid, N-1)].
    
    



%% FIXME: This seems to work in practice, but it is very inefficient.
%% See rawlog.rs: it's very efficient, but still can be ambiguous.
%% That same module has index rebuild as well, and the recorder is
%% currently generating index data during recording, so it is better
%% to assume that index data is available.
read_scan(DataFile, Offset, OffsetEndx) ->
    case (Offset > OffsetEndx) of
        true -> error;
        false ->
            case read_verify(DataFile, Offset) of
                error -> read_scan(DataFile, Offset+1, OffsetEndx);
                {ok,_}=OK -> OK
            end
    end.
read_verify(DataFile, Pos) ->
    file:position(DataFile, Pos),
    {ok, <<Size:32>>} = file:read(DataFile, 4),
    case {file:position(DataFile, Pos+4+Size),
          file:read(DataFile, 4)} of
        {{ok, _},{ok, <<0,0,0,0>>}} ->
            %% Case is too common
            error;
        {{ok, _},{ok, <<Size:32>>}}  ->
            %% We were able to read both size fields and they make
            %% sense.  Only then read the data.
            {ok, _}   = file:position(DataFile, Pos + 4),
            {ok, Bin} = file:read(DataFile, Size),
            {ok, _}   = file:position(DataFile, Pos + 4 + Size + 4),
            {ok, Bin};
        _ ->
            error
    end.



%% Re-generate index for a data file.
%% This is implemented in Rust.  Figure out how to make this indirection optional.
%% FIXME: This is still staged in exo.  Move rust module into erl_tools.
ensure_index(Dir, N) when is_number(N) ->
    Data  = iolist_to_binary([Dir,recorder:num_to_filename(data, N)]),
    Index = iolist_to_binary([Dir,recorder:num_to_filename(index, N)]),
    
    case filelib:is_regular(Index) of
        true  ->
            %% log:info("got: ~s~n", [Index]),
            {ok, already_have};
        false -> 
            log:info("gen: ~s~n", [Index]),
            exo_rs:call({rawlog_save_index_u32,{Data,Index}})
    end.

ensure_index(Dir) ->
    lists:foreach(
      fun(N) -> ensure_index(Dir, N) end,
      recorder:dir_chunks(Dir)).


%% Second level index structure to allow flat integer references.

%% 1. Get spans list from index chunk sizes
spans(Dir) ->              
    Chunks = recorder:dir_chunks(Dir),
    tl(
      lists:reverse(
        lists:foldl(
          fun(C,[{_,{_,Endx}}|_]=Acc) ->
                  IndexFile = recorder:num_to_filename(index, C),
                  Size = filelib:file_size(Dir ++ IndexFile),
                  N = Size div 4,
                  [{C, {Endx,N+Endx}}|Acc]
          end,
          [{no_chunk, {no_start, 0}}],
          Chunks))).

%% 2. Convert that into a binary tree
tree([Span1]) -> {leaf, Span1};
tree(List) -> 
    N = length(List) div 2,
    {A,B} = lists:split(N, List),
    {fork,
     {range(A),tree(A)},
     {range(B),tree(B)}}.
range(List) ->
    {_,{Start,_}} = hd(List),
    {_,{_,Endx}}  = lists:last(List),
    {Start,Endx}.

%% 3. Use the tree to perform lookups.
lookup(N, {fork,
           {{Left,_},   TLeft},
           {{Mid,Right},TRight}}) ->
    if (Left =< N) and (N < Mid)   -> lookup(N, TLeft);
       (Mid  =< N) and (N < Right) -> lookup(N, TRight);
       true -> error
    end;
lookup(_, {leaf, Span1}) -> 
    {ok, Span1}.

tree_span(Tree) ->
    case Tree of
        {leaf,{_,Span}} -> Span;
        {fork, {{Start,_},_}, {{_,Endx},_}} -> {Start,Endx}
    end.






%% Convert a textual "~999p~n" log to binary.
%% Write it as a task since it might run for a while.

%% Note: my log has a bunch of messages that are spread over multiple
%% lines.  I don't need them, so this just logs them as errors.

%% This is also going to be ridiculously slow.

convert(InFile,OutFile,N) ->
    {ok, I} = file:open(InFile,[binary,read]),
    {ok, O} = file:open(OutFile,[raw,append,delayed_write]),
    convert(I,O,N,[]).
convert(_, _, 0, _) -> ok;
convert(I, O, N, Stack) ->
    case file:read_line(I) of
        {ok, Line} -> 
            Term = 
                try
                    type_base:decode({pterm, Line})
                catch 
                    _:_ -> {error, Line}
                end,
            Bin = term_to_binary(Term),
            Size = size(Bin),
            Packet = [<<Size:32>>,Bin,<<Size:32>>],
            case file:write(O, Packet) of
                ok -> convert(I, O, N-1, Stack);
                E -> E
            end;
        E ->
            file:close(I),
            file:close(O),
            E
    end.






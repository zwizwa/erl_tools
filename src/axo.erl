-module(axo).
-export([start_link/1, handle/2, call/1, call/2]).
-define(MEM_TYPE_HINT_LARGE,(1 bsl 17)).



%% Axo protocol:
%% https://github.com/JohannesTaelman/axrai/blob/master/firmware/pconnection.c

%% #define tx_hdr_acknowledge 0x416F7841   // "AxoA"
%% #define tx_hdr_fwid        0x566f7841   // "AxoV"
%% #define tx_hdr_log         0x546F7841   // "AxoT"
%% #define tx_hdr_memrd       0x726f7841   // "Axor"
%% #define tx_hdr_patch_paramchange 0x71507841   // "AxPq"
%% #define tx_hdr_patch_disp  0x64507841   // "AxPd"
%% #define tx_hdr_patch_list  0x6C507841   // "AxPl"
%% #define tx_hdr_f_info      0x69467841   // "AxFi"
%% #define tx_hdr_f_read      0x72467841   // "AxFr"
%% #define tx_hdr_f_dir       0x64467841   // "AxFd"
%% #define tx_hdr_f_dir_end   0x44467841   // "AxFD"
%% #define tx_hdr_f_result    0x65467841   // "AxFe"
%% #define tx_hdr_result_ptr  0x536F7841   // "AxoS"
%% #define tx_hdr_cstring     0x736F7841   // "Axos"

%% #define rcv_hdr_ping                 0x706f7841 // "Axop"
%% #define rcv_hdr_getfwid              0x566f7841 // "AxoV"

%% #define rcv_hdr_mem_read             0x724d7841 // "AxMr"
%% #define rcv_hdr_mem_write            0x774d7841 // "AxMw"
%% #define rcv_hdr_mem_alloc            0x614d7841 // "AxMa"
%% #define rcv_hdr_mem_free             0x664d7841 // "AxMf"
%% #define rcv_hdr_mem_write_flash      0x464d7841 // "AxMF"

%% #define rcv_hdr_patch_stop           0x53507841 // "AxPS"
%% #define rcv_hdr_patch_start          0x73507841 // "AxPs"
%% #define rcv_hdr_patch_get_disp       0x64507841 // "AxPd"
%% #define rcv_hdr_patch_preset_apply   0x54507841 // "AxPT"
%% #define rcv_hdr_patch_preset_write   0x52507841 // "AxPR"
%% #define rcv_hdr_patch_get_name       0x6E507841 // "AxPn"
%% #define rcv_hdr_patch_get_list       0x6C507841 // "AxPl"
%% #define rcv_hdr_patch_get_error      0x65507841 // "AxPe"

%% #define rcv_hdr_patch                0x61507841 // "AxPa"

%% #define rcv_hdr_virtual_input_event  0x426f7841 // "AxoB"
%% #define rcv_hdr_midi                 0x4D6f7841 // "AxoM"
%% #define rcv_hdr_activate_dfu         0x446f7841 // "AxoD"
%% #define rcv_hdr_extra                0x586f7841 // "AxoX" temporary messages, for development purposes only!

%% #define rcv_hdr_f_open               0x6f467841 // "AxFo"
%% #define rcv_hdr_f_open_write         0x4f467841 // "AxFO"
%% #define rcv_hdr_f_close              0x63467841 // "AxFc"
%% #define rcv_hdr_f_seek               0x73467841 // "AxFs"
%% #define rcv_hdr_f_read               0x72467841 // "AxFr"
%% #define rcv_hdr_f_write              0x77467841 // "AxFw"
%% #define rcv_hdr_f_dirlist            0x64467841 // "AxFd"
%% #define rcv_hdr_f_getinfo            0x69467841 // "AxFi"
%% #define rcv_hdr_f_setinfo            0x49467841 // "AxFI"
%% #define rcv_hdr_f_delete             0x52467841 // "AxFR"
%% #define rcv_hdr_f_mkdir              0x6D467841 // "AxFm"



%% pconnection_mem.c rcv_mem_alloc

call(Cmd) ->
    [Pid|_] = axo_hub:pids(exo:need(axo_hub)),
    call(Pid, Cmd).

call(Pid, {mem_alloc, Size}) ->
    call(Pid, {mem_alloc, Size, 0, 4});


%% typedef enum {
%%   mem_type_can_execute   = (1<< 2),
%%   mem_type_hint_no_dma   = (1<<16),
%%   mem_type_hint_large    = (1<<17),
%%   mem_type_hint_tiny_dma = (1<<18),
%% } mem_type_flags_t;
call(Pid, {mem_alloc, Size, TypeFlags, Alignment}=_Cmd) ->
    log:info("~p~n",[_Cmd]),
    {ok, <<Addr:32/little>>} =
        call_u32(Pid, <<"AxoS">>, <<"AxMa">>, [Size, TypeFlags, Alignment]),
    {ok, Addr};

call(Pid, {mem_write, Offset, Data}) ->
    Size = size(Data),
    %% Write doesn't reply.  Also it sends two packets: one with
    %% header, the other with payload.
    Pid ! {send, [<<"AxMw">>, <<Offset:32/little, Size:32/little>>]},
    Pid ! {send, Data},
    %% FIXME: used another call to synchronize.
    %% call(Pid, {mem_read, Offset, Size});
    ok;

%% axo:call({mem_read, 537002088, 4}).

call(Pid, {mem_read, OffsetIn, SizeIn}) ->
    {ok, <<OffsetOut:32/little,
           SizeOut:32/little,
           Data/binary>>} =
        call_u32(Pid, <<"Axor">>, <<"AxMr">>, [OffsetIn, SizeIn]),
    Rv = {ok, #{ offset => OffsetOut, size => SizeOut,
                 data => Data}},
    %% log:info("mem_read -> ~p~n", [Rv]),
    Rv;

call(Pid, fwid) ->
    {ok, Bin} = call_u32(Pid, <<"AxoV">>, <<"AxoV">>, []),
    <<A,B,C,D,
      Crc:32/little,
      ChunkAddr:32/little>> = Bin,
    {ok, #{ver => {A,B,C,D},
           crc => Crc,
           chunkaddr => ChunkAddr}};

%% <0.3171.0>: AxoA: <<1,0,0,0,1,0,0,0,0,0,0,0,217,5,163,11,255,255,255,255,0,0,0,
%%                     0,70,44,12,89,18,39,17,87,0,0,0,0,0,0,0,0,0,0,0,0,240,239,
%%                     0,0,248,255,0,0,248,191,0,0,248,255,127,0>>

%% <0.3171.0>: AxoT: <<"exception report:\npc=0x8004754\npsr=0x61000000\nlr=0x1000E4F0">>
%% <0.3171.0>: AxoT: <<"\nr12=0x0\ncfsr=0x8200\nbfar=0xBD10B004\n">>


call(Pid, ping) ->
    call_u32(Pid, <<"AxoA">>, <<"Axop">>, []);

%% Start a patch, see:
%% axrai/src/main/java/axoloti/connection/USBBulkConnection_v2.java
%% -> transmitStartLive()
%% axo:call({transmit_start_live, 
call(Pid, start_xpatch) ->
    call(Pid, {transmit_start_live, "/i/exo/axrai/build/xpatch.elf", "xpatch"});
call(Pid, load_xpatch) ->
    call(Pid, {load_elf, "/i/exo/axrai/build/xpatch.elf", "xpatch"});

call(Pid, {load_elf, Elf, PatchName}) ->
    {ok, ElfBin} = file:read_file(Elf),
    Size = size(ElfBin),
    {ok, Addr} = call(Pid, {mem_alloc, Size, ?MEM_TYPE_HINT_LARGE, 16}),
    log:info("loading ELF at ~8.16.0B~n", [Addr]),
    ok = call(Pid, {mem_write, Addr, ElfBin}),
    {ok, #{ data := ElfBinVerify}} = call(Pid, {mem_read, Addr, Size}),
    ElfBin = ElfBinVerify,

    %% threadLoadPatch: name is assumed to be
    %% eg. "@12345678:patchname" where 0x12345678 is the memory
    %% address of a memory-mapped file
    Name = tools:format_binary("@~8.16.0B:~s",[Addr,PatchName]),
    log:info("Name: ~s\n", [Name]),
    {ok, Name};

call(Pid, {transmit_start_live, Elf, PatchName}) ->
    {ok, Name} = call(Pid, {load_elf, Elf, PatchName}),
    call(Pid, {patch_start, Name});

call(Pid, {patch_start, PatchName}) ->
    %% Note that is the first character in PatchName is null, then the
    %% format is different and contains a patch index.
    %% call_raw(Pid, <<"AxoS">>, [<<"AxPs">>, PatchName]).
    Pid ! {send, [<<"AxPs">>, PatchName]}.



call_u32(Pid, ReplyTag, Tag, Ints) when is_binary(ReplyTag) ->
    log:info("call_u32:~p~n",[{Pid,ReplyTag,Tag,Ints}]),
    Rv = call_raw(Pid, ReplyTag, [Tag, [<<I:32/little>> || I<-Ints]]),
    %% log:info("-> ~p~n",[Rv]),
    Rv.

call_raw(Pid, ReplyTag, RawMsg) when is_binary(ReplyTag) ->
    obj:call(Pid, {call, ReplyTag, RawMsg}, 1000).

%% We are started by axo_hub.  It will provide the correct config to
%% start the axo_connect.elf binary driver.
start_link(Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Port = 
                    tools:spawn_port(
                      Config,
                      {"axo_connect.elf", []},
                      [use_stdio, binary, exit_status, {packet,4}]),
                State = maps:merge(
                          Config,
                          #{ port => Port }),
                State
        end,
        fun ?MODULE:handle/2})}.

handle({send, RawMsg}, #{ port := Port}=State) ->
    Port ! {self(), {command, RawMsg}},
    State;

handle({Pid, {call, ReplyTag, RawMsg}}, State) when is_binary(ReplyTag) ->
    State1 = maps:put({waiting, ReplyTag}, Pid, State),
    handle({send, RawMsg}, State1);

handle({Port, {data, <<Tag:32/little, Data/binary>>}}, #{ port := Port} = State) ->
    %% All tags are human-readable, so it seems easier to keep them
    %% like that in the Erlang dispatch code.
    TagBin = <<Tag:32/little>>,
    case maps:find({waiting, TagBin}, State) of
        {ok, Pid} ->
            obj:reply(Pid, {ok, Data}),
            maps:remove({waiting, TagBin}, State);
        _ ->
            {Fmt,Terms} =
                case {TagBin,Data} of
                    {<<$A,$x,_,_>>,_} ->
                        %% log:info("~8.16.0B: ~p~n",[Tag,Data]),
                        {"~s: ~p~n", [TagBin,Data]};
                    _ ->
                        {"untagged: ~p bytes\n", [size(Data)]}
                end,
            log:info(Fmt,Terms),
            State
    end;

        
handle(Msg, State) ->
    obj:handle(Msg, State).



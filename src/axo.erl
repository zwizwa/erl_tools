-module(axo).
-export([start_link/1, handle/2, call/1, call/2]).



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

call(Pid, {alloc, Size}) ->
    TypeFlags = 0, Alignment = 4,
    {ok, <<Addr:32/little>>} =
        call_u32(Pid, <<"AxoS">>, <<"AxMa">>, [Size, TypeFlags, Alignment]),
    {ok, Addr};

call(Pid, version) ->
    {ok, Bin} = call_u32(Pid, <<"AxoV">>, <<"AxoV">>, []),
    <<A,B,C,D,
      Crc:32/little,
      ChunkAddr:32/little>> = Bin,
    {ok, #{ver => {A,B,C,D},
           crc => Crc,
           chunkaddr => ChunkAddr}}.

call_u32(Pid, ReplyTag, Tag, Ints) when is_binary(Tag) ->
    obj:call(Pid, {call, ReplyTag, Tag, [<<I:32/little>> || I<-Ints]}, 1000).

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



handle({send, IOL}, #{ port := Port}=State) ->
    Port ! {self(), {command, IOL}},
    State;

handle({Pid, {call, ReplyTag, Tag, IOL}}, State) ->
    State1 = maps:put({waiting, ReplyTag}, Pid, State),
    handle({send, [Tag | IOL]}, State1);

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
                    _ ->
                        %% log:info("~8.16.0B: ~p~n",[Tag,Data]),
                        {"~s: ~p~n", [TagBin,Data]}
                end,
            log:info(Fmt,Terms),
            State
    end;

        
handle(Msg, State) ->
    obj:handle(Msg, State).



-module(gdbstub).
-export([%% Functionality built on top of RSP
         dev/1,
         csv_call/4, rsp_call/2,
         rcmd/2,mem_hex/3,word/2,sword/2,set_mem/3,
         flash_erase/2,flash_erase/3,flash_write/3,
         flash_erase_write_page/3,flash_erase_write/3,
         string/3,stringp/4,
         reset/1,
         uid/1,
         %% uc_tools gdbstub code
         config/1,company/1,product/1,serial/1,firmware/1,version/1,
         config_start/0,protocol/1,protocol2/1]).

%% All binding goes through dev/1.  Devices identified with a process
%% id are supposed to support the {rsp_call,_} protocol.  Otherwise,
%% devices can be identified indirectly by referring to a {Hub,DevID}
%% pair.  The hub needs to support {dev_pid,_} call.

dev(Pid) when is_pid(Pid) -> Pid;
dev({hub,Hub,DevID}) -> 
    {ok, Pid} = obj:call(Hub, {dev_pid, DevID}),
    Pid;
dev(Any) ->
    dev({hub,gdbstub_hub,Any}).

          
%% Below, all device references are abstract.

rsp_call(ID, Packet) ->
    obj:call(dev(ID), {rsp_call, Packet}).

csv_call(D, C, A) -> csv_call(D, C, A, []).
csv_call(ID, Code, Args, Payload) ->
    Packet = rsp:hex_csv(Code, Args, Payload),
    Length = length(Packet),
    case Length =< 16#c0 of
        true ->
            Reply = rsp_call(ID, Packet),
            rsp:unwrap(Reply);
        false ->
            exit({rsp_packet_size,
                  {ID, Code, Args, Payload},
                  Packet, Length})
    end.



rcmd(ID,Cmd) ->
    Hex = csv_call(ID, "qRcmd," ++ tools:hex(Cmd), []),
    tools:unhex(Hex).

mem_hex(_, _, 0) -> [];
mem_hex(ID, Addr, Size) when Size =< 64 ->
    csv_call(ID, "m", [Addr,Size]);
mem_hex(ID, Addr, Size) ->
    mem_hex(ID, Addr, 64) ++ mem_hex(ID, Addr + 64, Size - 64).

mem(ID, Addr, Size) ->
    tools:unhex(mem_hex(ID, Addr, Size)).
word(ID, Addr) ->
    <<Word:32/little>> = list_to_binary(mem(ID, Addr, 4)),
    Word.
sword(ID, Addr) ->
    <<Word:32/signed-little>> = list_to_binary(mem(ID, Addr, 4)),
    Word.

%% Write to target memory. gdbstub.c uses 32bit access when possible,
%% which is necessary for some register access. See cmd_set_memory()
%% in gdbstub.c
set_mem(ID, Addr, Values) ->
    _ = csv_call(ID, "M", [Addr,string:len(Values)], tools:hex(Values)), ok.
set_word(ID, Addr, Value) ->
    _ = csv_call(ID, "M", [Addr,4], tools:hex_u32(Value)), ok.


%% FIXME: page size depends on device!
%% x8    : 1024
%% xC,xD : 2048
-define(STM32_PAGE_SIZE,2048).

%% Write to / erase target Flash memory.
flash_erase(ID, Addr) ->
    PageSize = ?STM32_PAGE_SIZE,
    0 = Addr rem PageSize,
    flash_erase(ID, Addr, PageSize).
flash_erase(ID, Addr, Size) ->
    "OK" = csv_call(ID, "vFlashErase:", [Addr, Size]),  ok.
flash_write(ID, Addr, Bin) when is_binary(Bin) ->
    flash_write(ID, Addr, binary_to_list(Bin));
flash_write(ID, Addr, List) when is_list(List) ->
    %% STM32 write unit is 16 bytes
    case {Addr rem 2, length(List) rem 2,
          length(List) < ?STM32_PAGE_SIZE} of
        {0,0,true} ->
            "OK" = csv_call(ID, "vFlashWrite:", [Addr], List), ok;
        _ ->
            exit({flash_write_align, ID, Addr, List})
    end.

flash_erase_write_page(ID, Addr, List) when is_list(List) ->
    %% Overhead: 26 = length(rsp:hex_csv("vFlashWrite:",[16#FFFFFFFF],"")).
    %% Worst case encoding: 2 bytes per byte.
    %% Buf size: c0.
    %% (16#c0 - 26) div 2 = 83
    %% Could be done more efficiently but it doesn't seem to be a problem
    flash_erase(ID, Addr),
    Stride = 82,
    lists:foreach(
      fun({N,Chunk}) -> flash_write(ID, Addr + N*Stride, Chunk) end,
      tools:enumerate(tools:chunks(Stride, List))).

flash_erase_write(ID, Addr, InList) when is_list(InList) ->
    List = case length(InList) rem 2 of
               0 -> InList;
               1 -> InList ++ [0]
           end,
    Stride = ?STM32_PAGE_SIZE,
    lists:foreach(
      fun({N,Chunk}) -> flash_erase_write_page(ID, Addr + N*Stride, Chunk) end,
      tools:enumerate(tools:chunks(Stride, List))).
    
    

%% Check for erased Flash blocks and NULL pointers.
pointer_check(16#FFFFFFFF) -> erased;
pointer_check(16#00000000) -> null;
pointer_check(P) -> {ok, P}.

%% Read a C string from device's memory.
string(ID, Addr, N) ->
    tools:strunk(mem(ID, Addr, N)).
%% Dereference pointer to string, then read if valid.
stringp(ID, Addr, N, Default) ->
    case pointer_check(word(ID, Addr)) of 
        {ok, StrAddr} -> string(ID, StrAddr, N);
        _  -> Default
    end.

%% Reset CM3 through SCB_AIRCR.  
reset(ID) -> set_word(ID, 16#E000ED0C, 16#05FA0004). 

%% See gdbstub_api.h for struct gdbstub_config layout.
config(Word)  -> 16#8002800 + 4*Word.
company(ID)   -> stringp(ID, config(0), 20, "unknown").
product(ID)   -> stringp(ID, config(1), 40, "unknown").
serial(ID)    -> stringp(ID, config(2), 10, "unknown").
firmware(ID)  -> stringp(ID, config(8), 80, "unknown").
version(ID)   -> stringp(ID, config(9), 80, "unknown").

protocol_(ID)   -> stringp(ID, config(13), 80, "unknown").
protocol2_(ID)  -> stringp(ID, config(14), 80, "unknown").
    
config_start() -> config(3). 

uid(ID)        -> mem_hex(ID, 16#1FFFF7E8, 12).


protocol(ID) -> parse_protocol(protocol_(ID)).
protocol2(ID) -> parse_protocol(protocol2_(ID)).
    
parse_protocol(Str) ->
    try
        type:decode({pterm,list_to_binary(Str)})
    catch _:_ ->
            {error, Str}
    end.

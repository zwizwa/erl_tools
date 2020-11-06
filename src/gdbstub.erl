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
         elf_sections/1, elf_mem_top/2, mem_ld/1,
         elf2config/1,
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
    %% log:info("rsp_call: C: ~p~n", [Packet]),
    Rv = obj:call(dev(ID), {rsp_call, Packet}, 6011),
    %% log:info("rsp_call: R: ~p~n", [Rv]),
    Rv.

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



%% Elf access.
%% There are two straightforward ways: 1) objdump, 2) load .elf and ask gdb.
%% gdbstub:elf_sections("/home/tom/exo/deps/uc_tools/gdb/lab_board.x8.f103.elf").

%% $ arm-none-eabi-objdump -h lab_board.x8.f103.elf 
%%
%% lab_board.x8.f103.elf:     file format elf32-littlearm
%%
%% Sections:
%% Idx Name          Size      VMA       LMA       File off  Algn
%%   0 .text         00000c64  08003000  08003000  00003000  2**3
%%                   CONTENTS, ALLOC, LOAD, READONLY, CODE
%%   1 .data         00000004  20002000  08003c64  0000a000  2**2
%%                   CONTENTS, ALLOC, LOAD, DATA
%% ...

elf_sections(File) ->
    %% This is very ad-hoc, but I really don't want to go through a C library.
    Report = os:cmd("arm-none-eabi-objdump -h " ++ File),
    [_,_,_,_,_|Tail] = re:split(Report,"\\n",[notempty,trim]),
    maps:from_list(elf_sections_tail(Tail)).

elf_sections_tail([]) -> [];
elf_sections_tail([Row,_Flags|Rest]) -> 
    [elf_sections_row(Row) | elf_sections_tail(Rest)].

elf_sections_row(Row) ->    
    [_,_,Name,Size,VMA,LMA|_] = re:split(Row,"\\ *",[notempty,trim]),
    {Name,{Size,VMA,LMA}}.

%% Returns something like this, together with sections we don't care
%% about.
%%
%% #{<<".config">> => {<<"000000dc">>,<<"08002800">>,<<"08002800">>},
%%   <<".text">>   => {<<"00000c64">>,<<"08003000">>,<<"08003000">>},
%%   <<".data">>   => {<<"00000004">>,<<"20002000">>,<<"08003c64">>},
%%   <<".bss">>    => {<<"00001264">>,<<"20002004">>,<<"08003c68">>},
%%

%% Now the objective is to find RAM and Flash regions that can be used
%% to construct a linker script for new code.
%%
%% Since we control the linker file that generated this, we can make
%% some assumptions: .bss SIZE and VMA give RAM top.  .bss LMA is
%% meaningless, but it is set to the correct value of the next free
%% Flash byte.  We round that up to the next erase block.

to_integer_hex(H) ->
    list_to_integer(binary_to_list(H),16).

elf_mem_top(Elf, FlashBS) ->
    #{ <<".bss">> := {HexRamAddr, HexRamSize, HexFlashAddr} }
        = elf_sections(Elf),
    [RamAddr,RamSize,FlashAddr] =
        [to_integer_hex(H)
         || H <- [HexRamAddr, HexRamSize, HexFlashAddr]],
    RamBlock = tools:n_div(RamAddr + RamSize, 4),  %% 32-bit align
    RamTop = 4 * RamBlock,
    FlashBS = 4096,
    FlashBlock = tools:n_div(FlashAddr, FlashBS),
    FlashTop = FlashBlock * FlashBS,
    #{ rom => FlashTop, ram => RamTop}.

mem_ld(#{ rom := FlashTop, ram := RamTop }) ->    
    Script = 
        %% We don't know length, so set it to something large so it
        %% won't trigger linker errors.
        tools:format(
          "MEMORY {\n"
          "        rom (rx)  : ORIGIN = 0x~8.16.0B, LENGTH = 0xFFFFFFFF\n"
          "        ram (rwx) : ORIGIN = 0x~8.16.0B, LENGTH = 0xFFFFFFFF\n"
          "}\n",
          [FlashTop,RamTop]),
    Script.



%% Second attempt at something simpler.  What I want is to be able to
%% load the firmware config block from an elf file.  It seems simplest
%% to not have the build system create .bin files, but to just dump
%% them when they are needed.

%% Currently hardcoded to ARM + assume it is in path.
elf2config_bin(Elf) ->
    Tmp = lib:nonl(os:cmd("mktemp")),
    Cmd = tools:format(
            "arm-none-eabi-objcopy -j .config -O binary '~s' '~s'",
            [Elf, Tmp]),
    {ok, []} = run:script_output(Cmd, infinity),
    %% log:info("Cmd = ~p~n",[Cmd]),
    {ok, Bin} = file:read_file(Tmp),
    file:delete(Tmp),
    {ok, Bin}.
               

%% Parse the config block from ELF file.
elf2config(Elf) ->
    %% First get the absolute address
    #{ <<".config">> := {_, Config_, _} } = elf_sections(Elf),
    Config = to_integer_hex(Config_),
    %% Then parse the rest from the binary dump of the file.
    {ok, Bin} = elf2config_bin(Elf),
    << C0:32/little,  C1:32/little, _C2:32/little, _C3:32/little,
      _C4:32/little, _C5:32/little, _C6:32/little, _C7:32/little,
       C8:32/little,  C9:32/little,
      _/binary>> = Bin,
    %% FIXME: Get this from the elf using routines above.
    Cstring = fun(Abs) -> cstring(Bin, Abs - Config) end,
    #{company  => Cstring(C0),
      product  => Cstring(C1),
      firmware => Cstring(C8),
      version  => Cstring(C9)}.


cstring(Bin, Offset) ->
    case binary:at(Bin, Offset) of
        0 -> [];
        Char -> [Char | cstring(Bin, Offset+1)]
    end.
            

    
    
    

-module(zip_stream).
-export([test/0]).

%% Bare-bones streaming zip file creation.

%% TODO:
%% - bit 3 of flags to allow for CRC and size append

%% Generalize this later.
cmd({data, Bytes}=_Msg,
    #{ iol    := IOL,
       offset := Offset } = State) when
      is_binary(Bytes) ->
    log:info("~p~n",[_Msg]),
    maps:merge(
      State,
      #{ iol    => [IOL,Bytes],
         offset => Offset + size(Bytes) });

cmd({file, #{ data := Data }=Info}, State) ->
    State1 =
        cmd({local_file_header,
             maps:merge(
               Info,
               #{ size => size(Data),
                  crc32 => erlang:crc32(Data)})},
            State),
    cmd({data, Data}, State1);
           

cmd({local_file_header, 
     #{ name   := Name,
        size   := Size } = FileInfo},
    #{ offset := Offset,
       files  := Files } = State) when
      is_binary(Name) ->
    Files1 =
      maps:put(
        Name,
        #{ size   => Size,
           offset => Offset },
        Files),
    Header = local_file_header(FileInfo),
    State1 = 
        maps:merge(
          State,
          #{ files => Files1 }),
    cmd({data, Header}, State1);

cmd(central_directory,
    #{ files := Files, offset := CDROffset} = State) ->
    FilesL = maps:to_list(Files),
    State1 = #{ offset := CDREndx } =
        lists:foldl(
          fun({File, #{ size   := Size,
                        offset := FileOffset }}, S) ->
                  CDR = 
                      central_directory_record(
                        #{ name => File,
                           size => Size,
                           offset => FileOffset }),
                  cmd({data, CDR}, S)
          end,
          State,
          FilesL),
    EOCD = end_of_central_directory(
          #{ nb_entries => length(FilesL),
             offset => CDROffset, 
             size => CDREndx - CDROffset }),
    cmd({data, EOCD}, State1).



%% https://users.cs.jmu.edu/buchhofp/forensics/formats/pkzip.html




test() ->
    Bin = <<"123\n">>,
    # { iol := IOL }
        = lists:foldl(
            fun cmd/2,
            #{ offset => 0, files => #{}, iol => [] },
            [{file, #{ name => <<"test1.txt">>, data => Bin }}
            ,{file, #{ name => <<"test2.txt">>, data => Bin }}
            ,central_directory
            ]),
    file:write_file("/tmp/test.zip", IOL).

end_of_central_directory(
  #{ nb_entries := TotalEntries,
     offset := CentralDirOffset, 
     size := CentralDirSize }) ->
    <<16#504b0506: 32, %% Sig
      0: 16/little, %% Disk
      0: 16/little, %% DiskCD
      TotalEntries: 16/little, %% DiskEntries
      TotalEntries: 16/little, %% TotalEntries
      CentralDirSize: 32/little,
      CentralDirOffset: 32/little,
      0: 16/little>>. %% CommentLen

central_directory_record(
  #{ name   := FileName,
     size   := Size,
     offset := LocalHeaderOffset 
   } = Info) ->
    CRC32 = maps:get(crc32, Info, 0),
    ModTime = maps:get(mod_time, Info, 0),
    ModDate = maps:get(mod_date, Info, 0),
    FileNameLen = size(FileName),
    <<16#504b0102: 32, %% Sig
      20: 16/little, %% Ver
      20: 16/little, %% VerNeed
      0: 16/little, %% Flags
      0: 16/little, %% Comp
      ModTime: 16/little,
      ModDate: 16/little,
      CRC32: 32/little,
      Size: 32/little, %% USize
      Size: 32/little, %% CSize
      FileNameLen: 16/little,
      0: 16/little, %% ExtraFieldLen
      0: 16/little, %% FileCommLen
      0: 16/little, %% DiskStart
      0: 16/little, %% InternalAttr
      0: 32/little, %% ExternalAttr
      LocalHeaderOffset: 32/little,
      FileName/binary>>.

local_file_header(
  #{ name := FileName,
     size := Size } = Info) when 
      is_binary(FileName) ->
    CRC32 = maps:get(crc32, Info, 0),
    ModTime = maps:get(mod_time, Info, 0),
    ModDate = maps:get(mod_date, Info, 0),
    FileNameLen = size(FileName),
    <<16#504b0304:32, %% Sig
      20:16/little, %% Ver
      0:16/little, %% Flags
      0:16/little, %% Compression
      ModTime:16/little,
      ModDate:16/little,
      CRC32:32/little, %% CRC32
      Size:32/little, %% CSize
      Size:32/little, %% USize
      FileNameLen:16/little,
      0:16/little, %% ExtraFieldLen
      FileName/binary>>.
     
     


      
      

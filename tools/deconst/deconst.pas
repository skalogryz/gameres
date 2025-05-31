program deconst;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, zlib, helpzlib;


var
  gCommand  : string = '';
  gOffset   : UInt64 = 0;
  gHasOfs   : Boolean = false;
  gFileName : string = '';
  gSize     : Uint64 = 0;
  gShowoutput : Boolean = false;
  gWriteAnyway : Boolean = false;

procedure ErrorOffset(const ofsStr: string);
begin
  writeln(StdErr, 'Invalid offset value: "', ofsStr,'"');
  writeln(StdErr, 'Use either decimal of hexidemical ($FE, 0xFE) format' );
  Halt(1);
end;

procedure ErrorNoFile;
begin
  writeln(StdErr, 'Input file is not specified' );
  Halt(1);
end;

procedure ErrorNoOffset;
begin
  writeln(StdErr, 'Offset is not specified' );
  Halt(1);
end;

function SafeParamStr(i: integer; const def: string=''):string;
begin
  if (i<0) or (i>ParamCount) then Result:=def
  else Result:=PAramStr(i);
end;

procedure ParseParams;
var
  i : integer;
  s : string;
  err: integer;
begin
  i:=1;
  while i<=ParamCount do begin
    s:=ParamStr(i);
    inc(i);
    if Pos('-', s)=1 then begin
      s:=AnsiLowerCase(s);
      if (s = '-size') or (s='-sz') then begin
        Val(SafeParamStr(i), gSize, err);
        writeln('gsize = ',gsize);
        inc(i);
      end else if ( s= '-stdout') then begin
        gShowoutput:=true;
      end else if (s='-force') then begin
        gWriteAnyway:= true;
      end;
    end else begin
      if gCommand ='' then
        gCommand:=s
      else if not ghasOfs then begin
        if Pos('0x', s)=1 then s:='$'+Copy(s, 3, length(s));
        Val(s, gOffset, err);
        if err<>0 then begin
          ErrorOffset(ParamStr(i-1));
          Exit;
        end;
        ghasOfs:=true;
      end else if gFileName='' then
        gFileName:=s;
    end;
  end;
end;

procedure PrintHelp;
begin
  writeln('deconst %command% [%options%] %offset% %filename%');
  writeln('  %offset% can be specified as decimal 0xhex or $hex');
  writeln('commands:');
  writeln('  uz  - attempts to decomparess zlib streat at offset');
  writeln('options');
  writeln(' -stdout - write the unpacked data to stdout');
  writeln(' -force  - write the unpacked data even if an error occured');
  writeln(' -sz (-size) - the size of the data to be read from the offset for unpack');
end;

function zlibErrorToStr(err: integer): string;
begin
  case err of
   Z_OK            :  Result:='Z_OK';
   Z_STREAM_END    :  Result:='Z_STREAM_END';
   Z_NEED_DICT     :  Result:='Z_NEED_DICT';
   Z_ERRNO         :  Result:='Z_ERRNO';
   Z_STREAM_ERROR  :  Result:='Z_STREAM_ERROR';
   Z_DATA_ERROR    :  Result:='Z_DATA_ERROR';
   Z_MEM_ERROR     :  Result:='Z_MEM_ERROR';
   Z_BUF_ERROR     :  Result:='Z_BUF_ERROR';
   Z_VERSION_ERROR :  Result:='Z_VERSION_ERROR';
  else
    Result:='unknown';
  end;
end;



procedure UnzlibStream(st: TStream; amaxsize: uint64; showOutput: Boolean; dstSt: TSTream);
var
  buf : array of byte;
  dst : array of byte;
  dstl : LongWord;
  p   : Int64;
  sz  : integer;
  res : integer;
begin
  SetLength(Buf, 1024*1024);
  SetLength(dst, 1024*1024*4);
  writeln('reading!');
  sz:=length(Buf);
  if (amaxsize>0) and (sz>amaxsize) then sz:=amaxsize;
  p := st.Position;
  sz:=st.Read(buf[0], sz);
  writeln('data read: ', sz,' first byte is: ', IntToHex(buf[0],2),'h');

  dstl:=length(dst);
  res:=zuncompress (@dst[0], dstl, buf, sz);
  writeln('uncomparess = ',res, ' ',zlibErrorToStr(res));

  if (res <> Z_OK)
     and (buf[0] = $78)
     and (sz > 1) and (isGzHeader(buf[0], buf[1])) then begin
    writeln('the start bit seems to be Zlib Magic header ',IntToHex(buf[0],2),IntToHex(buf[1],2),'h ',ZlibMagicComment(buf[1]));
    writeln('trying to uncompress by skipping it (new offset is: ',IntToHex(p+2,8),'h):');
    sz:=zuncompress (@dst[0], dstl, PByteArray(@buf[2])^, sz-2);
  end;

  writeln(' len = ',dstl);
  if (res = Z_OK) or (gWriteAnyway) then begin
    if showOutput then
      writeln(' ',PCHar(@dst[0]));
    if dst<>nil then
      dstSt.Write(dst[0], dstl);
  end;




end;

procedure Unzlib(const fn: string; ofs, maxSize: UInt64);
var
  fs : TfileStream;
  dst : TFileStream;
begin
  fs:=TFileStream.create(fn, fmOpenRead or fmShareDenyNone);
  dst := TFileStream.Create(fn+'.deconst',fmCreate);
  try
    fs.Position:=ofs;
    UnzlibStream(fs, maxSize, gShowoutput, dst);
  finally
    fs.Free;
    dst.free;
  end;
end;

procedure PrintInfo;
begin
  writeln('hexer: 0.0.0');
  writeln('zlib:  ', zlibVersion);
end;

begin
  ParseParams;
  if gCommand='' then begin
    PrintHelp;
    Exit;
  end;

  if gCommand='info' then begin
    Printinfo;
    Exit;
  end;

  if not ghasOfs then begin
    ErrorNoOffset;
    Exit;
  end;

  if gFileName='' then begin
    ErrorNoFile;
    exit;
  end;

  if gCommand='uz' then begin
    writeln('zlib decompress (inflate) at: ', gOffset,' ',Format('%x', [gOffset]));
    Unzlib(gFileName, gOffset, gSize);
  end;
end.



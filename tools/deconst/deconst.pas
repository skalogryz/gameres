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
      end else if ( s= '-stdout') then
        gShowoutput:=true;
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
  writeln('deconst %command% %offset% %filename%');
  writeln('commands:');
  writeln(' uz  - attempts to decomparess zlib streat at offset');
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

procedure UnzlibStream(st: TStream; amaxsize: uint64; showOutput: Boolean);
var
  buf : array of byte;
  dst : array of byte;
  dstl : LongWord;
  sz  : integer;
begin
  SetLength(Buf, 1024*1024);
  SetLength(dst, 1024*1024*4);
  writeln('reading!');
  sz:=length(Buf);
  if (amaxsize>0) and (sz>amaxsize) then sz:=amaxsize;
  sz:=st.Read(buf[0], sz);
  writeln('data read: ', sz,' first byte is: ', IntToHex(buf[0],2));

  dstl:=length(dst);
  sz:=zuncompress (@dst[0], dstl, buf, sz);
  writeln('uncomparess = ',sz, ' ',zlibErrorToStr(sz));
  writeln(' len = ',dstl);
  if (sz = Z_OK) and showOutput then
    writeln(' ',PCHar(@dst[0]));
end;

procedure Unzlib(const fn: string; ofs, maxSize: UInt64);
var
  fs : TfileStream;
begin
  fs:=TFileStream.create(fn, fmOpenRead or fmShareDenyNone);
  try
    fs.Position:=ofs;
    UnzlibStream(fs, maxSize, gShowoutput);
  finally
    fs.Free;
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



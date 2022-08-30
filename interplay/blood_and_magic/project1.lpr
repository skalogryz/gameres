program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, bmfile
  { you can add units after this };

procedure RunFile(const fn: string);
var
  s: TSTFFile;
  m: TSTFMap;
  i   : integer;
  j   : integer;
  fs  : TFileStream;
begin
  s:=TSTFFile.Create;
  m:=TSTFMap.Create;
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    ReadMAPStream(fs, m);
    writeln('m.cnt=',m.cnt);
    for i:=0 to m.cnt-1 do begin

      {write(m.ent[i].tp,' ', m.ent[i].id:13, IntToHex(m.ent[i].id,8):12,m.ent[i].offset:13,inTtoheX(m.ent[i].offset,8):13);
      if i<m.cnt-1 then
        write(' size: ',m.ent[i+1].offset-m.ent[i].offset);
      writeln;}
      write(i,': ');
      for j := 0 to 15 do
        write( IntToHex(m.ent[i].buf[j], 2),' ');
      writeln;
    end;
  finally
    fs.Free;
    m.Free;
    s.Free;
  end;
end;

function GetExtByType(const tp: integer): string;
begin
  case tp of
    TYPE_PIC:  Result := '.pic';
    TYPE_MIDI: Result := '.hmp';
    TYPE_WAVE: Result := '.wav';
    TYPE_PAL:  Result := '.pal';
    TYPE_FONT: Result := '.font';
    TYPE_TEXT: Result := '.txt';
    TYPE_ANMLIB: Result := '.bnk';
  else
    Result := '';
  end;
end;


procedure ExtractFiles(const mapfn, stffn: string);
var
  s: TSTFFile;
  m: TSTFMap;
  i   : integer;
  fs  : TFileStream;
  d   : TFileStream;
  sz  : integer;
  hdr : TInSTFFileHeader;

begin
  s:=TSTFFile.Create;
  m:=TSTFMap.Create;
  fs:=TFileStream.Create(mapfn, fmOpenRead or fmShareDenyNone);
  try
    ReadMAPStream(fs, m);
  finally
    fs.Free;
  end;
  fs:=TFileStream.Create(stffn, fmOpenRead or fmShareDenyNone);
  try
    fs.Position:=0;
    for i:=0 to m.cnt-1 do begin
      if (i<m.cnt-1) then
        sz := m.ent[i+1].offset-fs.Position
      else
        sz := fs.Size - fs.Position;
      writeln('sz=',sz);

      dec(sz, sizeof(TInSTFFileHeader));
      d := TFileStream.Create('file_'+IntToStr(i)+'_'+IntToStr(m.ent[i].tp)+'.hdr', fmCreate);
      try
        fs.Read(hdr, sizeof(hdr));
        d.Write(hdr, sizeof(hdr));
      finally
        d.Free;
      end;

      if (hdr.extrasz>0) then begin
        dec(sz, hdr.extrasz);
        d := TFileStream.Create('file_'+IntToStr(i)+'_'+IntToStr(m.ent[i].tp)+'.ext', fmCreate);
        try
          d.CopyFrom(fs, hdr.extrasz);
        finally
          d.Free;
        end;
      end;


      d := TFileStream.Create('file_'+IntToStr(i)+'_'+IntToStr(m.ent[i].tp)+GetExtByType(m.ent[i].tp), fmCreate);
      try
        d.CopyFrom(fs, sz);
      finally
        d.Free;
      end;
    end;
  finally
    fs.Free;
    m.Free;
    s.Free;
  end;
end;


procedure RunFileHeader(const fn: string; extractFile: Boolean = false; extractExtra: Boolean = false);
var
  fs  : TFileStream;
  h   : TInSTFFileHeader;
  dst : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    while fs.Position < fs.Size do begin
      fs.Read(h, sizeof(h));

      write('id: ', h.id,'; tp: ', h.tp);
      write('; size: ', h.size1,' (',h.size2,'); extra: ', h.extrasz);
      writeln;

      if extractExtra and (h.extrasz>0) then begin
        dst := TFileStream.Create(IntToStr(h.id)+'.'+IntToStr(h.tp)+'.extra', fmCreate);
        try
          dst.CopyFrom(fs, h.extrasz);
        finally
          dst.Free;
        end;
      end else
        fs.Position:=fs.Position+h.extrasz;

      if extractFile and (h.size1>0) then begin
        dst := TFileStream.Create(IntToStr(h.id)+'.'+IntToStr(h.tp), fmCreate);
        try
          dst.CopyFrom(fs, h.size1);
        finally
          dst.Free;
        end;
      end else
        fs.Position:=fs.Position+h.size1;
    end;
  finally
    fs.Free;
  end;
end;

procedure RunImageHeader(const fn: string);
var
  fs : TFileStream;
  h  : TImageHeader;
  i  : integer;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    i:=1;
    while fs.Position < fs.Size do begin
      fs.Read(h, sizeof(h));
      writeln('frame: ',i);
      writeln('  ofs:  ', h.ofsx,' ',h.ofsy,' flag: ', h.flags);
      writeln('  size: ', h.width,' ',h.height);
      writeln('  time: ', h.time);
      inc(i);
    end;
  finally
    fs.Free;
  end;
end;

var
  fn  : string;
  ext : string;
begin
  if ParamCount=0 then begin
    writeln('please specify .stf or .map file name');
    exit;
  end;
  fn := ParamStr(1);
  ext := UpperCase(ExtractFileExt(fn));
  if (ext = '.MAP') then begin
    RunFile(fn);
    ExtractFiles(fn, ChangeFileExt(fn, '.stf'));
  end
  //RunFileHeader(ParamStr(1));
  else
    RunImageHeader(fn);
end.


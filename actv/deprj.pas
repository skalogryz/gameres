program unzfs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, Math, prjfile, helpzlib, zlibwin
  { you can add units after this };

procedure ReadMap(const fn: string);
var
  fs  : TFileStream;
  m   : TMap;
  i   : integer;
  sm  : integer;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    m := prjfile.ReadMap(fs);
    writeln('files count: ', m.header.filesCount);
    sm := 0;
    for i:=0 to m.header.typesCount-1 do begin
      sm := sm+m.types[i].count;
    end;
    writeln('files sum: ', sm);
  finally
    fs.Free;
  end;
end;

procedure UnpackFiles(const fn: string; doUnpack: Boolean);
var
  hdr : TPrjFileHeader;
  fs  : TFileStream;
  p   : Int64;
  i   : integer;
  mem : TMemoryStream;
  res : array of byte;
  l   : longWord;
  outfn : string;
  dst : TFileStream;
  tp  : string;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    i := 0;
    while (fs.Position < fs.Size) do begin
      fs.Read(hdr, sizeof(hdr));
      writeln(hdr.typeName, ' ',IntToHex(hdr.unk0, 4),' ',hdr.realSize);
      if (hdr.packSize <= 0) then begin
        writeln('unexpected pack size: ',hdr.packSize);
        Break;
      end;
      p := fs.Position;
      if (doUnpack) then begin
        fs.Position:=fs.Position+2;
        mem := TMemoryStream.Create;
        try
          mem.CopyFrom(fs, hdr.packSize-2);
          SetLength(res, hdr.realSize);
          l := hdr.realSize;
          if (zuncompress(@res[0], l, PByteArray(mem.Memory)^, mem.Size))=0 then begin
            outfn :='extract'+IntTostr(i)+hdr.ext;
            dst := TFileStream.Create(outfn, fmCreate);
            try
              dst.Write(res[0], l);;
            finally
              dst.Free;
            end;
          end;
        finally
          mem.Free;
        end;
      end;
      fs.Position:=p+hdr.packSize;
      inc(i);
    end;

  finally
    fs.Free;
  end;
end;

procedure UnpackFilesWithMap(const mapFn: string; const prjFn: string);
var
  mp  : TMap;
  fs  : TFileStream;
  dst : TFileStream;
  hdr : TPrjFileHeader;
  outfn : string;
  i     : integer;
  mem   : TMemoryStream;
  res   : array of byte;
  l     : longword;
begin
  mp:=nil;
  try
    fs := TFileStream.Create(mapFn, fmOpenRead or fmShareDenyNone);
    try
      mp := prjfile.ReadMap(fs);
    finally
      fs.free;
    end;
    fs := TFileStream.Create(prjFn, fmOpenRead or fmShareDenyNone);
    try
      for i := 0 to mp.header.filesCount-1 do begin
        outfn := mp.files[i].name+'.'+mp.files[i].ext;
        fs.Position := mp.files[i].offset;
        fs.Read(hdr, sizeof(hdr));
        fs.Position:=fs.Position+2;
        mem := TMemoryStream.Create;
        try
          mem.CopyFrom(fs, hdr.packSize-2);
          SetLength(res, hdr.realSize);
          l := hdr.realSize;
          writeln(i,'/',mp.header.filesCount,': ', outfn);
          if (zuncompress(@res[0], l, PByteArray(mem.Memory)^, mem.Size))=0 then begin
            dst := TFileStream.Create(outfn, fmCreate);
            try
              dst.Write(res[0], l);;
            finally
              dst.Free;
            end;
          end;
        finally
          mem.Free;
        end;
      end;
    finally
      fs.free;
    end;
  finally
    mp.Free;
  end;
end;


procedure SmartFiles(const fn: string; doUnpack: boolean);
var
  x : string;
  prjFile : string;
  mapFile : string;
  hasMap : boolean;
  hasPrj : boolean;
begin
  mapFile := '';
  prjFile := '';
  x := ExtractFileExt(fn);
  x := LowerCase(x);
  if (x ='.prj') then begin
    mapFile := ChangeFileExt(fn,'.map');
    prjFile := fn;
  end else if (x='.map') then begin
    mapFile := fn;
    prjFile := ChangeFileExt(fn,'.prj');
  end;
  hasMap := (mapFile <>'') and FileExists(mapFile);
  hasPrj := (prjFile <>'') and FileExists(prjFile);

  if hasPrj and not hasMap then
    UnpackFiles(prjFile, doUnpack)
  else if hasMap and not hasPrj then
    ReadMap(mapFile)
  else if hasMap and hasPrj and doUnpack then begin
    UnpackFilesWithMap(mapfile, prjFile);
  end else if hasMap and hasPrj then begin
    ReadMap(mapFile);
    UnpackFiles(prjFile, false);
  end;
end;

var
  fn : string;
begin
    if ParamCount=0 then begin
     writeln('please provide input res file name');
     exit;
  end;
  fn := Paramstr(1);
  //UnpackFiles(fn, ParamCount>1);
  SmartFiles(fn, ParamCount>1);
end.


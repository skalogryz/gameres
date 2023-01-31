program skiftest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes, skifstorfile
  { you can add units after this };

procedure DoExtract(const fn: string; const info : TStorFileInfo);
var
  fs  : TFileStream;
  i   : integer;
  nm  : string;
  dst : TFileStream;
  dir : string;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    for i := 0 to info.count-1 do begin
      fs.Position := info.fileinfo[i].offset;
      nm := info.names[i];

      dir := ExtractFileDir(nm);
      if dir <> '' then ForceDirectories( ExtractFileDir(nm));
      dst := TFileStream.Create(nm, fmCreate);
      try
        dst.CopyFrom(fs, info.fileinfo[i].size);
      finally
        dst.Free;
      end;
    end;
  finally
    fs.Free;
  end;
end;

var
  info : TStorFileInfo;
  i    : integer;
  doEx : Boolean;
begin
  if ParamCOunt=0 then begin
    writeln('please provide file name');
    exit;
  end;
  doEx := ParamCount>2;
  if not ReadFile(ParamStr(1), info) then begin
    writeln('failed to read the file');
    exit;
  end;
  writeln('count: ', info.count);
  for i:=0 to info.count-1 do begin
    writeln(info.fileinfo[i].size:10,': ',info.names[i]);
  end;
  if doEx then
    DoExtract(ParamStr(1), info);
end.


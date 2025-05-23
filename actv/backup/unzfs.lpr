program unzfs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, zfsfile
  { you can add units after this };

procedure UnpackFiles(const fn: string; doUnpack: Boolean);
var
  hdr : Theader;
  fs  : TFileStream;
  i   : integer;
  fe  : TFileEntry;
  p   : int64;
  d   : TfileStream;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    fs.Read(hdr, sizeof(hdr));
    writeln('header: ', hdr.id);
    writeln('files:  ', hdr.count);
    for i:=0 to hdr.Count-1 do begin
      fs.Read(fe, sizeof(fe));
      writeln(fe.name);
      writeln('  ofs: ',  fe.ofs);
      writeln('  sz:  ',  fe.size);
      writeln('  f0:  ',  fe.f0);
      writeln('  f4:  ',  fe.f4);
      writeln('  f6:  ',  fe.f6);
      if doUnpack then begin
         p :=fs.Position;

         fs.Position := fe.ofs;
         d:=TFileStream.CreatE(fe.Name, fmCreate);
         try
           d.CopyFrom(fs, fs.Size);
         finally
           d.Free;
         end;

         fs.Position := p;
      end;
    end;

  finally
    fs.Free;
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
  UnpackFiles(fn, ParamCount>1);
end.


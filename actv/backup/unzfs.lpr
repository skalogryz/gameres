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
  nextfs : integer;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    fs.Read(hdr, sizeof(hdr));
    writeln('header: ', hdr.id);
    writeln('files:  ', hdr.count);
    nextfs := 0;
    for i:=0 to hdr.Count-1 do begin
      fs.Read(fe, sizeof(fe));


      if ((fe.seqnum mod 100) = 0) then nextfs := fe.f0;
      if ((fe.seqnum mod 100) = 99) then fs.Position := nextfs;

      writeln(fe.name);


      writeln('  ofs: ',  fe.ofs);
      writeln('  sz:  ',  fe.size);
      writeln('  f0:  ',  fe.f0,' ', IntToHex(f0,8));
      writeln('  seq: ',  fe.seqnum);
      writeln('  f6:  ',  fe.f6,' ', IntToHex(f6,8));
      if doUnpack then begin
         p :=fs.Position;

         fs.Position := fe.ofs;
         d:=TFileStream.CreatE(fe.Name, fmCreate);
         try
           d.CopyFrom(fs, fe.Size);
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


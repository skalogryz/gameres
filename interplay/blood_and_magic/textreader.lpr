program textreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, bmfile
  { you can add units after this };

var
  fs : TFileStream;
  mp : TTextMap;
  i  : integer;
begin
  if ParamCount=0 then begin
    writeln('please specify extracted (from .STF file) .text file to read');
    exit;
  end;
  fs := TFileStream.Create(ParamSTr(1), fmOpenRead or fmShareDenyNone);
  try
    ReadTextMap(fs, mp);
    writeln('lines: ', mp.count);
    for i:=0 to mp.count-1 do begin
      write(i,': ');
      write('[',mp.entries[i].textIdx,']: ');
      writeln(mp.texts[i]);
    end;
  finally
    fs.Free;
  end;
end.


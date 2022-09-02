program tilereader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, bmfile
  { you can add units after this };

procedure ParseTiles(const fn: string);
var
  fs : TFileStream;
  h  : TTileHeader;
  i  : integer;
  t  : TTileInfo;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    fs.Read(h, sizeof(h));
    for i:=0 to h.count2-1 do begin
      fs.Read(t, sizeof(t));
      write(t.name);
      write(',',t.flag1);
      write(',',t.flag2);
      write(',',t.flag3);
      write(',',t.flag4);
      write(',',t.flag5);
      write(',',t.flag6);
      write(',',t.flag7);
      write(',',t.iconId,' ',IntToHex(t.iconId, 8));

      writeln;
    end;
    writeln(fs.Size-fs.Position);

  finally
    fs.Free;
  end;
end;

begin
  if ParamCount=0 then begin
    writeln('please specify extract tile file (18)');
    exit;
  end;
  ParseTiles(ParamStr(1));
end.


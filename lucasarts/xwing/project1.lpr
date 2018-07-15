program project1;

uses xwingfiles, sysutils, classes;

procedure OutputFile(const brf: TBrfFile);
var
  i : integer;
begin
  writeln('id = ', brf.header.id);
  writeln('units    = ', brf.header.units);
  //writeln('messages = ', brf.header.msgs);
  for i:=0 to brf.header.units-1 do begin
    writeln(brf.unitName[i].name);
  end;

end;

var
  fs :TFileStream;
  f : TBrfFile;
begin
  if ParamCount=0 then begin
    writeln('please specify file');
    exit;
  end;
  fs :=TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone);
  try
    ReadBrf(fs, f);
    OutputFile(f);
    writeln(IntTohex(fs.Position,8));
  finally
    fs.Free;
  end;
end.


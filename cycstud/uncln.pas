{$mode delphi}
uses SysUtils, Classes,
  clnfile;

procedure UnpackFiles(const fn: string; doExtract: boolean);
var
  fs    : TfileStream;
  list  : TList;
  fe    : TFileEntry;
  i     : integer;
  dst   : TFileStream;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  list  := TList.Create;
  try
    Readfiles(fs, list);
    for i:=0 to list.Count-1 do begin
      fe := TFileEntry(list[i]);
      writeln(fe.fileName);
      if doextract and (fe.info.packsize <> ATTR_DIR) and (fe.info.offset > 0) then begin
        ForceDirectories(ExtractFileDir(fe.fileName));
        dst:= TFileStream.Create( fe.fileName,  fmCreate);
        try
          ExtractFile(fs, fe, dst);
        finally
          dst.Free;
        end;
      end;
    end;
  finally
    for i:=0 to list.count-1 do
      Tobject(list[i]).Free;
    fs.free;
    list.Free;
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

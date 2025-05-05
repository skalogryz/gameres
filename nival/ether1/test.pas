{$mode delphi}
uses SysUtils, Classes, ether1res;

function StrFromArr(const ch: array of char; ofs, len: integer): string;
var
  s : string;
begin
  SetLength(s, len);
  Move(ch[ofs], s[1], len);
  result := s;
end;

procedure UnpackFiles(const fn: string; doExtract: boolean);
var
  fs    : TfileStream;
  hdr   : TResHeader;
  ent   : array of TEntryInfo;
  nmbuf : array of char;
  i     : integer;
  nm    : string;
  op    : TFileStream;
  dir   : string;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  fs.Read(hdr, sizeof(hdr));  
  writeln('count:    ', hdr.count);
  writeln('entyofs:  ', hdr.entryofs);
  writeln('namesize: ', hdr.namesz);

  fs.Position := hdr.entryofs;
  SetLength(ent, hdr.count);
  SetLength(nmbuf, hdr.namesz);
  fs.read(ent[0], hdr.count*sizeof(TEntryInfo));
  fs.read(nmbuf[0], length(nmbuf));
  for i:=0 to hdr.Count-1 do begin
    nm := StrFromArr(nmbuf, ent[i].nameofs, ent[i].namelen);
    writeln( nm );
    if (doextract) then begin 
      dir := ExtractFileDir(nm);
      if (dir<>'') and (not DirectoryExists(dir)) then 
        ForceDirectories(dir);
  
      op :=TFileStream.Create(nm, fmCreate);
      try
        fs.Position := ent[i].offset;
        op.CopyFrom(fs, ent[i].size);
      finally
        op.Free;
      end;
    end;
  end;

  fs.free;
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
unit skifstorfile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStorFileEntry = packed record
    nmofs  : longword;
    offset : longword;
    size   : longword;
  end;

  TStorFileInfo = record
    count    : integer;
    fileinfo : array of TStorFileEntry;
    namesBuf : array of char;
    names    : array of PChar;
  end;

const
  ID_StOR = 'StOR';

function ReadStream(const s: TFileStream; var fi: TStorFileInfo): Boolean;
function ReadFile(const fn: string; var fi: TStorFileInfo): Boolean;

implementation

function ReadFile(const fn: string; var fi: TStorFileInfo): Boolean;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    Result := ReadStream(fs, fi);
  finally
    fs.Free;
  end;
end;

function ReadStream(const s: TFileStream; var fi: TStorFileInfo): Boolean;
var
  id  : array [0..3] of char;
  ofs : longword;
  i,j : integer;
begin
  fi.count := 0;
  s.Read(id, sizeof(id));
  if (id <> ID_StOR) then begin
    Result := false;
    Exit;
  end;
  ofs := s.ReadDWord;
  Result := ofs <= s.Size;
  if not Result then Exit;
  s.Position := ofs;
  fi.count := s.ReadDWord;
  SetLength(fi.fileinfo,  fi.count);
  s.Read(fi.fileinfo[0], fi.count * sizeof(TStorFileEntry));
  SetLength(fi.namesBuf, s.Size - s.Position);
  s.Read(fi.namesBuf[0], length(fi.namesBuf));

  SetLength(fi.names, fi.count);
  j:=0;
  for i:=0 to fi.count-1 do begin
    if j>=length(fi.namesBuf) then fi.names[i]:=nil
    else begin
      fi.names[i]:=@fi.namesBuf[j];
      while (j<=length(fi.namesBuf)) and (fi.namesBuf[j] <> #0) do inc(j);
      inc(j);
    end;
  end;
end;

end.


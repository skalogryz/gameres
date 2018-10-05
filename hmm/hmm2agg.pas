unit hmm2agg;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

// Based on the description from https://thaddeus002.github.io/fheroes2-WoT/infos/informations.html

uses
  Classes, SysUtils;

type
  THMM2FileInfo = packed record
    fileid  : LongWord;
    offset  : LongWord;
    size    : LongWord;
  end;

  THMM2FileName = packed record
    name : array [0..12] of ansichar;
    pad  : word;
  end;

type

  { THHM2AggFile }

  THHM2AggFile = class(TObject)
  public
    count : Word;
    info  : array of THMM2FileInfo;
    name  : array of THMM2FileName;
  end;

procedure HMM2ReadStream(asrc: TStream; dst: THHM2AggFile);
function HMM2ReadFile(const fn: string; dst: THHM2AggFile): Boolean;
function GetName(nm: THMM2FileName): string;

procedure Dump(agg: THHM2AggFile);

implementation

{ TAggFile }

procedure HMM2ReadStream(asrc: TStream; dst: THHM2AggFile);
begin
  dst.count := asrc.ReadWord;
  SetLength(dst.info, dst.count);
  SetLength(dst.name, dst.count);
  asrc.Read(dst.info[0], dst.count * sizeof(THMM2FileInfo));

  asrc.Position := asrc.Size - Int64(dst.count) * sizeof(THMM2FileName);
  asrc.Read(dst.name[0], dst.count * sizeof(THMM2FileName));
end;

function HMM2ReadFile(const fn: string; dst: THHM2AggFile): Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      HMM2ReadStream(fs, dst);
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

procedure Dump(agg: THHM2AggFile);
var
  i : integer;
begin
  writeln('count ', agg.count);
  writeln('size':10,'offset':10,' name');
  for i := 0 to agg.count-1 do begin
    writeln(agg.info[i].offset:10,agg.info[i].size:10,' ', GetName(agg.name[i]));
  end;
end;

function GetName(nm: THMM2FileName): string;
var
  i : integer;
begin
  for i := 0 to length(nm.name)-1 do
    if (nm.name[i] = #0) then begin
      SetLength(Result, i);
      if i > 0 then Move(nm.name[0], Result[1], i);
      Exit;
    end;
  Result := nm.name;
end;

end.


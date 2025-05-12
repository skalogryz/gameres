unit ether1res;

{$mode delphi}

interface

uses Classes, SysUtils;

type
  TResHeader = packed record
    id       : array [0..3] of byte; // 3C E2 9C 01 - eng
                                     // 3D E2 9C 01 - rus
    count    : integer; // total files count
    entryofs : integer; 
    namesz   : integer;
  end;

  TEntryInfoC = packed record
    flag1   : integer;
    size    : integer;
    offset  : integer;
    flag2   : integer;
    namelen : word;
    nameofs : integer;
  end;

  TEntryInfoD = packed record
    size    : integer;
    nameofs : integer;
    flag1   : integer;
    namelen : word;
    offset  : integer;
  end;

  // Figure (Model) file
  TFigHeader = packed record
    id        : array [0..3] of char; // FIG1
    count1    : integer;
    count2    : integer; // looks like the same count1
    uvcount   : integer; // "the number of uvs" before the list of indecis
    fcount    : integer; // some number that indiates the number of indiices
    uvcount2  : integer; //  
    unk1      : integer; // unknown 1 = count1 * 4
    unk2      : integer; // unknown 2 = always zero?
    unk3      : integer; // unknown 3 = usually 0x12 or 0x17
    unk4      : integer; // unknown 4 = 1 or 8 or 0x0D
  end;

  TFigCoord = packed record
    x,y,z  : single;
  end
  ;
  TFigUV = packed record
    u,v : single;
  end;

  // the indicies of coord, normal  and texture used for a vertex
  TFigVertex = packed record
    c, n, t: Word;
  end;

const
  VtxEntrySize = 12 * 4;
  VtxExtraSize = 12 * 3 + 4;
  CntEntrySize = 16 * 4;

type
  TSharedEntryInfo = packed record
    size    : integer;
    nameofs : integer;
    namelen : integer;
    offset  : integer;
  end;
  TSharedEntryArray = array of TSharedEntryInfo;
  

function ReadEntries(hdr: TResHeader; src: TStream): TSharedEntryArray;
function GetNameBufOfs(const hdr: TResHeader): Integer;
function ReadNameBuf(const hdr: TResHeader; src: TStream): string;
function GetName(const nameBuf: string; nameofs, namelen: integer): string;

implementation

procedure EntryCToShared(const src: TEntryInfoC; var dst: TSharedEntryInfo);
begin
  dst.size := src.size;  
  dst.nameofs := src.nameofs;
  dst.namelen := src.namelen;
  dst.offset := src.offset;
end;


procedure EntryDToShared(const src: TEntryInfoD; var dst: TSharedEntryInfo);
begin
  dst.size := src.size;  
  dst.nameofs := src.nameofs;
  dst.namelen := src.namelen;
  dst.offset := src.offset;
end;

function ReadEntries(hdr: TResHeader; src: TStream): TSharedEntryArray;
var
  i : integer;
  c : TEntryInfoC;
  d : TEntryInfoD;
begin
  Result:=nil;
  src.Position := hdr.entryofs;
  SetLength(Result, hdr.count);
  if (hdr.id[0] = $3C) then begin
    for i := 0 to hdr.count - 1 do begin
      src.Read(c, sizeof(c));
      EntryCToShared(c, result[i]);
    end;
  end else  begin
    for i := 0 to hdr.count -1  do begin
      src.Read(d, sizeof(d));
      EntryDToShared(d, result[i]);
    end;
  end;
end;

function GetNameBufOfs(const hdr: TResHeader): Integer;
var
  sz : integer;
begin
  sz := 0;
  if (hdr.id[0] = $3C) then sz := SizeOf(TEntryInfoC)
  else sz := SizeOF(TEntryInfoD);
  Result := hdr.entryofs + hdr.count * sz;
end;

function ReadNameBuf(const hdr: TResHeader; src: TStream): string;
var
  p : Int64;
begin
  p := src.Position;
  src.Position := GetNameBufOfs(hdr);
  SetLength(Result, hdr.namesz);
  src.Read(Result[1], hdr.namesz);
  src.Position := p;
end;

function GetName(const nameBuf: string; nameofs, namelen: integer): string;
begin
   // pascal sctrings are 1-based
  result := copy(namebuf, nameofs+1, namelen);
end;


end.                                   

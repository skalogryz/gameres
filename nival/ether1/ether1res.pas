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

  TFigVertex = packed record
    x,y,z  : single;
  end;

const
  VtxEntrySize = 12 * 4;
  VtxExtraSize = 12 * 3 + 4;
  CntEntrySize = 16 * 4;


type
  TFigureClass = class
    header : TFigHeader;


  end;

procedure ReadFigure(fig: TFigureClass; src: TStream);

type
  TSharedEntryInfo = packed record
    size    : integer;
    nameofs : integer;
    namelen : integer;
    offset  : integer;
  end;
  TSharedEntryArray = array of TSharedEntryInfo;
  

function ReadEntries(hdr: TResHeader; src: TStream): TSharedEntryArray;

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

procedure ReadFigure(fig: TFigureClass; src: TStream);
begin
{
fs.Read(hdr, sizeof(hdr));
writeln(hdr.id);
writeln('count1   = ', hdr.count1);
writeln('         = ', hdr.count2);
writeln('uvcount  = ', hdr.uvcount);
writeln('fcount   = ', hdr.fcount);
writeln('uvcount2 = ', hdr.uvcount2);
writeln('unk1     = ', hdr.unk1);
writeln('unk2     = ', hdr.unk2);
writeln('unk2     = ', hdr.unk3);
writeln('unk4     = ', hdr.unk4);

writeln('---after header ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
initOfs := 0 * sizeof(single);
writeln('initOfs = ',initOfs,' extra size: ', VtxExtraSize);
fs.Position:=fs.Position+initOfs;

//floatbuf := hdr.count1 * VtxEntrySize + VtxExtraSize + hdr.count1 * CntEntrySize;
extra := hdr.count1 * VtxEntrySize; // + VtxExtraSize;
SetLength(coords, hdr.count1 * 4);
SetLength(ff, extra div 4);
writeln('---after header ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
fs.Read(ff[0], extra);
for i := 0 to length(ff)-1 do
  writeln(ff[i]:0:6);

writeln('# test ff ', ff[0]:0:6,' ', ff[1]:0:6,' ',ff[2]:0:6);
fs.Position:=fs.Position+ VtxExtraSize-initOfs;

//fs.Position := fs.Position +  ;
writeln('---starting at: ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
for i := 0 to hdr.count1-1 do begin
  fs.Read(check[0], CntEntrySize);
  write(i,': ');
  for j := 0 to length(check)-1 do
     write(check[j]:0:6,' ');
  writeln;
  // Move(check[0], coords[i*4],  sizeof(TCoord)*3*4);
  // writeln(check[12]:0:6 ,' ',check[13]:0:6,' ',check[14]:0:6,' ',check[15]:0:6)
end;

writeln('--uvs: (',hdr.uvcount,')');
for i := 0 to hdr.uvcount-1 do begin
  fs.Read(uv[0], sizeof(uv));
  writeln(i,': ', uv[0]:0:6 ,' ',uv[1]:0:6);
end;



writeln('# test ff ', ff[0]:0:6,' ', ff[1]:0:6,' ',ff[2]:0:6);

SetLength(faces, hdr.fcount div 3);
fs.Read(faces[0], length(faces)*sizeof(TVertexIndex));
writeln('# test ff ', ff[0]:0:6,' ', ff[1]:0:6,' ',ff[2]:0:6);

SetLength(vtx, hdr.uvcount2);
fs.Read(vtx[0], length(vtx)*sizeof(TVertexDescr));

writeln('# test ff ', ff[0]:0:6,' ', ff[1]:0:6,' ',ff[2]:0:6);
SetLength(u1ind, hdr.unk1);
fs.Read(u1ind[0], length(u1ind)*sizeof(TUVIndex));
}
end;

end.                                   

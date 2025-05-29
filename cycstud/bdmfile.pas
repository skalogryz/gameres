unit bdmfile;

interface

uses Classes, SysUtils;

// Model file

const
  BdmHeader10Ast = '****************';
  BdmHeaderVer = $10;
  EndOfName = $0a;

type
  TBdmHeader10 = packed record
    ver : int32; // usually 0x10
    ast : array [0..15] of char; // ****************
    unk : int32; /// alaways 0x012c
    texNames : int16; // number of the texture names w/o defining the size of the buffer
                        // all names are separted by 0x10 (\n)
  end;

  TBdmVertex = record
    x,y,z: single;
  end;
  TBdmFace = record
    v0, v1, v2 : Int16;
  end;
  TBdmFaceUv = record
    u0, v0 : single;
    u1, v1 : single;
    u2, v2 : single;
  end;

  { TBdmMesh }

  TBdmMesh = class
    texture   : string;
    vertexCnt : integer;
    facesCnt  : integer;
    vtx       : array of TBdmVertex;
    face      : array of TBdmFace;
    uvs       : array of TBdmFaceUv; // per Face
    texidx    : array of Int16;
    unk1      : array of single;
    unk2Count : int16; // multiply by 7
    unk3Count : int16;
    unk2      : array of single;
{
00 00 00 00,00 00 00 00,00 00 00 00,00 00 00 00,  4
00 00 00 00,00 00 00 00,27 0B A8 43,3C 0A 42 C3,  8
3E 0A 42 43,3D 0A 42 43,3D 0A 42 43,00 00 00 00,  12
00 00 00 00,00 00 00 00,27 0B A8 43,3C 0A 42 C3,  16
3E 0A 42 43,3D 0A 42 43                           18
01 00 00 00
9F F1 4F C3 A1 F1 4F 43 A0 F1 4F 43 00 1B 55 C0 30 A4 F0 40 FF 94 BC 3E E7 5A 26 43
00 00 00 00 00 00 00 00

}
    constructor Create;
    constructor Create(v, f: integer);


  end;

  { TBdmFile }

  TBdmFile = class
    Names     : TStringList;
    Meshes    : array of TBdmMesh;
    MeshCnt   : integer;
    destructor Destroy; override;
  end;

function ReadBdmFile(st: TStream): TBdmFile;
function ReadBdmFile10(st: TStream; const header: TBdmHeader10): TBdmFile;

implementation

function ReadBdmFile(st: TStream): TBdmFile;
var
  h : TBdmHeader10;
  p : int64;
begin
  p := st.Position;
  st.Read(h, sizeof(h));
  if (h.ver = BdmHeaderVer) and (h.ast = BdmHeader10Ast) then begin
    Result := ReadBdmFile10(st, h);
  end else
    Result := nil;
end;

function GetNames(st: TStream; count: integer): TStringList;
var
  b : byte;
  bi  : integer;
  buf : array of byte;
  r   : string;
begin
  result := TStringList.Create;
  SetLength(buf, 64);
  bi := 0;
  while count>0 do begin
    b := st.ReadByte;
    if (b = EndOfName) then begin
      SetLength(r, bi);
      Move(buf[0], r[1], bi);
      result.Add(r);
      dec(count);
    end else begin
      buf[bi] := b;
      inc(bi);
      if (bi = length(buf)) then
        SetLength(buf, length(buf)*2);
    end;
  end;
end;

// vertexCnt and facesCnt must be read already
procedure ReadBdmMesh10(st: TStream; dst: TBdmMesh);
var
  i : integer;
begin
  writeln('pos = ',st.Position);
  writeln('vv = ',dst.vertexCnt);
  writeln('ff = ',dst.facesCnt);
  SetLength(dst.vtx, dst.vertexCnt);
  st.Read(dst.vtx[0], dst.vertexCnt * sizeof(TBdmVertex));
  SetLength(dst.face, dst.facesCnt);
  st.Read(dst.face[0], dst.facesCnt * sizeof(TBdmFace));

  SetLength(dst.uvs, dst.facesCnt);
  st.Read(dst.uvs[0], dst.facesCnt * sizeof(TBdmFaceUv));

  SetLength(dst.texidx, dst.facesCnt);
  st.Read(dst.texidx[0], dst.facesCnt * sizeof(Int16));


  writeln('pos unk1: ', st.Position,' (',IntToHex(st.Position, 8),')');
  SetLength(dst.unk1, 12);
  st.Read(dst.unk1[0], length(dst.unk1)*sizeof(single));
  for i:=0 to length(dst.unk1)-1 do begin
    writeln('    ',i,': ',dst.unk1[i]:0:8);
  end;

  writeln('pos types: ', st.Position,' (',IntToHex(st.Position, 8),')');
  dst.unk2Count := st.ReadWord();
  dst.unk3Count := st.ReadWord();
  writeln('Typ2 ', dst.unk2Count);
  writeln('Typ3 ', dst.unk3Count);
  SetLength(dst.unk2, dst.unk2Count * 7);
  writeln('pos unk2: ', st.Position,' (',IntToHex(st.Position, 8),')');
  if (length(dst.unk2)>0) then
    st.Read(dst.unk2[0], length(dst.unk2)*sizeof(single));
  writeln('unk2:');
  for i:=0 to length(dst.unk2)-1 do begin
    writeln('    ',i,': ',dst.unk2[i]:0:8);
    if i mod 7 = 6 then writeln('    ---');
  end;

  writeln('done!');
end;

function ReadBdmFile10(st: TStream; const header: TBdmHeader10): TBdmFile;
var
  mesh  : TBdmMesh;
  fl    : TBdmFile;
  v,f   : integer;

begin
  fl := TBdmFile.Create;
  fl.names := GetNames(st, header.texNames);
  writeln('st pos: ', st.Position);

  fl.MeshCnt := 1;
  SetLength(fl.Meshes, 1);


  v := integer(st.ReadDWord);
  f := integer(st.ReadDWord);
  mesh := TBdmMesh.Create(v,f);
  fl.Meshes[0] := mesh;
  ReadBdmMesh10(st, mesh);

  // for the file ACIS.bdm, this position should be of 0x011B4
  writeln('pos for reading next v,f: ', st.Position,' (',IntToHex(st.Position, 8),')');
  v := integer(st.ReadDWord);
  f := integer(st.ReadDWord);
  writeln('at the end');
  writeln('v = ', v);
  writeln('f = ', f);
  while (v > 0) and (f > 0) do begin
    if (fl.MeshCnt = length(fl.Meshes)) then begin
      SetLength(fl.Meshes, fl.MeshCnt * 2);
    end;
    mesh := TBdmMesh.Create(v,f);
    fl.Meshes[fl.Meshcnt] := mesh;
    inc(fl.MeshCnt);
    ReadBdmMesh10(st, mesh);
    v := integer(st.ReadDWord);
    f := integer(st.ReadDWord);
  end;
  Result := fl;
  writeln('--end');
end;

{ TBdmMesh }

constructor TBdmMesh.Create;
begin

end;

constructor TBdmMesh.Create(v, f: integer);
begin
  vertexCnt := v;
  facesCnt := f;
end;

{ TBdmFile }

destructor TBdmFile.Destroy;
begin
  Names.Free;
  inherited Destroy;
end;

end.

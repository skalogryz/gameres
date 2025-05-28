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

  TBdmMesh = class
    texture   : string;
    vertexCnt : integer;
    facesCnt  : integer;
    vtx       : array of TBdmVertex;
    face      : array of TBdmFace;
    uvs       : array of TBdmFaceUv; // per Face
  end;

  TBdmFile = class
    Meshes: array of TBdmMesh;
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

procedure ReadBdmMesh10(st: TStream; dst: TBdmMesh);
begin
  writeln('pos = ',st.Position);
  dst.vertexCnt := integer(st.ReadDWord);
  dst.facesCnt := integer(st.ReadDWord);
  writeln('vv = ',dst.vertexCnt);
  writeln('ff = ',dst.facesCnt);
  SetLength(dst.vtx, dst.vertexCnt);
  st.Read(dst.vtx[0], dst.vertexCnt * sizeof(TBdmVertex));
  SetLength(dst.face, dst.facesCnt);
  st.Read(dst.face[0], dst.facesCnt * sizeof(TBdmFace));

  SetLength(dst.uvs, dst.facesCnt);
  st.Read(dst.uvs[0], dst.facesCnt * sizeof(TBdmFaceUv));
  writeln('done!');
end;

function ReadBdmFile10(st: TStream; const header: TBdmHeader10): TBdmFile;
var
  names : TStringList;
  i     : integer;
  mesh  : TBdmMesh;

begin
  Result := TBdmFile.Create;
  names := GetNames(st, header.texNames);
  writeln('st pos: ', st.Position);
  SetLength(Result.Meshes, names.count);
  for i := 0 to names.count-1 do begin
    mesh := TBdmMesh.Create;
    Result.Meshes[i] := mesh;
    ReadBdmMesh10(st, mesh);
    break;
  end;
end;

end.

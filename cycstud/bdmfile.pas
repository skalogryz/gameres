unit bdmfile;

interface

uses Classes, SysUtils;

// Model file
//
// There are two kinds of model files.
// The "asterisked" files and "legacy" files
//
// Each model file contains of a "full" mesh
// and additional parts. which seems to be used for destruction of the object.
// Mote: that "destroyed" parts often have additional texture
//  (for the fractured parts)

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

  TBdmMesh = class;

  { TBdmSubMesh }

  TBdmSubMesh = class(TObject)
    unk    : array [0..2] of single; // coordinates?
    mesh   : TBdmMesh;
    constructor Create;
    destructor Destroy; override;
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
    unk0      : array of single; // populated for all meshes

    unkint0   : Int32; // used for parts of a gun.
    sub0      : array of single;
    unkint1   : Int32;
    sub1      : array of single; // unkint1 * 4

    subMeshCnt : Int32; // the number of "gun" meshes
                        // the gun submeshes are prefixed
                        // with an array of 3 signles (coordinates x,y,z?)
    submesh  : array of TBdmSubMesh;




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

  TBdmExtraInfo = record
    count : integer;
    vals  : array of single;
  end;

  { TBdmFile }

  TBdmFile = class
    Names     : TStringList;
    MainMesh  : TBdmMesh;

    /// the fields below only populated for the first mesh?
    unk1       : array of single;
    unk2Count  : int16; // multiply by 7
    unk3Count  : int16;
    unk2       : array of single;

    unk4Count  : int32;
    extraInfo  : array of TBdmExtraInfo;
    extra1Cnt  : int32;
    extra1     : array of single;
    extra2Cnt  : int32;
    extra2     : array of single;

    // todo: this seems like a good place to use TBdmSubMesh, but we don't
    breakCount : int32; // number of break parts? (each part is an additional mesh)
    breakCoord : array of single; // break part centers? (x,y,z)
    breakParts : array of TBdmMesh;

    others     : TList; // of TBdmMesh
    constructor Create;
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

procedure DumpArray(const f: array of single; const name : string; const pfx: string = '    ');
var
  i : integer;
begin
  writeln(name);
  for i := 0 to length(f)-1 do begin
    writeln(pfx,i,': ',f[i]:0:8);
  end;
end;

// vertexCnt and facesCnt must be read already
procedure ReadBdmMesh10(st: TStream; dst: TBdmMesh);
var
  i : integer;
  gunsub : TBdmSubMesh;
begin
  writeln('mesh starts with v,f, at pos : ', st.Position,' (',IntToHex(st.Position, 8),')');

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
  SetLength(dst.texidx, dst.facesCnt);
  st.Read(dst.texidx[0], dst.facesCnt * sizeof(Int16));

  writeln('pos unk0: ', st.Position,' (',IntToHex(st.Position, 8),')');
  SetLength(dst.unk0, 5);
  st.Read(dst.unk0[0], length(dst.unk0)*sizeof(single));
  for i:=0 to length(dst.unk0)-1 do begin
    writeln('    ',i,': ',dst.unk0[i]:0:8);
  end;
  dst.unkint0 := st.ReadDWord;
  writeln(' int0 : ',dst.unkint0);
  if (dst.unkint0 > 0) then begin
    Setlength(dst.sub0, dst.unkint0 * 3);
    st.Read(dst.sub0[0], length(dst.sub0)*sizeof(single));
    DumpArray(dst.sub1, 'sub0');
  end;


  dst.unkint1 := st.ReadDWord;
  writeln(' int1 : ',dst.unkint1);
  if (dst.unkint1 > 0) then begin
    SetLength(dst.sub1, dst.unkint1 *  4);
    st.Read(dst.sub1[0], length(dst.sub1)*sizeof(single));
    DumpArray(dst.sub1, 'sub1');
  end;

  dst.subMeshCnt := st.ReadDWord;
  writeln(' submesh : ',dst.subMeshCnt);

  if dst.subMeshCnt > 0 then begin
    // recursive reading of "submeshes"
    SetLength(dst.submesh,dst.subMeshCnt);

    for i:=0 to length(dst.submesh)-1 do begin
      gunsub := TBdmSubMesh.Create;
      dst.submesh[i] := gunsub;
      gunsub.mesh := TBdmMesh.Create;
      st.Read(gunsub.unk[0], length(gunsub.unk)*sizeof(single));
      ReadBdmMesh10(st, gunsub.mesh);
    end;
    writeln('welocme back to the main mesh:');
    writeln('post gun pos: ', st.Position,' (',IntToHex(st.Position, 8),')');
  end;

end;

function ReadBdmFile10(st: TStream; const header: TBdmHeader10): TBdmFile;
var
  dst   : TBdmFile;
  i, j  : integer;

  p     : Int64;
  m : TBdmMesh;
begin
  dst := TBdmFile.Create;
  dst.names := GetNames(st, header.texNames);
  writeln('st pos: ', st.Position);
  ReadBdmMesh10(st, dst.MainMesh);

  // for the building there's an additional information
  // for battle units, it's followed immediately by the mesh.
  // It's unknown if the information is stored somewhere in BDM file
  // to distinguish one from the other, but we can try to guess
  while (true) and (st.Position < st.Size) do begin
    p := st.Position;
    writeln('doing euthrestic at pos: ', st.Position,' (',IntToHex(st.Position,8),')');
    i := st.ReadDWord;
    j := st.readDWORD;
    writeln('value = ',i,' ',J);
    st.Position := p;
    if ((i> 0)  and (i < 1024) and (j> 0)  and (j < 4096)) then begin
      if (dst.others = nil) then
        dst.others := TList.create;
      m := TBdmMesh.Create;
      ReadBdmMesh10(st, m);
      dst.others.Add(m);
    end else
      break;
  end;



  writeln('--pos unk1: ', st.Position,' (',IntToHex(st.Position, 8),')');
  SetLength(dst.unk1, 4);
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

  writeln('pos unk4: ', st.Position,' (',IntToHex(st.Position, 8),')');
  dst.unk4Count := st.ReadDWord();
  writeln('unk4count: ', dst.unk4Count);
  SetLength(dst.extraInfo, dst.unk4Count);
  if (dst.unk4Count>0) then begin

    for i := 0 to dst.unk4Count-1 do begin
      dst.extraInfo[i].count := st.ReadDWord;
      writeln('cnt  = ', dst.extraInfo[i].count);
      if (dst.extraInfo[i].count = 0) then begin
        SetLength(dst.ExtraInfo[i].vals, 9);
        st.Read(dst.ExtraInfo[i].vals[0], length(dst.ExtraInfo[i].vals)*sizeof(single));
      end else if (dst.extraInfo[i].count = 3) then begin
        SetLength(dst.ExtraInfo[i].vals, 21);
        st.Read(dst.ExtraInfo[i].vals[0], length(dst.ExtraInfo[i].vals)*sizeof(single));
      end else if (dst.extraInfo[i].count = 4) then begin
        if (dst.others <> nil) then
          // assuming "ppp1t.bdm"
          SetLength(dst.ExtraInfo[i].vals, 12)
        else
          SetLength(dst.ExtraInfo[i].vals, 27);
        st.Read(dst.ExtraInfo[i].vals[0], length(dst.ExtraInfo[i].vals)*sizeof(single));
      end else begin
        writeln('UNKNOWN VALUE IN EXTRA INFO! ',dst.extraInfo[i].count,' we only know 4 and 3');
        halt;
      end;
    end;
    dst.extra1Cnt  := st.ReadDWord;
    SetLength(dst.extra1, dst.extra1Cnt * 3);
    if dst.extra1Cnt > 0 then
      st.Read(dst.extra1[0], length(dst.extra1)*sizeof(Single));

    dst.extra2Cnt  := st.ReadDWord;
    SetLength(dst.extra2, dst.extra2Cnt * 3);
    if dst.extra2Cnt > 0 then
      st.Read(dst.extra2[0], length(dst.extra2)*sizeof(Single));
  end;

  writeln('breakCount pos: ', st.Position,' (',IntToHex(st.Position, 8),')');
  dst.breakCount := st.ReadDWord();
  writeln('breakCount: ', dst.breakCount);
  if dst.breakCount>0 then begin
    SetLength(dst.breakCoord, dst.breakCount * 3);
    st.Read(dst.breakCoord[0], length(dst.breakCoord)*sizeof(single));
    SetLength(dst.breakParts, dst.breakCount);
    for i:=0 to dst.breakCount-1 do begin
      dst.breakparts[i]:=TBdmMesh.Create;
      writeln('### BREAK PART: ',i);
      ReadBdmMesh10(st, dst.breakparts[i]);
    end;
  end;

  writeln('### END OF FILE');
  Result := dst;
end;

{ TBdmSubMesh }

constructor TBdmSubMesh.Create;
begin
  mesh:=TBdmMesh.Create;
end;

destructor TBdmSubMesh.Destroy;
begin
  mesh.Free;
  inherited Destroy;
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

constructor TBdmFile.Create;
begin
  MainMesh := TBdmMesh.Create;
end;

destructor TBdmFile.Destroy;
var
  i : integer;
begin
  MainMesh.Free;
  for i := 0 to length(breakParts)-1 do
    breakParts[i].Free;
  Names.Free;

  if others <> nil then begin
    for i := 0 to others.Count-1 do
      TObject(others[i]).free;
    others.Free;
  end;
  inherited Destroy;
end;

end.

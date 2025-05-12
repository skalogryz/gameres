unit unfigutils;
{$mode delphi}
{$rangechecks on}
interface

uses
  Classes, SysUtils,
  ether1res,
  export3d;

type
  TNormalSet = array [0..15] of single;
  TFigFace = packed record v0, v1, v2: word; end;
  TFigSomeIndex = packed record n1, n2: word; end;

  TFigureData = class
    header : TFigHeader;

    // initial values
    init   : array of single;



    // the list of coordinates
    coords : array of TFigCoord;

    // (seems like normals information)
    normals : array of TNormalSet;

    // the list of uv coordinates
    uv     : array of TFigUv;

    // the list of faces (as indexes to vertexes)
    faces  : array of TFigFace;
    // the list of vertexes, that are referened by faces
    vert   : array of TFigVertex;
    // something unknown
    someInd : array of TFigSomeIndex;
  end;


function ReadFigure(src: TStream; dst: TFigureData): Boolean;
procedure FlipCoords(var coord : array of TFigCoord);

// .mod file that contains mulitple figures
procedure ExportModel(const modFn: string; dst: IGeomFileExport);

procedure ExportFigure(figure: TFigureData; dst: IMeshExport);

implementation

procedure FlipCoords(var coord : array of TFigCoord);
var
  i : integer;
  tmp : array [0..3] of TFigCoord;
begin
  //  0  1  2  3  4  5  6  7  8  9  10 11
  // source layout
  //  x0 x1 x2 x3 y0 y1 y2 y3 z0 z1 z2 z0
  // wanted result layout
  //  x0 y0 z0 x1 y1 z1 x2 y2 z2 x3 y3 z3
  i := 0;
  while i < length(coord) do begin
    tmp[0].x := coord[i+0].x;
    tmp[0].y := coord[i+1].y;
    tmp[0].z := coord[i+2].z;

    tmp[1].x := coord[i+0].y;
    tmp[1].y := coord[i+1].z;
    tmp[1].z := coord[i+3].x;

    tmp[2].x := coord[i+0].z;
    tmp[2].y := coord[i+2].x;
    tmp[2].z := coord[i+3].y;

    tmp[3].x := coord[i+1].x;
    tmp[3].y := coord[i+2].y;
    tmp[3].z := coord[i+3].z;

    Move(tmp[0], coord[i], sizeof(tmp));
    inc(i, 4);
  end;
end;

procedure ExportFigure(figure: TFigureData; dst: IMeshExport);
var
  i : integer;
  c : integer;
  varr : array of TFloatVertex;
  v    : TFigCoord;

  tr    : array of TTriangleInd;
begin
  SetLength(varr, length(figure.vert));
  for i := 0 to length(figure.vert)-1 do begin
    c := figure.vert[i].c;
    v := figure.coords[c];
    varr[i].x := v.x;
    varr[i].y := v.y;
    varr[i].z := v.z;
  end;
  dst.AddCoords(varr);

  setLength(tr, length(figure.faces));
  for i := 0 to length(tr)-1 do begin
    tr[i][0] := figure.faces[i].v0;
    tr[i][1] := figure.faces[i].v1;
    tr[i][2] := figure.faces[i].v2;
  end;
  dst.AddTriangles(tr);
end;

procedure ExportModel(const modFn: string; dst: IGeomFileExport);
var
  fs  : TFileStream;
  hdr : TResHeader;
  sh  : TSharedEntryArray;
  i   : integer;
  namebuf : string;
  m   : IMeshExport;
  f   : TFigureData;
  meshname : string;
  j   : integer;
  fv  : TFloatVertex;
begin
  fs  := TFileStream.Create(modFn, fmOpenRead or fmShareDenyNone);
  try
    fs.Read(hdr, sizeof(hdr));
    namebuf := ReadNameBuf(hdr, fs);
    sh := ReadEntries(hdr, fs);

    for i:=0 to length(sh)-1 do begin
      fs.Position := sh[i].offset;
      f := TFigureData.Create;
      try
        if not ReadFigure(fs, f) then continue;
        meshname := GetName(namebuf, sh[i].nameofs, sh[i].namelen);

        m := dst.StartMesh(meshname);
        // fv.x := f.init[0];
        // fv.y := f.init[1];
        // fv.z := f.init[2];
        // m.SetOffset(fv);

        writeln('mesh: ', meshname);
        for j := 0 to length(f.init)-1 do begin
          write(j);
          write(': ');
          writeln(f.init[j]:0:6);
        end;
        ExportFigure(f, m);
        dst.FinishMesh(m);
      finally
        f.Free;
      end;
    end;
  finally
    fs.Free;
  end;
end;

function ReadFigure(src: TStream; dst: TFigureData): Boolean;
begin
  result := false;
  if (src = nil) then Exit;
  if (dst = nil) then Exit;
  src.Read(dst.header, sizeof(dst.header));
  result := dst.header.id ='FIG1';
  if not result then Exit;
  SetLength(dst.init, VtxExtraSize div sizeof(single));
  src.Read(dst.init[0], length(dst.init)*sizeof(single));

  SetLength(dst.coords, dst.header.count1*4);
  src.Read(dst.coords[0], length(dst.coords)*sizeof(TFigCoord));
  FlipCoords(dst.coords);

  setLength(dst.normals, dst.header.count1);
  src.Read(dst.normals[0], dst.header.count1 * sizeof(TNormalSet));

  SetLength(dst.uv, dst.header.uvcount);
  src.Read(dst.uv[0], dst.header.uvcount * sizeof(TFigUV));

  SetLength(dst.faces, dst.header.fcount div 3);
  src.Read(dst.faces[0], length(dst.faces)*sizeof(TFigFace));

  SetLength(dst.vert, dst.header.uvcount2);
  src.Read(dst.vert[0], length(dst.vert)*sizeof(TFigVertex));

  SetLength(dst.someInd, dst.header.unk1);
  src.Read(dst.someInd[0], length(dst.someInd)*sizeof(TFigSomeIndex));
end;

end.

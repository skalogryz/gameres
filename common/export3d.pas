unit export3d;

{$mode delphi}

interface

type 
  IMeshExport = interface;

  // ['{00000000-0000-0000-C000-000000000046}'] 
  IGeomFileExport = interface(IUnknown)
    function StartMesh(const meshname: string): IMeshExport;
    // if true, only 1 mesh should worked on at a time
    // mulitple meshes are not allowed and could cause errors
    // FinishMesh must be called , before starting the new one
    function IsPerMeshExport: Boolean;

    // if true the full vertex information must be provided
    //  "coord"+"norm"+"uv" before exporting faces
    // function IsStrictOrder: Boolean;

    procedure FinishMesh(m: IMeshExport);
  end;

  
  TFloatVertex = record x,y,z: single; end;
  TTriangleInd = array [0..2] of integer;

  IMeshExport = interface(IUnknown)
    procedure AddCoords(const vtx: array of TFloatVertex);
    procedure AddTriangles(const faces: array of TTriangleInd);
  end;

function face(n0,n1,n2: integer): TTriangleInd; inline; overload;
procedure face(n0,n1,n2: integer; out f: TTriangleInd); inline; overload;

function coord(x,y,z: single): TFloatVertex; inline; overload;
procedure coord(x,y,z: single; out v: TFloatVertex); inline; overload;

procedure TextExportBox(dst: IGeomFileExport; const meshName: string = 'box');

implementation

function face(n0,n1,n2: integer): TTriangleInd;
begin
  face(n0,n1,n2, result);
end;

procedure face(n0,n1,n2: integer; out f: TTriangleInd);
begin
  f[0]:=n0;
  f[1]:=n1;
  f[2]:=n2;
end;


procedure coord(x,y,z: single; out v: TFloatVertex); overload;
begin
  v.x := x;
  v.y := y;
  v.z := z;  
end;

function coord(x,y,z: single): TFloatVertex; overload;
begin
  coord(x,y,z, result);
end;


procedure TextExportBox(dst: IGeomFileExport; const meshName: string);
var
  v : array [0..7] of TFloatVertex;
  f : array [0..11] of TTriangleInd;
  m : IMeshExport;
begin
  if (dst = nil) then Exit;
  m := dst.StartMesh(meshName);
  try
    v[0]:=coord(-0.539383, -0.000000, -0.539383);
    v[1]:=coord(-0.539383,  0.000000,  0.539383);
    v[2]:=coord( 0.539383,  0.000000,  0.539383);
    v[3]:=coord( 0.539383, -0.000000, -0.539383);
    v[4]:=coord(-0.539383, -0.954293, -0.539383);
    v[5]:=coord( 0.539383, -0.954293, -0.539383);
    v[6]:=coord( 0.539383, -0.954293,  0.539383);
    v[7]:=coord(-0.539383, -0.954293,  0.539383);
    m.AddCoords(v);
    
    f[0] := face(0,1,2);
    f[1] := face(2,3,0);
    f[2] := face(4,5,6);
    f[3] := face(6,7,4);
    f[4] := face(0,3,5);
    f[5] := face(5,4,0);
    f[6] := face(3,2,6);
    f[7] := face(6,5,3);
    f[8] := face(2,1,7);
    f[9] := face(7,6,2);
    f[10] := face(1,0,4);
    f[11] := face(4,7,1);
    m.AddTriangles(f);
  finally
    dst.FinishMesh(m);
  end;
end;

end.

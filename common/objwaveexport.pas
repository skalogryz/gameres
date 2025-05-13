unit objwaveexport;
{$mode delphi}

interface

uses
  Classes, SysUtils, export3d, textwriter;

type 

  { TWaveObjFileExport }

  TWaveObjFileExport = class (TInterfacedObject, IGeomFileExport, IMeshExport)
  protected
    function GetWriter: TTextWriter;
  public 
    _txt    : TTextWriter;
    vN      : integer;

    meshUse : integer;
    startV  : integer; // the first "vertex" in the mesh
    meshnm  : string;
    vofs    : TFloatVertex;
    vtx    : array of TFloatVertex;
    faces  : array of TTriangleInd;

    // if true , then "o meshName" would be added to object
    useObjects : Boolean;

    constructor Create;

    function StartMesh(const meshname: string): IMeshExport;
    function IsPerMeshExport: Boolean;
    procedure FinishMesh(m: IMeshExport);

    procedure SetOffset(coord: TFloatVertex);
    procedure AddCoords(const avtx: array of TFloatVertex);
    procedure AddTriangles(const afaces: array of TTriangleInd);

    function DumpString: string;
  end;

implementation

function TWaveObjFileExport.IsPerMeshExport: Boolean;
begin
  Result := true; 
end;

function TWaveObjFileExport.GetWriter: TTextWriter;
begin
  if (_txt = nil) then begin
    _txt := TTextWriter.Create;
    _txt.dst := TStringStream.Create();
  end;
  Result := _txt;
end;

constructor TWaveObjFileExport.Create;
begin
  vN      := 1;
  startV  := 1;
  useObjects := true;
end;

function TWaveObjFileExport.StartMesh(const meshname: string): IMeshExport;
begin
  if (meshUse > 0) then begin
    Result := nil;
    Exit;
  end;
  Result := self;
  meshnm := meshname;
  startV := vN;
  vofs.x := 0;
  vofs.y := 0;
  vofs.z := 0;
  vtx := nil;
  faces := nil;
end;

procedure TWaveObjFileExport.FinishMesh(m: IMeshExport);
var
  i : integer;
  txt : TTextWriter;
  v    :TFloatVertex;
begin
  if (meshUse > 0) then dec(meshUse);
  if (meshUse > 0) then Exit;

  txt := GetWriter;
  txt.WrLn('## mesh %s', [meshnm]);
  txt.WrLn;
  if (useObjects) then begin
    if (meshnm <> '') then
      txt.WrLn('o %s', [meshnm]);
  end;

  for i := 0 to length(vtx)-1 do begin
    v := vtx[i];
    txt.Wr('v ');
    txt.Wr('%0.6n %0.6n %0.6n', [v.x+vofs.x, v.y+vofs.y, v.z+vofs.z]);
    txt.WrLn;
  end;
  inc(vN, length(vtx));

  for i := 0 to length(faces)-1 do begin
    txt.Wr('f ');
    txt.Wr('%d %d %d', [faces[i][0]+startV, faces[i][1]+startV, faces[i][2]+startV]);
    txt.WrLn();
  end;

end;

procedure TWaveObjFileExport.SetOffset(coord: TFloatVertex);
begin
  vofs := coord;
end;

procedure TWaveObjFileExport.AddCoords(const avtx: array of TFloatVertex);
var
  i : integer;
begin
  if length(avtx)=0 then Exit;

  i := length(vtx);
  SetLength(vtx, length(vtx)+length(avtx));
  Move(avtx[0], vtx[i], length(avtx)*sizeof(TfloatVertex));
end;

procedure TWaveObjFileExport.AddTriangles(const afaces: array of TTriangleInd);
var
  i : integer;
begin
  i := length(faces);
  SetLength(faces, length(faces)+length(afaces));
  Move(afaces[0], faces[i], length(afaces)*sizeof(TTriangleInd));
end;

function TWaveObjFileExport.DumpString: string;
var
  i  : integer;
  pos: Int64;
  sz : integer;
begin
  if (_txt = nil) then begin
    Result := '';
    Exit;
  end;
  if (_txt.dst is TStringStream) then begin
    Result := (TStringStream(_txt.dst)).DataString;
    Exit;
  end;

  pos := _txt.dst.Position;
  sz := Integer(pos);

  _txt.dst.Position := 0;
  SetLength(Result, pos);
  _txt.dst.Read(Result[1], sz);
end;

end.

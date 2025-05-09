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
    meshUse : integer;
    vN      : integer;
    startV  : integer; // the first "vertex" in the mesh

    constructor Create;

    function StartMesh(const meshname: string): IMeshExport;
    function IsPerMeshExport: Boolean;
    procedure FinishMesh(m: IMeshExport);

    procedure AddCoords(const vtx: array of TFloatVertex);
    procedure AddTriangles(const faces: array of TTriangleInd);

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
end;

function TWaveObjFileExport.StartMesh(const meshname: string): IMeshExport;
var
  txt : TTextWriter;
begin
  if (meshUse > 0) then begin
    Result := nil;
    Exit;
  end;
  Result := self;

  txt := GetWriter;
  txt.WrLn('## mesh %s', [meshname]);
  txt.WrLn;

  startV := vN;
end;

procedure TWaveObjFileExport.FinishMesh(m: IMeshExport);
begin
  if (meshUse > 0) then dec(meshUse);
end;

procedure TWaveObjFileExport.AddCoords(const vtx: array of TFloatVertex);
var
  i : integer;
  txt : TTextWriter;
  v    :TFloatVertex;
begin
  txt := GetWriter;
  for i := 0 to length(vtx)-1 do begin
    v := vtx[i];
    txt.Wr('v ');
    txt.Wr('%0.8n %0.8n %0.8n', [v.x, v.y, v.z]);
    txt.WrLn;
  end;
  inc(vN, length(vtx));
end;

procedure TWaveObjFileExport.AddTriangles(const faces: array of TTriangleInd);
var
  i : integer;
  txt : TTextWriter;
begin
  txt := GetWriter;
  for i := 0 to length(faces)-1 do begin
    txt.Wr('f ');
    txt.Wr('%d %d %d', [faces[i][0]+startV, faces[i][1]+startV, faces[i][2]+startV]);
    txt.WrLn();
  end;
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

unit export3dutils;

interface

uses
  Classes, SysUtils, export3d;

type
  TDebugInfo = class
  public
    comment : string;
    vtx     : TFloatVertex;
    vtx2    : TFloatVertex;
    isLine  : boolean;
  end;

  { TDebugExportCollect }

  TDebugExportCollect = class (TInterfacedObject, IMeshExportDebug)
  public
    infos : Tlist;
    constructor Create;
    destructor Destroy; override;

    procedure AddDebugPoint(vtx: TFloatVertex; const name: string);
    procedure AddDebugLine(vtx1, vtx2: TFloatVertex; const name: string);
    procedure Clear;
  end;


implementation

{ TDebugExportCollect }

procedure TDebugExportCollect.Clear;
var
  i : integer;
begin
  for i:=0 to infos.Count-1 do
    TObject(infos[i]).Free;
  infos.Clear;
end;

constructor TDebugExportCollect.Create;
begin
  infos := TList.Create;
end;

destructor TDebugExportCollect.Destroy;
begin
  Clear;
  infos.Free;
end;

procedure TDebugExportCollect.AddDebugPoint(vtx: TFloatVertex;
  const name: string);
var
  v1: TDebugInfo;
begin
  v1:=TDebugInfo.Create;
  v1.vtx := vtx;
  v1.comment := name;

  infos.Add(v1);
end;

procedure TDebugExportCollect.AddDebugLine(vtx1, vtx2: TFloatVertex;
  const name: string);
var
  v2: TDebugInfo;
begin
  v2:=TDebugInfo.Create;
  v2.isLine := true;
  v2.vtx := vtx1;
  v2.vtx2 := vtx2;
  v2.comment := name;
  infos.Add(v2);
end;

end.

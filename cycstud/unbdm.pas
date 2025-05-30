{$mode delphi}
uses
  SysUtils, Classes,
  objwaveexport,
  export3d, export3dutils,
  bdmfile;

procedure ExportMesh(m : TBdmMesh; fl: IGeomFileExport; const name: string);
var
  mexp: IMeshExport;
  va  : array of TFloatVertex;
  fc  : array of TTriangleInd;
  i   : integer;
begin
  mexp := fl.StartMesh(name);
  writeln('v = ',m.vertexCnt);
  writeln('f = ',m.facesCnt);
  writeln('sz = ', sizeof(TFloatVertex),'; sz2 = ',sizeof(TBdmVertex));
  SetLength(va, m.vertexCnt);
  Move(m.vtx[0], va[0], sizeof(TFloatVertex)*m.vertexCnt);
  mexp.AddCoords(va);
  writeln(' proting faces');
  SetLength(fc, m.facesCnt);
  for i := 0 to m.facesCnt-1 do begin
    fc[i][0] := m.face[i].v0;
    fc[i][1] := m.face[i].v1;
    fc[i][2] := m.face[i].v2;
  end;
  writeln('adding faces');
  mexp.AddTriangles(fc);
  writeln('finishing');
  fl.FinishMesh(mexp);

  for i := 0 to length(m.submesh)-1 do
    ExportMesh(m.submesh[i].mesh, fl, name+'_sub_'+IntToStR(i));

  writeln('finished?');
end;

procedure UnpackFiles(const fn: string; doExtract: boolean);
var
  fs : TfileStream;
  f  : TBdmFile;
  i  : Integer;
  s  : string;

  wobj : TWaveObjFileExport;
  exp  : IGeomFileExport;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  wobj := TWaveObjFileExport.Create;
  exp := wobj;
  try
    f := ReadBdmFile(fs);

    //writeln('meshes: ', length(f.Meshes));
    ExportMesh(f.MainMesh, wobj, 'main_'+fn);
    for i:=0 to length(f.breakParts)-1 do begin
      ExportMesh(f.breakParts[i], wobj, 'breakpart_'+intTostR(i));
    end;
    if f.others <> nil then
      for i := 0 to f.others.Count-1 do
        ExportMesh(f.others[i], wobj, 'other_'+intTostR(i));

    writeln('## dumping');
    s := wobj.DumpString;
    writeln(s);
    writeln('## dumping done');
  finally
    wobj := nil;
    exp := nil;
    fs.Free;
  end;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
     writeln('please provide input bdm file name');
     exit;
  end;
  fn := Paramstr(1);
  UnpackFiles(fn, ParamCount>1);

end.

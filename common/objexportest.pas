{$mode delphi}
uses export3d, objwaveexport;

var
  obj : TWaveObjFileExport;
  dst : IGeomFileExport;
 
begin
  obj := TWaveObjFileExport.Create;
  dst := obj;
  TextExportBox(dst);
  writeln(obj.DumpString);
end.

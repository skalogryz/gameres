{$mode delphi}
uses SysUtils, Classes, ether1res;

procedure UnfigFiles(const fn: string);
var
  fs    : TFileStream;
  hdr   : TFigHeader;
  vtx   : array of TFigVertex;
  i     : integer;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try 
    fs.Read(hdr, sizeof(hdr));  
    writeln(hdr.id);
    writeln(hdr.count1);
    writeln(hdr.count2);
    writeln(hdr.count3);
    writeln('vcount=',hdr.vcount);
    writeln(hdr.count5);
    writeln(hdr.count6);
    writeln(hdr.count7);
    writeln(hdr.count8);
    writeln(hdr.count9);

   
    SetLength(vtx, hdr.vcount);
    fs.Read(vtx[0], hdr.vcount*sizeof(TFigVertex));
    for i := 0 to length(vtx)-1 do begin
      writeln(i,' ', vtx[i].x:0:6,' ',vtx[i].y:0:6,' ',vtx[i].z:0:6);
    end; 
  finally 
    fs.free;
  end;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
     writeln('please provide input res file name');
     exit;
  end;
  fn := Paramstr(1);
  UnfigFiles(fn);
end.
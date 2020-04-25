unit palutils;

interface

type
  TVGAPalColor = packed record
    r,g,b : Byte;
  end;
  TVGAPal = array [0..255] of TVGAPalColor;
  PVGAPal = ^TVGAPal;

procedure PalInit(out p : TVGAPal);

implementation

procedure PalInit(out p : TVGAPal);
var
  i : integer;
begin
  for i:=0 to length(p)-1 do begin
    p[i].r:=i;
    p[i].g:=i;
    p[i].b:=i;
  end;
end;

end.

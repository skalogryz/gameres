unit fpimageutils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FPimage, FPWriteBMP, palutils;

// buf goes horizontal scan-lines horizontal
function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  dst: TFPCustomImage): Boolean; overload;

function PalBytesToFpImage(const buf: array of byte; bufOfs: Integer;
  width, height: integer;
  const palbuf: array of byte; palTranspIdx: integer;
  dst: TFPCustomImage; dstX, dstY: Integer): Boolean; overload;

function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const pal: array of byte): TFPCustomImage; overload;

procedure VGAColorToFPColor(const vga: TVGAPalColor; out clr: TFPColor); inline;

function PalBytesToBmpFile(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  const bmpfn: string): Boolean;

function FPImageToBmp(const fp: TFPCustomImage; st: TStream): Boolean; overload;
function FPImageToBmp(const fp: TFPCustomImage; const fn: string): Boolean; overload;

implementation

procedure VGAColorToFPColor(const vga: TVGAPalColor; out clr: TFPColor); inline;
begin
  clr.red:=vga.r shl 8;
  clr.green:=vga.g shl 8;
  clr.blue:=vga.b shl 8;
end;

function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  dst: TFPCustomImage): Boolean;
begin
  Result := PalBytesToFpImage(buf, 0, width, height, palbuf, 0, dst, 0,0);
end;

function PalBytesToFpImage(const buf: array of byte; bufOfs: Integer;
  width, height: integer;
  const palbuf: array of byte; palTranspIdx: Integer;
  dst: TFPCustomImage; dstX, dstY: Integer): Boolean; overload;
var
  x,y : integer;
  pal : PVGAPal;
  clr : TFPColor;
  i   : integer;
  pid : byte;
begin
  Result :=
    Assigned(dst)
    and (length(buf)>=(width*height))
    and (length(palbuf)>=sizeof(TVGAPal));
  if not Result then Exit;

  pal:=@palbuf[0];

  i:=bufOfs;
  for y:=0 to height-1 do
    for x:=0 to width-1 do begin
      pid := buf[i];
      VGAColorToFPColor( pal^[pid], clr);
      if (palTranspIdx>=0) then begin
        if pid = palTranspIdx then clr.Alpha := 0
        else clr.Alpha := $FFFF;
      end else clr.Alpha := $FFFF;
      dst.Colors[dstX+x,dstY+y]:=clr;
      inc(i);
    end;
  Result:=true;
end;


function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const pal: array of byte): TFPCustomImage;
begin
  Result:= TFPMemoryImage.create(width, height);
  if not PalBytesToFpImage(buf, width, height, pal, Result) then begin
    Result.Free;
    Result:=nil;
  end;
end;

function PalBytesToBmpFile(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  const bmpfn: string): Boolean;
var
  img : TFPCustomImage;
begin
  img := PalBytesToFpImage(buf, width, height, palbuf);
  Result := Assigned(img);
  if Result then Exit;

  Result := FPImageToBmp(img, bmpfn);
  img.Free;
end;

function FPImageToBmp(const fp: TFPCustomImage; st: TStream): Boolean;
var
  w : TFPWriterBMP;
begin
  Result :=Assigned(fp) and Assigned(st);
  if not Result then Exit;

  try
    w := TFPWriterBMP.Create;
    try
      w.BitsPerPixel:=32;
      w.ImageWrite(st, fp);
      Result:=true;
    finally
      w.Free;
    end;
  except
    Result:=false;
  end;
end;

function FPImageToBmp(const fp: TFPCustomImage; const fn: string): Boolean;
var
  fs  : TFileStream;
begin
  Result:=Assigned(fp);
  if not result then Exit;
  try
    fs:= TFileStream.Create(fn, fmCreate);
    try
      FPImageToBmp(fp, fs);
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;


end.


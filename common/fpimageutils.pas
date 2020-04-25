unit fpimageutils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FPimage, FPWriteBMP, palutils;

function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  dst: TFPCustomImage): Boolean; overload;

function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const pal: array of byte): TFPCustomImage; overload;

procedure VGAColorToFPColor(const vga: TVGAPalColor; out clr: TFPColor); inline;


// bmp related utils
function PalBytesToBmpFile(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  const bmpfn: string): Boolean;

function FPImageToBmp(const fp: TFPCustomImage; st: TStream): Boolean; overload;
function FPImageToBmp(const fp: TFPCustomImage; const fn: string): Boolean; overload;

implementation

procedure VGAColorToFPColor(const vga: TVGAPalColor; out clr: TFPColor); inline;
begin
  clr.red:=vga.r shl 16;
  clr.green:=vga.g shl 16;
  clr.blue:=vga.b shl 16;
end;

function PalBytesToFpImage(const buf: array of byte;
  width, height: integer;
  const palbuf: array of byte;
  dst: TFPCustomImage): Boolean;
var
  x,y : integer;
  pal : PVGAPal;
  clr : TFPColor;
  i   : integer;
begin
  Result :=
    Assigned(dst)
    and (length(buf)>=(width*height))
    and (length(palbuf)>=sizeof(TVGAPal));
  if not Result then Exit;

  pal:=@palbuf[0];

  i:=0;
  for x:=0 to width-1 do
    for y:=0 to height-1 do begin
      VGAColorToFPColor( pal^[buf[i]], clr);
      dst.Colors[x,y]:=clr;
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


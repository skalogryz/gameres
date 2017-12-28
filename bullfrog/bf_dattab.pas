unit bf_dattab;

interface

{$mode delphi}

type
  TTabEntry = packed record
    offset : LongWord;
    width  : Byte;
    height : Byte;
  end;

{ Image buffer is encoded as following:
  if pixel byte is a negative number, Number of pixels to skip are indicated.
  If pixel by is equal to zero, then the row has finished and the next row is started.
  If number is positive, then it shows the number of pixels that needs to be rendered/copied
}
function BFDecodeRLE(width, height: Integer; const src: array of byte; var dst: array of byte): Integer; overload;
function BFDecodeRLEOffset(width, height: Integer; const src: array of byte; Ofs: integer; var dst: array of byte): Integer; overload;

implementation

type
  TByteArray = array[word] of byte;
  PByteArray = ^TByteArray;

function BFDecodeRLE(width, height: Integer; const src: array of byte; var dst: array of byte): Integer;
var
  si, di: Integer;
  b : ShortInt;
  y : integer;
  i : integer;
  m : integer;
begin
  si:=0;
  di:=0;
  y:=0;
  while (y<height) and (si<length(src)) and (di<length(dst)) do begin
    b:=ShortInt(src[si]);
    inc(si);
    if b<0 then inc(di, -b)
    else if b=0 then begin
      m:=(di mod width);
      if m>0 then inc(di, width - m);
      inc(y);
    end else begin
      for i:=1 to b do begin
        dst[di]:=src[si];
        inc(si);
        inc(di);
      end;
    end;
  end;
  Result:=si;
end;

function BFDecodeRLEOffset(width, height: Integer; const src: array of byte; Ofs: integer; var dst: array of byte): Integer; overload;
begin
  Result:=BFDecodeRLE(width, height, PByteArray(@src[Ofs])^, dst);
end;

end.

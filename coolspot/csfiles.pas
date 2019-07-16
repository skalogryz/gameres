unit csfiles;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TXunHeader = record
    Offset  : Word; // the offset of in the file where the actual data starts
    Count   : word; // number of sprites in the file
    dummy22 : Word; // it's always $16
  end;

  TXunSpriteEntry = record
    width  : Word;  // the logical width. The actual width
                    // needs to be calcualted from the the size and height
    height : Word;  // the actual height
    Unk    : Word;  // the number of bytes for the image.
  end;

  // The entire spireset (multiple sprite) is considered as a single buffer.
  // and thus run-length encoded for across all sprites. (an error in one sprite would affect all the rest)
  //
  // The RLE keeps 2 pixels in 1 byte (the first pixel is in hi and, the second pixel is  low part)
  // The RLE starts with a controlling byte.
  // If byte value is
  //    0 - then the sprite set is completed. (the remaining pixels, if any are transparent?)
  // >$80 - indicates the number of times + 1 the next 2 pixels (1 byte) should be repeated.
  //        The number of times should be calculated as follows:
  //        For example, if the following sequence is met:
  //          05 55
  //        That indicates the control number "05", meaning that the next byte (2-pixels) $55
  //        Should be repeat 5 + 1 - 6 times.
  //        Meaning that you'd end up having 12 pixels:
  //          5 5 5 5 5 5 5 5 5 5 5 5
  //>=$80 - indicates the number of the next BYTES + 1 (pairs of pixels) to be read
  //        the high bit should be ignored
  //        For example, if the following sequence is met:
  //          83 CC DC DD D5
  //        that indicates the control number "83", meaning that the next 3+1 bytes
  //        Meaning you'll end up having 8 pixels:
  //          C C D C D D D 5
  //
  //  The sequence of bytes:
  //    05 55 83 CC DC DD D5 07 55
  //  eventually turns into following pixels:
  //  5 5 5 5 5 5 5 5 5 5 5 5
  //  c c d c d d d 5
  //  5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5

  { TXunReader }

  TXunReader = class(TObject)
  private
    fsource: TStream;
  public
    buf : array of byte;
    bufCount: Integer;
    done : Boolean;
    constructor Create(asource: TStream);
    function ReadNext: Boolean;
    property Source: TStream read fsource;
  end;

procedure ByteToColors(b: byte; out cl1, cl2: byte); inline;
function RealWidth(h: integer; halfSize: Integer): integer; inline;

type
  TByteBuffer = array of byte;

function XunStreamToBuffer(s: TStream; var buf: TByteBuffer): Integer;
procedure XunDumpStream(s: TStream);
procedure XunDumpFile(const fn: string);

implementation

function RealWidth(h: integer; halfSize: Integer): integer; inline;
begin
  Result := halfSize * 2 div h;
end;

procedure ByteToColors(b: byte; out cl1, cl2: byte); inline;
begin
  cl1 := b shr 4;
  cl2 := b and $f;
end;

{ TXunReader }

constructor TXunReader.Create(asource: TStream);
begin
  fsource := asource;
  done := false;
end;

function TXunReader.ReadNext: Boolean;
var
  b : byte;
  c : integer;
  i : integer;
  j : integer;
  c1, c2: byte;
begin
  if not done and (Source.Position >= Source.Size) then
    done := true;

  if done then begin
    Result := false;
    Exit;
  end;

  b := Source.ReadByte;
  if b = 0 then begin
    done := true;
    Result := false;
  end else if b and $80 > 0 then begin
    Result := true;
    c := b and $7f + 1;
    bufCount := c * 2;
    if length(buf) < bufCount then SetLength(buf, bufCount);
    Source.Read(buf[0], c);
    j := c * 2 - 2;
    for i := c - 1 downto 0 do begin
      ByteToColors(buf[i], buf[j], buf[j+1]);
      dec(j, 2);
    end;
  end else begin
    Result := true;
    c := b + 1;
    b := Source.ReadByte;
    ByteToColors(b, c1, c2);
    bufCount := c * 2;
    if length(buf) < bufCount then SetLength(buf, bufCount);
    j := 0;
    for i := 0 to c - 1 do begin
      buf[j] := c1; inc(j);
      buf[j] := c2; inc(j);
    end;
  end;
end;

function XunStreamToBuffer(s: TStream; var buf: TByteBuffer): Integer;
var
  rdr : TXunReader;
  i   : integer;
begin
  rdr := TXunReader.Create(s);
  try
    i := 0;
    while rdr.ReadNext do begin
      while (i+rdr.bufCount > length(buf)) do begin
        if length(buf)=0 then SetLength(buf, rdr.bufCount)
        else SetLength(buf, length(buf)*2);
      end;
      Move(rdr.buf[0], buf[i], rdr.bufCount);
      inc(i, rdr.bufCount);
    end;
    SetLength(buf, i);
    Result := i;
  finally
    rdr.Free;
  end;
end;

procedure XunDumpStream(s: TStream);
var
  buf : array of byte;
  h   : TXunHeader;
  sp  : array of TXunSpriteEntry;
  i   : integer;
  j   : integer;
  x,y : integer;
  cl  : byte;
begin
  buf := nil;
  s.Read(h, sizeof(h));
  SetLength(sp, h.Count);
  s.Read(sp[0], h.Count * sizeof(TXunSpriteEntry));

  for i:=0 to h.Count-1 do begin
    writeln(i,': ',sp[i].width,' x ',sp[i].height,'; sz = ',sp[i].Unk, '; calc sz = ',sp[i].width * sp[i].height div 2);
    if sp[i].width * sp[i].height div 2 <> sp[i].Unk then begin
      sp[i].width := RealWidth(sp[i].height, sp[i].Unk);
    end;
  end;

  writeln('reading...');
  i := XunStreamToBuffer(s, buf);
  writeln('buf = ', length(buf),' i = ', i);
  j:=0;
  for i := 0 to h.Count - 1 do begin
    writeln(sp[i].width,' ',sp[i].height);
    for y := 0 to sp[i].height - 1 do begin
      for x := 0 to sp[i].width - 1 do begin
        cl := buf[j];
        inc(j);
        case cl of
          0: write(' ');
          1: write('#');
          2..3: write('/');
          4: write('|');
          //1..4: write('_');
          5: write('.');
        else
          write(IntToHex(cl,1));
        end;
      end;
      writeln;
    end;
  end;
  writeln('j = ',j);
end;

procedure XunDumpFile(const fn: string);
var
  f : TFileStream;
begin
  f := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    XunDumpStream(f);
  finally
    f.Free;
  end;
end;

end.


unit hmm2agg;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils;

// Based on the description from https://thaddeus002.github.io/fheroes2-WoT/infos/informations.html

type
  THMM2FileInfo = packed record
    fileid  : LongWord;
    offset  : LongWord;
    size    : LongWord;
  end;

  THMM2FileName = packed record
    name : array [0..12] of ansichar;
    pad  : word;
  end;

  TICNSpriteHeader = packed record
    X, Y : SmallInt; // offset
    W, H : Word; // width / height
    tp   : Byte;
    ofs  : LongWord;
  end;

  TICNSpriteFile = class(TObject)
    count  : Word;
    size   : LongWord;
    header : array of TICNSpriteHeader;
    data   : array of byte;
  end;

  { THHM2AggFile }

  THHM2AggFile = class(TObject)
  public
    count : Word;
    info  : array of THMM2FileInfo;
    name  : array of THMM2FileName;
  end;

procedure AGG2ReadStream(asrc: TStream; dst: THHM2AggFile);
function AGG2ReadFile(const fn: string; dst: THHM2AggFile): Boolean;
function GetName(nm: THMM2FileName): string;

procedure ICNReadStream(asrc: TStream; dst: TICNSpriteFile);
function ICNReadFile(const fn: string; dst: TICNSpriteFile): Boolean;

type
  { TIcnPixels }

  TICNPixelLine = array of Word;

  TICNPixels = class(TObject)
    Width  : Integer;
    Height : Integer;
    Lines  : array of TIcnPixelLine;
    isMono : boolean;
    constructor Create(Awidth, AHeight: integer; AisMono: Boolean);
  end;

const
  ICN_PIXEL_EMPTY  = $100;
  ICN_PIXEL_SHADOW = $200;

procedure ICNNormDataToPixData(const data: array of byte; ofs: Integer; dst: TIcnPixels);
procedure ICNMonoDataToPixData(const data: array of byte; ofs: Integer; dst: TIcnPixels);
procedure ICNDataToPixData(const data: array of byte; ofs: Integer; dst: TIcnPixels);

const
  ICN_TYPE_NORM = 0;
  ICN_TYPE_MONO = 32;

  ICN_NORM_EOL         = $00; // end of line reached, go to the first pixel of next line. All of remaining pixels of current line are transparent.
  ICN_NORM_PIXELMIN    = $01; // number n of data. The next n bytes are the colors of the next n pixels.
  ICN_NORM_PIXELMAX    = $7f;
  ICN_NORM_END         = $80; // end of data. The sprite is yet totaly describe.
  ICN_NORM_SKIPMIN     = $81; // number of pixels to skip + 0x80. The (n - 128) pixels are transparents.
  ICN_NORM_SKIPMAX     = $BF;
  ICN_NORM_SHADOW      = $C0; // put here n pixels of shadow. If the next byte
                              // modulo 4 is not null, n equals the next byte modulo 4,
                              // otherwise n equals the second next byte.

  ICN_NORM_REPEAT      = $C1; // next byte is the number of next pixels of same color. The second next byte is the color of these pixels.

  ICN_NORM_REPEATMIN   = $C2; // number of pixels of same color plus 0xC0. Next byte is the color of these pixels.
  ICN_NORM_REPEATMAX   = $FF;

  ICN_MONO_EOL         = $00; // end of line reached, go to the first pixel of next line.
  ICN_MONO_PIXELMIN    = $01; // number of black pixels
  ICN_MONO_PIXELMAX    = $7f;
  ICN_MONO_END         = $80; // end of data. The sprite is yet totaly describe.
  ICN_MONO_SKIPMIN     = $81; // number of pixels to skip + 0x80. The (n - 128) pixels are transparents.
  ICN_MONO_SKIPMAX     = $FF;

type
  THMMPalColor = packed record
    r,g,b: byte;
  end;
  THMMPalette = array [0..255] of THMMPalColor;

implementation

{ TAggFile }

procedure AGG2ReadStream(asrc: TStream; dst: THHM2AggFile);
begin
  dst.count := asrc.ReadWord;
  SetLength(dst.info, dst.count);
  SetLength(dst.name, dst.count);
  asrc.Read(dst.info[0], dst.count * sizeof(THMM2FileInfo));

  asrc.Position := asrc.Size - Int64(dst.count) * sizeof(THMM2FileName);
  asrc.Read(dst.name[0], dst.count * sizeof(THMM2FileName));
end;

function AGG2ReadFile(const fn: string; dst: THHM2AggFile): Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      AGG2ReadStream(fs, dst);
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

function GetName(nm: THMM2FileName): string;
var
  i : integer;
begin
  for i := 0 to length(nm.name)-1 do
    if (nm.name[i] = #0) then begin
      SetLength(Result, i);
      if i > 0 then Move(nm.name[0], Result[1], i);
      Exit;
    end;
  Result := nm.name;
end;

procedure ICNReadStream(asrc: TStream; dst: TICNSpriteFile);
var
  len : Integer;
begin
  dst.count := asrc.ReadWord;
  dst.size := asrc.ReadDWord;
  SetLength(dst.header, dst.Count);
  if dst.Count>0 then
    asrc.Read(dst.header[0], dst.count * sizeof(TICNSpriteHeader));

  len := Integer(dst.size) - Integer(LongWord(dst.count) * sizeof(TICNSpriteHeader));
  SetLength(dst.data, len);
  if len>0 then
    asrc.Read(dst.data[0], len);
end;

function ICNReadFile(const fn: string; dst: TICNSpriteFile): Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      ICNReadStream(fs, dst);
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

procedure ICNNormDataToPixData(const data: array of byte; ofs: Integer;
  dst: TIcnPixels);
var
  i,j : integer;
  cnt : integer;
  x,y : integer;
begin
  i := ofs;
  x := 0;
  y := 0;
  while ((y < dst.height) and (i < length(data))) and (data[i]<>ICN_NORM_END)  do begin
    case data[i] of
      ICN_NORM_EOL: begin
        inc(y);
        x := 0;
      end;

      ICN_NORM_PIXELMIN..ICN_NORM_PIXELMAX: begin; // number n of data. The next n bytes are the colors of the next n pixels.
        cnt := data[i];
        for j:=1 to cnt do begin
          inc(i);
          dst.Lines[y][x]:=data[i];
          inc(x);
          //todo: add checks for x and y
        end;
      end;

      ICN_NORM_END: break;

      ICN_NORM_SKIPMIN..ICN_NORM_SKIPMAX: begin
        cnt := data[i] - ICN_MONO_SKIPMIN + 1;
        for j:=0 to cnt - 1 do begin
          dst.Lines[y][x] := ICN_PIXEL_EMPTY;
          inc(x); // todo: add check for bounds
        end;
      end;

      ICN_NORM_SHADOW: begin
        inc(i);
        if i>=length(data) then break;
        if data[i] mod 4 <> 0 then cnt := data[i] mod 4
        else begin
          inc(i);
          if i>=length(data) then break;
          cnt := data[i];
        end;
        for j:=0 to cnt - 1 do begin
          dst.Lines[y][x] := ICN_PIXEL_SHADOW;
          inc(x); // todo: add check for bounds
        end;
      end;

      ICN_NORM_REPEAT, ICN_NORM_REPEATMIN..ICN_NORM_REPEATMAX: begin
        if data[i]=ICN_NORM_REPEAT then begin
          inc(i);
          cnt := data[i];
        end
        else
          cnt := data[i] - ICN_NORM_REPEAT+1;
        inc(i);
        if i>=length(data) then break;
        for j:=0 to cnt - 1 do begin
          dst.Lines[y][x] := data[i];
          inc(x);
        end;
      end;
    end;
    inc(i);
  end;
end;

procedure ICNMonoDataToPixData(const data: array of byte; ofs: Integer; dst: TIcnPixels);
var
  i,j : integer;
  x,y : integer;
  cnt : integer;
begin
  i := ofs;
  x := 0;
  y := 0;
  while (y < dst.height) and (i < length(data)) and (data[i]<>ICN_MONO_END) do begin
    case data[i] of
      ICN_MONO_EOL: begin
        inc(y);
        x := 0;
      end;
      ICN_MONO_PIXELMIN..ICN_MONO_PIXELMAX: begin
        cnt := data[i] - ICN_MONO_PIXELMIN + 1;
        for j:=0 to cnt - 1 do begin
          dst.Lines[y][x] := $0;
          inc(x); // todo: add check for bounds
        end;
      end;
      ICN_MONO_END: begin
        // done!
      end;
      ICN_MONO_SKIPMIN..ICN_MONO_SKIPMAX:
      begin
        cnt := data[i] - ICN_MONO_SKIPMIN + 1;
        for j:=0 to cnt - 1 do begin
          dst.Lines[y][x] := ICN_PIXEL_EMPTY;
          inc(x); // todo: add check for bounds
        end;
      end;
    end;
    inc(i);
  end;
end;

procedure ICNDataToPixData(const data: array of byte; ofs: Integer; dst: TIcnPixels);
begin
  if dst.isMono
    then ICNMonoDataToPixData(data, ofs, dst)
    else ICNNormDataToPixData(data, ofs, dst);
end;

{ TIcnPixels }

constructor TIcnPixels.Create(Awidth, AHeight: integer; AisMono: Boolean);
var
  i : integer;
begin
  inherited Create;
  width := AWidth;
  height := AHeight;
  isMono := AisMono;
  SetLength(Lines, height);
  for i := 0 to height - 1 do begin
    SetLength(Lines[i], width);
    FillWord(lines[i][0], width, ICN_PIXEL_EMPTY);
  end;

end;

end.


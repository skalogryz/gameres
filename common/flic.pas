unit flic;
// based on  Aseprite FLIC Library

interface

{$mode delphi}

uses SysUtils, Classes;

const
   FLI_MAGIC_NUMBER       = $AF11;
   FLC_MAGIC_NUMBER       = $AF12;

   FLI_FRAME_MAGIC_NUMBER = $F1FA;

   FLI_COLOR_256_CHUNK    = 4;
   FLI_DELTA_CHUNK        = 7;
   FLI_COLOR_64_CHUNK     = 11;
   FLI_LC_CHUNK           = 12;
   FLI_BLACK_CHUNK        = 13;
   FLI_BRUN_CHUNK         = 15;
   FLI_COPY_CHUNK         = 16;

type
  TColor = packed record
    r,g,b : byte;
  end;
  PColor = ^TColor;

  TColorMap = array [0..255] of TColor;

  THeader = packed record
    frames : integer;
    width  : integer;
    height : integer;
    speed  : integer;
  end;

  TFrame = record
    pixels    : array of Byte;
    rowstride : Integer;
    colormap  : TColormap;
  end;

  { TDecoder }

  TDecoder = class(TObject)
  private
    m_file   : TStream;
    fownFile : Boolean;
    m_width, m_height : integer;
    m_frameCount      : integer;

    function read16: word;
    function read32: dword;
    procedure readChunk(var frame : TFrame);
    procedure readBlackChunk(var frame: TFrame);
    procedure readCopyChunk(var frame: TFrame);
    procedure readColorChunk(var frame: TFrame; oldColorChunk: Boolean);
    procedure readLcChunk(var frame: TFrame);
    procedure readDeltaChunk(var frame: TFrame);
    procedure readBrunChunk(var frame: TFrame);
  public
    m_offsetFrame1    : integer;
    m_offsetFrame2    : integer;
    constructor Create(ASource: TStream; ownSource: Boolean);
    destructor Destroy; override;
    function readHeader(var header: THeader): Boolean;
    function readFrame(var frame : TFrame): Boolean;
  end;

implementation

function ChunkTypeToStr(_type: integer): string;
begin
  case _type of
    FLI_COLOR_256_CHUNK: result := 'COLORMAP';
    FLI_DELTA_CHUNK    : result := 'DELTA';
    FLI_COLOR_64_CHUNK : result := 'COLORMAPOLD';
    FLI_LC_CHUNK       : Result := 'LINE';
    FLI_BLACK_CHUNK    : Result := 'BLACK';
    FLI_BRUN_CHUNK     : Result := 'BRUN';
    FLI_COPY_CHUNK     : Result := 'COPY';
    else
      Result := IntToStr(_type);
  end;
end;

{ TDecoder }

constructor TDecoder.Create(ASource: TStream; ownSource: Boolean);
begin
  inherited Create;
  m_file := ASource;
  fownFile := ownSource;
end;

destructor TDecoder.Destroy;
begin
  if fownFile then m_file.Free;
  inherited Destroy;
end;

function TDecoder.readHeader(var header: THeader): Boolean;
var
  magic : word;
begin
  m_file.ReadDWord; // file size
  magic := m_file.ReadWord;
  if ((magic <> FLI_MAGIC_NUMBER) and (magic<>FLC_MAGIC_NUMBER)) then
  begin
    Result := false;
    Exit;
  end;

  header.frames := m_file.ReadWord();
  header.width  := m_file.ReadWord();
  header.height := m_file.ReadWord();
  m_file.ReadWord();      // Color depth (it is interpreted as 8bpp anyway)
  m_file.ReadWord();      // Skip flags
  header.speed := m_file.ReadDWord();
  if (magic = FLI_MAGIC_NUMBER) then begin
    if (header.speed = 0) then
      header.speed := 70
    else
      header.speed := 1000 * header.speed div 70;
  end;

  if (magic = FLC_MAGIC_NUMBER) then begin
    // Offset to the first and second frame
    m_file.Position := 80;
    m_offsetFrame1 := m_file.readDWord();
    m_offsetFrame2 := m_file.readDWord();
  end;

  if (header.width = 0) then header.width := 320;
  if (header.height = 0) then header.height := 200;

  m_width := header.width;
  m_height := header.height;

  // Skip padding
  m_file.position := 128;

  Result:=true;
end;

function TDecoder.readFrame(var frame: TFrame): Boolean;
var
  frameStartPos : LongWord;
  frameSize : longWord;
  magic  : Word;
  chunks : Integer;
  i : integer;
begin
  case (m_frameCount) of
    0:
      if (m_offsetFrame1 <> 0) then
        m_file.Position := m_offsetFrame1;
    1:
      if (m_offsetFrame2 <> 0) then
        m_file.Position := m_offsetFrame2;
  end;

  frameStartPos := m_file.Position;
  frameSize := m_file.ReadDWord;
  magic := m_file.readWord;
  Result := magic = FLI_FRAME_MAGIC_NUMBER;
  if not Result then Exit;

  writeln('framesize = ',framesize);

  chunks := m_file.ReadWord;
  for i:=0 to 7 do       // Padding
    m_file.ReadByte();

  for i:=0 to chunks - 1 do begin
    writeln('frame ', m_frameCount,' chunk: ', i,' / ',chunks);
    readChunk(frame);
  end;

  m_file.Position := frameStartPos+frameSize;
  inc(m_frameCount);
  Result := true;

end;

procedure TDecoder.readChunk(var frame : TFrame);
var
  chunkStartPos : longWord;
  chunkSize     : longWord;
  _type : word;
begin
  chunkStartPos := m_file.position;
  chunkSize := read32();
  _type := read16();
  writeln(' chunk type: ',ChunkTypeToStr(_type),' (sz: ', chunkSize,')');
  case (_type) of
    FLI_COLOR_256_CHUNK: readColorChunk(frame, false);
    FLI_DELTA_CHUNK:     readDeltaChunk(frame);
    FLI_COLOR_64_CHUNK:  readColorChunk(frame, true);
    FLI_LC_CHUNK:        readLcChunk(frame);
    FLI_BLACK_CHUNK:     readBlackChunk(frame);
    FLI_BRUN_CHUNK:      readBrunChunk(frame);
    FLI_COPY_CHUNK:      readCopyChunk(frame);
  end;

  m_file.Position := chunkStartPos+chunkSize;
end;

procedure TDecoder.readBlackChunk(var frame: TFrame);
begin
  FillChar(frame.pixels[0], frame.rowstride*m_height, 0);
end;

procedure TDecoder.readCopyChunk(var frame: TFrame);
var
  y : integer;
  x : integer;
  it : integer;
begin
  //assert(m_width == 320 && m_height == 200);
  if ((m_width = 320) and (m_height = 200)) then begin
    for y:=0 to 200 -1 do begin
      it := y*frame.rowstride;
      for x:=0 to 320-1 do begin
        frame.pixels[it] := m_file.ReadByte;
        inc(it);
      end;
    end;
  end;
end;

procedure TDecoder.readColorChunk(var frame: TFrame; oldColorChunk: Boolean);
var
  npackets : integer;
  i        : integer;
  colors   : integer;
  j        : integer;
  color    : PColor;
begin
  npackets := read16();

  // For each packet
  i := 0;
  writeln('packets= ',npackets,' ',m_file.Position,' ',m_file.Size);
  while (npackets>0) do begin
    dec(npackets);
    i := i + m_file.ReadByte;       // Colors to skip

    colors := m_file.ReadByte;
    if (colors = 0) then
      colors := 256;


    for j:=0 to colors-1 do begin
           // If i+j >= 256 it means that the color chunk is invalid,
           // we check this to avoid an buffer overflow of frame.colormap[]
      if (i+j>=256) then break;
      color := @frame.colormap[i+j];
      color.r := m_file.readByte();
      color.g := m_file.readByte();
      color.b := m_file.readByte();
      if (oldColorChunk) then begin
        color.r := 255 * color.r div 63;
        color.g := 255 * color.g div 63;
        color.b := 255 * color.b div 63;
      end;
    end;
  end;
end;

procedure TDecoder.readBrunChunk(var frame: TFrame);
var
  y  : integer;
  it : integer;
  x  : integer;
  npackets : integer;
  count : integer;
  color : byte;
begin
  for y:=0 to m_height-1 do begin
    it := frame.rowstride*y;
    x := 0;
    npackets := m_file.ReadByte(); // Use the number of packet to check integrity
    while (npackets > 0) and (x < m_width) do begin
      dec(npackets);
      count := Int8( m_file.readByte);
      if (count >= 0) then begin
        color := m_file.ReadByte;
        while (count > 0) and (x < m_width) do begin
          frame.pixels[it] := color;
          inc(it);
          inc(x);
          dec(count);
        end;
      end
      else begin
        while (count < 0) and (x < m_width) do begin
          frame.pixels[it] := m_file.readByte();
          inc(it);
          inc(x);
          inc(count);
        end;
      end;
    end;
  end;
end;

procedure TDecoder.readLcChunk(var frame: TFrame);
var
  skipLines : integer;
  nlines    : integer;
  y         : integer;
  it        : integer;
  x         : integer;
  npackets  : integer;
  skip      : integer;
  count     : integer;
  _end      : integer;
  color     : byte;
begin
  skipLines := read16();
  nlines := read16();

  for y:=skipLines to skipLines+nlines - 1 do begin
    // Break in case of invalid data
    if (y < 0) or (y >= m_height) then
      break;

    it := frame.rowstride*y;
    x := 0;
    npackets := m_file.ReadByte;
    while (npackets >0) and (x < m_width) do begin
      dec(npackets);
      skip := m_file.ReadByte;

      inc(x,skip);
      inc(it, skip);

      count := int8(m_file.readByte);
      if (count >= 0) then begin
        _end := frame.rowstride*m_height;
        while (count > 0) and (it < _end) do begin
          frame.pixels[it] := m_file.readByte;
          inc(it);
          inc(x);
          dec(count);
        end;
        // Broken file? More bytes than available buffer
        if (it = _end) then
          Exit;
      end else begin
        color := m_file.ReadByte();
        while (count < 0) and (x < m_width) do begin
          frame.pixels[it] := color;
          inc(it);
          inc(x);
          inc(count);
        end;
      end;
    end;
  end;
end;

procedure TDecoder.readDeltaChunk(var frame: TFrame);
var
  nlines   : integer;
  y        : integer;
  npackets : integer;
  _word    : int16;
  it       : integer;
  x        : integer;
  count    : int8;
  color1   : integer;
  color2   : integer;
begin
  nlines := read16();
  y := 0;
  while (nlines > 0) do begin
    dec(nlines);
    npackets := 0;

    //while (m_file->ok()) {
    while (true) do begin
      _word := int16(read16());
      if (_word < 0) then begin          // Has bit 15 (0x8000)
        if ((_word and $4000) > 0) then begin   // Has bit 14 (0x4000)
          inc(y,  - _word);          // Skip lines
        end
        // Only last pixel has changed
        else begin
          //assert(y >= 0 && y < m_height);
          if ((y >= 0) and (y < m_height)) then begin
            it := y*frame.rowstride + m_width - 1;
            frame.pixels[it] := (_word and $ff);
          end;
          inc(y);
          dec(nlines);
          if (nlines= 0) then
            Exit;             // We are done
        end
      end
      else begin
        npackets := _word;
        break;
      end;
    end;

    // Avoid invalid data to skip more lines than the availables.
    if (y >= m_height) then
      break;

    x := 0;
    while (npackets > 0) do begin
      dec(npackets);
      inc(x, m_file.readByte);           // Skip pixels
      count := Int8(m_file.readByte); // Number of words

      //assert(y >= 0 && y < m_height && x >= 0 && x < m_width);
      it := y*frame.rowstride + x;

      if (count >= 0) then begin

        while (count > 0) and (x < m_width) do begin
          dec(count);
          color1 := m_file.readbyte();
          color2 := m_file.readbyte();

          frame.pixels[it] := color1;
          inc(it);
          inc(x);

          if (x < m_width) then begin
            frame.pixels[it] := color2;
            inc(it);
            inc(x);
          end;
        end;
      end
      else begin
        color1 := m_file.readByte;
        color2 := m_file.readByte;

        while (count <0) and (x < m_width) do begin
          inc(count);
          frame.pixels[it] := color1;
          inc(it);
          inc(x);
          if (x < m_width) then begin
            frame.pixels[it] := color2;
            inc(it);
            inc(x);
          end;
        end;
      end;
    end;

    inc(y);
  end;
end;

function TDecoder.read16();
begin
  Result := m_file.readWord;
end;

function TDecoder.read32();
begin
  Result := m_file.readDword;
end;

end.

unit helpzlib;

interface

uses Classes, SysUtils, zlibwin;

function zuncompress (dest : Pbyte;
                     var destLen : cardinal;
                     const source : array of byte;
                     sourceLen : cardinal) : integer;

function isGZHeader(b1, b2: byte): Boolean;
function ZlibMagicComment(b2: Byte): string;
const
  Zlib_NoCompr = $01;
  Zlib_FastCompr = $5e;
  Zlib_DefCompr  = $9c;
  Zlib_BestComp  = $Da;

implementation

function ZlibMagicComment(b2: Byte): string;
begin
  case b2 of
    Zlib_NoCompr: Result := 'No Compression';
    Zlib_FastCompr: Result := 'First Compression';
    Zlib_DefCompr: Result := 'Default Compression';
    Zlib_BestComp: Result := 'Best Compression';
  else
    result := 'Unknown '+IntToHex(b2, 2);
  end;
end;


function isGZHeader(b1, b2: byte): Boolean;
begin
  Result := (b1 = $78)
    and (
      (b2 = Zlib_NoCompr)
   or (b2 = Zlib_FastCompr)
   or (b2 = Zlib_DefCompr)
   or (b2 = Zlib_BestComp)
   )
end;

function zuncompress (dest : Pbyte;
                     var destLen : cardinal;
                     const source : array of byte;
                     sourceLen : cardinal) : integer;
var
  stream : z_stream;
  err : integer;
begin
  FillChar(stream, sizeof(stream), 0);
  stream.next_in := Pbytef(@source);
  stream.avail_in := cardinal(sourceLen);
  { Check for source > 64K on 16-bit machine: }
  if (cardinal(stream.avail_in) <> sourceLen) then
  begin
    zuncompress := Z_BUF_ERROR;
    exit;
  end;

  stream.next_out := PBytef(dest);
  stream.avail_out := cardinal(destLen);
  if (cardinal(stream.avail_out) <> destLen) then
  begin
    zuncompress := Z_BUF_ERROR;
    exit;
  end;

  err := inflateInit2_(stream, -15, ZLIB_VERSION, sizeof(z_stream));
  if (err <> Z_OK) then
  begin
    zuncompress := err;
    exit;
  end;

  err := inflate(stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    inflateEnd(stream);
    if err = Z_OK then
      zuncompress := Z_BUF_ERROR
    else
      zuncompress := err;
    exit;
  end;
  destLen := stream.total_out;

  err := inflateEnd(stream);
  zuncompress := err;
end;

end.

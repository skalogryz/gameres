unit helpzlib;

interface

uses Classes, SysUtils, zlibwin;

function zuncompress (dest : Pbyte;
                     var destLen : cardinal;
                     const source : array of byte;
                     sourceLen : cardinal) : integer;

implementation

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

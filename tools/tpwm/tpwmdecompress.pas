unit tpwmdecompress;

{$mode delphi}{$H+}

// This is the decompressor of TPWM (Turbo Packer by Wolfgang Mayerle)
// The source code is a port of the code from:
// https://moddingwiki.shikadi.net/wiki/TPWM_compression
//
// The article of TPWM: https://www.stcarchiv.de/tos1990/10/turbo-packer-samt-quelltext
// Google Translate from German into English:
//
// > How the packing algorithm works:
//
// > The packer works with a packing algorithm that shortens repeating byte
// > sequences with a length between 3 and 18 bytes to 2 bytes. In these 2 bytes,
// > the so-called unpacking information, the length of the sequence and the
// > relative offset (max. $fff bytes) are recorded (see Figure 1).
//
// > Longer sequences are therefore made up of several such unpacking
// > information. All other byte sequences remain unchanged.
//
// > In order to determine where in the packed file there is unpacking information
// > instead of normal data bytes, the packer inserts so-called pack bytes.
// > One bit tells the unpacker whether the following is unpacking information
// > or a data byte (see Figure 2).
// TPWM is used in the earlier BlueByte games (i.e. Battle Isle 1)

interface

uses
  Math, Classes, SysUtils;

// returns true, if the header was found.
//   newOfs would point to the offset in "buf" where the data stats
//   unpackSize would have the value of unpac kdata
function ParseHeader(const buf: array of byte; var dataOfs: integer; var unpackSize: integer): Boolean;

// This should be called afte rread
procedure Decompress(const buf: array of byte; dataOfs: integer; var dst: array of byte; unpackSize: integer);

implementation

function ParseHeader(const buf: array of byte; var dataOfs: integer;
  var unpackSize: integer): Boolean;
begin
  Result := (buf[dataOfs]=byte('T'))
    and (buf[dataOfs+1]=byte('P'))
    and (buf[dataOfs+2]=byte('W'))
    and (buf[dataOfs+3]=byte('M'));

  if not Result then begin
    unpackSize := 0;
    Exit;
  end;
  inc(dataOfs, 4);
  unpackSize := PInteger(@buf[dataOfs])^;
  inc(dataOfs, 4);
end;

procedure Decompress(const buf: array of byte; dataOfs: integer; var dst: array of byte; unpackSize: integer);
var
  i, j, c  : integer;
  pack_byte : byte;
  b1    : byte;
  b2    : byte;
  distance : integer;
  count    : integer;
  bit : integer;
begin
  FillChar(dst[0], unpackSize, $ff);
  i := dataOfs;
  j := 0;

  while i < length(buf) do begin
    pack_byte := buf[i];
    inc(i);
    if (pack_byte = 0) then begin
      move(buf[i], dst[j], 8);
      inc(i,8);
      inc(j,8);
    end else begin
      for bit := 0 to 7 do begin
        // if ((j >= unpackSize) or (i>=length(buf))) then Exit;
        if (pack_byte and ($80 shr bit)) <> 0 then begin
          b1 := buf[i]; inc(i);
          b2 := buf[i]; inc(i);
          distance := ((b1 and $f0) shl 4) or b2;
          count := b1 and $f + 2;
          for c := 0 to count do begin
            if (j>=unpackSize) then Exit;
            dst[j] := dst[j-distance];
            inc(j);
          end;
        end else begin
          dst[j] := buf[i];
          inc(i);
          inc(j);
        end;
      end;
    end;
  end;
end;

end.


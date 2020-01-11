unit huffmantree;
// Huffman code utilities
// Heavily used in variaty of compression algorithms in different ways

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  THuffmanCode = record
    bitlen : Integer;
    code   : Longword;
  end;

  THuffmanTree = record
    codes : array of THuffmanCode;
    count : integer;
  end;

procedure HuffmanInitTree(out t: THuffmanTree);

// Populates Huffman codes based on the given "lengths" (or "depths").
// Codes[i].bitlen must be populated.
// The codes are assigned per rules stated in default (Zlib) documentation (RFC1951)
//  * All codes of a given bit length have lexicographically
//    consecutive values, in the same order as the symbols
//    they represent;
//  * Shorter codes lexicographically precede longer codes.
// DoinverseBits keeps code in the manner easier for the comparison with a bitstream
procedure HuffmanCodesByLens(var codes: array of THuffmanCode; count: integer; doInverseBits: boolean = true); overload;
// .count must be populated. Codes[i].bitlen must be populated
procedure HuffmanCodesByLens(var t: THuffmanTree; doInverseBits: boolean = true); overload;

implementation

procedure HuffmanInitTree(out t: THuffmanTree);
begin
  t.count:=0;
  SetLength(t.codes,0)
end;

function inverse_bits(value: uint32; count: integer): LongWord;
var
  i : integer;
begin
  i := 0;
  while (count>0) do begin
    i := i shl 1;
    if (value and 1 > 0) then i := i or 1;
    value := value shr 1;
    dec(count);
  end;
  Result:=i;
end;

procedure HuffmanCodesByLens(var codes: array of THuffmanCode; count: integer; doInverseBits: boolean);
var
  bits_count : Integer;
  i    : integer;
  pfx  : integer;
begin
  pfx:=0;
  for bits_count := 1 to 16 do begin
    for i := 0 to count- 1 do
      if (codes[i].bitlen = bits_count) then begin
        if doInverseBits then
          codes[i].code := inverse_bits(pfx, bits_count)
        else
          codes[i].code := pfx;
        inc(pfx);
      end;
    pfx := pfx shl 1;
  end;
end;

procedure HuffmanCodesByLens(var t: THuffmanTree; doInverseBits: boolean); overload;
begin
  HuffmanCodesByLens(t.codes, t.count, doInverseBits);
end;

end.


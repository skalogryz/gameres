unit rncutils;
// Rob North Compression
//
// https://segaretro.org/Rob_Northen_compression
// https://icculus.org/libsyndicate/
//
// used by Bullfrog and Traveller's Tales

{$mode delphi}

interface

uses
  Classes, SysUtils, huffmantree;

type
  TRNCHeader = array [0..2] of char;

const
  RNC_HDR = 'RNC';

type

  { TRNCBitStream1 }

  // RNC1 bit stream (based on u16 words)
  TRNCBitStream1 = class(TObject)
  public
    Source: TStream;
    buf   : LongWord;
    bitSz : Integer;
    // makes sure, that the buffer is at least 16 bits
    procedure ReserveBits;
    function ReadBits(n: Integer): LongWord;
  end;

function AllocBitStream1(ASource: TStream): TRNCBitStream1;

// reads huffman tree format.
// 5 - bits for the count of leaf
// with 4 - bits for each length depth (following right after)
// Runs HuffmanValuesByLens right after reading bits
procedure ReadHuffmanTree(bs: TRNCBitStream1; out tree: THuffManTree);

// Reads the data from the bitstream, based on the huffman tree, earlier
// initialized. (used by RNC1)
function ReadTableData(v: TRNCBitStream1; const data: THuffmanTree): Longword;

// UnpackRNC1
// Doesn't calculate CRC.
// Doesn't validate the input
// Doesn't catch exceptions
// Returns false, if RNC header is not found
function UnpackRNC1(srcstream, dstream: TStream): Boolean;

implementation

function AllocBitStream1(ASource: TStream): TRNCBitStream1;
begin
  Result:=TRNCBitStream1.Create;
  Result.Source:=ASource;
end;

{ TRNCBitStream1 }

procedure TRNCBitStream1.ReserveBits;
var
  w: longword;
begin
  if bitSz<=16 then begin
    w:=LEtoN(Source.ReadWord);
    buf:=buf or (w shl bitSz);
    inc(bitSz, 2*8);
  end;
end;

function TRNCBitStream1.ReadBits(n: Integer): LongWord;
var
  m        : integer;
  nextRead : integer;
  p        : integer;
begin
  Result:=0;
  if (n=0) then Exit;
  p:=0;
  while n>0 do begin
    if (bitsz=0) then begin
      buf:=LEtoN(Source.ReadWord);
      bitSz:=2*8;
    end;

    nextRead:=n;
    if nextRead>bitsz then nextRead:=bitsz;

    m:=(1 shl nextRead)-1;
    Result:=Result or ((buf and m) shl p);
    buf := buf shr nextRead;

    dec(n, nextRead);
    dec(bitsz, nextRead);
    inc(p, nextRead);
  end;
end;

procedure ReadHuffmanTree(bs: TRNCBitStream1; out tree: THuffManTree);
var
  i : integer;
begin
  HuffmanInitTree(tree);
  tree.count := bs.ReadBits(5);
  //writeln('leads=',tree.count);
  if tree.count>16 then tree.count:=16; // hmm;

  if (tree.count=0) then Exit;
  setLength(tree.codes, tree.count);
  for i:=0 to tree.count-1 do
    tree.codes[i].bitlen:=bs.Readbits(4);

  HuffmanCodesByLens(tree);
end;

function ReadTableData(v: TRNCBitStream1; const data: THuffmanTree): Longword;
var
  i : longword;
begin
  i := 0;
  v.ReserveBits;
  while (true) do begin
    if ((data.codes[i].bitlen>0) and (data.codes[i].code = (v.buf and ((1 shl data.codes[i].bitlen) - 1))))
    then begin
      //writeln('  skip bits=',data.codes[i].bitlen,' match=', binStr(data.codes[i].code, data.codes[i].bitlen),' to ',
      //   binstr( v.buf and ((1 shl data.codes[i].bitlen) - 1), data.codes[i].bitlen)
      //   ,' i =',i,' ',data.codes[i].bitlen);
      v.ReadBits(data.codes[i].bitlen);
          //input_bits_m1(v, data[i].bit_depth);
      if (i < 2) then
        Result := i  // huh?
      else
        Result := v.Readbits(i - 1) or (1 shl (i - 1));
      exit;
    end;
    inc(i);
  end;
end;


function UnpackRNC1(srcstream, dstream: TStream): Boolean;
var
  hdr    : TRNCHeader;
  m      : byte;
  sz     : LongWord;
  packsz : LongWord;
  leeway : Byte;
  cnt    : byte;
  bs     : TRNCBitStream1;
  t      : integer;

  lit : THuffmanTree; // literals
  ofs : THuffmanTree; // offset
  dst : THuffmanTree; // destinations

  subs : Integer;

  datalen : integer;

  mofs : integer;
  mcnt : integer;

  w   : TMemoryStream;
  wb  : array of byte;

  j   : integer;
  i   : integer;
begin
  Result := false;
  wb:=nil;
  w:=TMemoryStream.Create;
  try
    srcstream.read(hdr, sizeoF(hdr));
    if hdr<>RNC_HDR then begin
      exit;
    end;
    m:=srcstream.ReadByte;
    sz:=BEtoN(srcstream.ReadDWord);
    packsz:=BEtoN(srcstream.ReadDWord);
    srcstream.ReadWord; // orig crc
    srcstream.ReadWord; // comp crc
    srcstream.ReadByte; // leeway
    srcstream.ReadByte; // cnt

    bs :=AllocBitStream1(srcstream);
    t:=bs.ReadBits(1); // encryption related keys
    t:=bs.ReadBits(1); //

    while (w.Size < sz) do begin
      ReadHuffmanTree(bs, lit);
      ReadHuffmanTree(bs, ofs);
      ReadHuffmanTree(bs, dst);

      subs:=bs.ReadBits(16);

      while (subs>0) do begin
        dec(subs);
        datalen := ReadTableData(bs, lit);
        if datalen>0 then begin
          // This is respecting the data alignments
          if bs.bitSz>=16 then begin
            bs.Source.Position:=bs.Source.Position-2;
            dec(bs.bitSz, 16);
            bs.buf:=bs.buf and ((1 shl bs.bitSz)-1);
          end;
          w.CopyFrom(bs.Source, datalen);
          bs.ReserveBits;
        end;

        if (subs>0) then begin
          mofs := ReadTableData(bs, ofs) + 1;
          mcnt := ReadTableData(bs, dst) + 2;
          if mcnt>0 then begin
            if length(wb)<mcnt then SetLength(wb, mcnt);

            // the combination of offset + count can span beyond the window size
            // in this case, the buffer should "wrap" the contents.
            // It's easily implemented, when doing byte-by-byte copy.
            // Yet might be not the most effecient implementation
            i:=0;
            repeat
              w.Position:=w.Size-mofs;
              j:=w.Read(wb[i], mcnt-i);
              inc(i,j);
            until i>=mcnt;
            w.Position:=w.Size;
            w.Write(wb[0], mcnt);
          end;
        end;
      end;
    end;

    w.Position:=0;
    dstream.CopyFrom(w, w.Size);
    Result := true;
  finally
    w.Free;
  end;
end;

end.


unit bmfile;
// The parser for "Blood and Magic" (1996 game) .STF files
// The sources for the game are presumably available on AD&D forgotten realms archives CD
// The cd was released in 1997... and I don't have it! (no surprise)

// STF files can be read consequently. However, the game also provides INDEX
// files that help to find a file by ID and identify it's type and offset in .STF file

// Index files are also indexes in MAP.IDX file. But it's pretty useless, as it only
// indicates the number of entries in the file.
// (which can be calcualted as indexes consists of fixed size records)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMapHeader = packed record
    idx    : shortint;
    size   : integer;
    offset : integer;
  end;

  TSTFHeader = packed record
    id      : word;
    res1    : longword;
    v1, v2  : Integer;
  end;


  TSTFFile = class(TObject)
    hdr : TSTFHeader;
  end;

  TSTFMapEntry = packed record
  case byte of
  0:(
    tp     : byte;     // type of the file (can be read in the file header)
    id     : longword; // id of the file (can be read in the file header)
    w2     : byte;     // (always 1)
    res1   : byte;     // (always 0)
    offset : longword; // offset in .stf file
    res2   : array [0..$6f-1-11] of byte;
    );
   1:(buf : array [0..$6f-1] of byte;)
  end;

  { TSTFMap }

  TSTFMap = class(TObject)
    ent : array of TSTFMapEntry;
    cnt : integer;
    procedure Sort;
  end;

  // This header preceeds every entry of the .STF file
  TInSTFFileHeader = packed record
    id      : Longword;  // id of the file (matches the id in .MAP file)
    fl      : Word;      // ?
    size1   : LongWord;  // size1
    size2   : LongWord;  // size2 (some times it's 0) packsize?
    tp      : Word;      // type of the file (matches the type in .MAP file)
    w2      : Word;      //
    extrasz : Word;      // extra bytes following the header! and prior the main data.
                         // should be read together with the data.
                         // used for images files only.
    w3, w4  : Word;      //
    w5  : Word;
    w6  : Word;      // always $FEFE
  end;

const
  TYPE_PIC        = 1; // images (static and sprites) (size is not specified in the file, so you've to know ahead of time!)
  TYPE_MIDI       = 3; // music (midi) files
  TYPE_WAVE       = 4; // straight .wav format!
  TYPE_PAL        = 5; // palette. it's always size of 768 (256 * 3)
  TYPE_FONT       = 7; // font (sizes are stored at extra)
  TYPE_TEXT       = 8; // typically provides translation
                          // id file
                          // 0 - english
                          // 1 - german
                          // 2 - french
  TYPE_ANMLIB    = 12; // index refernece library?
  TYPE_RES_INDEX = 18; // map library
  TYPE_UNITLIB   = 19; // unit library file

type
  // The array of the FRameInfo is provided with the "extra header" of every picture file
  // Even if the picture contains only 1 frame (i.e. a splash screen)
  TFrameInfo = packed record
    ofsx   : integer;
    ofsy   : integer; // 8
    width  : word;  // 10
    height : word;  // 12
    flags  : word;  // 14
    time   : longword; // 18
  end;
  TImageHeader = TFrameInfo;

//  MAP.IDX
// 0000000000: 6D 61 69 6E 00 00 00 00 │ 00 6F 00 6E 05 00 00 01  main     o n♣  ☺
// 0000000010: 00 00 63 69 6E 65 73 00 │ 00 00 00 6F 00 F5 01 00    cines    o õ☺
// 0000000020: 00 01 00 00             │                           ☺

//                                 [Files      ][?]
// 6D 61 69 6E 00 00 00 00 00 6F 00 6E 05 00 00 01 00 00       56E = 1390    6f = 111
// 63 69 6E 65 73 00 00 00 00 6F 00 F5 01 00 00 01 00 00       1f5 = 501

procedure ReadStream(asrc: TStream; dst: TSTFFile);
procedure ReadMAPStream(asrc: TStream; dst: TSTFMap);
function ReadFile(const fn: string; dst: TSTFFile; map: TSTFMap): Boolean;

(*
  This comes with RES_CFG.hpp file (that comes with the game)

    /// resources   // tp   ext     flags
    RES_CEL,        // 0    cel     .
    RES_ANIM,       // 1    ani     .
    RES_PIC,        // 2    pic     .
    RES_MIDI,       // 3    hmp     . H M
    RES_DAC,        // 4    wav     . H M
    RES_PAL,        // 5    pal     .
    RES_DATA,       // 6    dat     . S
    RES_FONT,       // 7    fon     . L
    RES_SQUIB,      // 8    sqb     .
    RES_CONV,       // 9    cnv     .
    RES_DCEL,       // 10   dcl     . S L
    RES_CHOREO,     // 11   cgf     .
    RES_BNK,        // 12   bnk     . H L M G
    RES_SYNCH,      // 13   syn     . H
    RES_CHUNK,      // 14   chu     .
    RES_ROUTE,      // 15   rot     .
    RES_SAVE,       // 16   sav     . H
    RES_FLIC,       // 17   flc     . H
    RES_TILELIB,    // 18   tlb     .
    RES_MAP_INFO,   // 19   mif     .
    RES_STUFF,      // 20   stf     .;stfs;D:\stfs
    RES_8TRACK,     // 21   8tr     .		     H
    RES_SMACKER,    // 22   smk     .;stfs;D:\stfs   H
    RES_SMK_SCRIPT, // 23   scr     .;stfs;D:\stfs   H
    RES_LAST        // 24

Format:

   DEFINE                  // number extension path [flag [...]]

The above defines are used in the TIGRE source code.  If the defines
themselves are changed, tigre.exe must be rebuilt.  If the attribute info
in the comment is changed, tigre.exe need not be rebuilt.


The attribute information must contain the file extension and path, and may
also contain:

  (G) - use a system grip, not a standard grip (will not get flushed on restore)
  (H) - headerless flag (this resource will not have a res header)
  (L) - a locked flag
  (M) - non-moveable flag
  (S) - saveable flag (these will be saved and restored)

RES_MIDI, RES_DAC, and RES_DATA use these optional flags.

*)

type
  TTextEntries = packed record
    offset  : integer;
    textIdx : integer;
  end;

  TTextMap = record
    count   : Integer;
    entries : array of TTextEntries;
    buf     : string;
    texts   : array of PChar;
  end;

procedure ReadTextMap(const s: TStream; var mp: TTextMap);

const
  // the index of the transparent color
  TRANSP_COLOR = $FE;
  PLAYER_COLOR_MIN = $C0;
  PLAYER_COLOR_MAX = $C7;

type
  // Fonts extra data starts with this header record.
  // The header is followed by the maxChar-minChar+1 count of TFontChar
  // describing every character
  TFontInfo = packed record
    flag1   : Word;
    Height  : Word;
    flag2   : Word;
    flag3   : Word;
    flag4   : Word;
    minChar : Word;
    maxChar : Word;
    flag5   : Word;
  end;

  // The descriptor for a single character
  TFontChar = packed record
    Width  : Word;    // The number of colmuns in the character
    Offset : Integer; // where the pixel starts
  end;

  // This is a high-level structure to read from "extra" header
  TFontExtraInfo = record
    info      : TFontInfo;
    chars     : array of TFontChar;
    charCount : integer;
  end;

type
  TUnitLibEntryInfo = record
    zero  : Integer;
    descr : array [0..75] of char;
  end;

procedure ReadFontExtaInfo(const s: TStream; var mp: TFontExtraInfo);

const
  TileWidth  = 20;
  TileHeight = 38;
  MaxLogTile = 128;

type
  TTileHeader = packed record
    zeros : Integer;
    count1 : Integer;
    count2 : Integer;
    buf    : array [0..16*4-1] of byte;
  end;

  TTileInfo = record
    name  : array [0..19] of char;
    moveCost   : integer; // positive
    flag2      : integer;
    flag3      : integer;
    defBonus   : integer; // defense bonus. can be negative
    healthCost : integer; // the damage caused
    flag6      : integer;
    flag7      : integer;
    iconId     : integer; // id of the icon file name
  end;

  TTile = packed record
    id    : integer;
    flag1 : integer;
    flag2 : integer;
    flag4 : byte;
    buf : array [0..TileWidth*TileHeight-1] of byte;
  end;
  PTile = ^TTile;


const
  DefaultPalette : array [0..256*3-1] of byte = (
   $00 ,$00 ,$00 ,$0F ,$0F ,$17 ,$1B ,$1B ,$27 ,$1F ,$1F ,$3B ,$2B ,$2B ,$4B,$37
  ,$37 ,$5B ,$43 ,$43 ,$6B ,$53 ,$53 ,$7B ,$67 ,$67 ,$8B ,$77 ,$77 ,$9B ,$8B,$8B
  ,$AB ,$9F ,$9F ,$BB ,$B7 ,$B7 ,$CB ,$CF ,$CF ,$DF ,$E7 ,$E7 ,$EF ,$FF ,$FF,$FF
  ,$0F ,$07 ,$07 ,$1F ,$13 ,$13 ,$2F ,$1F ,$1F ,$3B ,$27 ,$2B ,$4B ,$37 ,$3B,$5B
  ,$43 ,$47 ,$6B ,$53 ,$57 ,$7B ,$63 ,$6B ,$8B ,$73 ,$7B ,$9B ,$87 ,$8F ,$AB,$97
  ,$9F ,$BB ,$AB ,$B3 ,$CB ,$C3 ,$C7 ,$DF ,$D3 ,$D7 ,$EF ,$E7 ,$EB ,$17 ,$17,$17
  ,$23 ,$23 ,$23 ,$33 ,$33 ,$33 ,$43 ,$43 ,$43 ,$53 ,$53 ,$53 ,$63 ,$63 ,$63,$77
  ,$77 ,$77 ,$87 ,$87 ,$87 ,$97 ,$97 ,$97 ,$A7 ,$A7 ,$A7 ,$B7 ,$B7 ,$B7 ,$CB,$CB
  ,$CB ,$07 ,$0F ,$07 ,$13 ,$1F ,$13 ,$1B ,$2F ,$1B ,$27 ,$3F ,$23 ,$33 ,$4F,$2F
  ,$3F ,$5F ,$3B ,$4F ,$6B ,$47 ,$5F ,$7B ,$53 ,$73 ,$8B ,$63 ,$83 ,$9B ,$73,$93
  ,$AB ,$83 ,$A7 ,$BB ,$93 ,$BB ,$CB ,$A7 ,$CF ,$DB ,$BB ,$1F ,$3B ,$0B ,$27,$47
  ,$13 ,$33 ,$5B ,$1B ,$3F ,$6F ,$27 ,$4F ,$87 ,$2F ,$5F ,$9B ,$3B ,$6F ,$AF,$4B
  ,$7F ,$C3 ,$57 ,$93 ,$D7 ,$67 ,$A7 ,$EF ,$77 ,$B3 ,$FF ,$87 ,$0B ,$2B ,$2B,$0F
  ,$3B ,$37 ,$1B ,$4B ,$47 ,$23 ,$5B ,$57 ,$2F ,$6B ,$63 ,$3B ,$7B ,$73 ,$4B,$8B
  ,$7F ,$5B ,$9B ,$8F ,$6B ,$AB ,$9B ,$7F ,$BB ,$AB ,$8F ,$CB ,$BB ,$A7 ,$DB,$CB
  ,$BF ,$EB ,$DB ,$D7 ,$FB ,$EF ,$0B ,$0B ,$1F ,$13 ,$13 ,$43 ,$1F ,$1F ,$5B,$27
  ,$2B ,$77 ,$33 ,$37 ,$97 ,$37 ,$47 ,$A7 ,$4B ,$5B ,$B3 ,$57 ,$6F ,$BB ,$5F,$7F
  ,$CB ,$73 ,$97 ,$D7 ,$8B ,$AF ,$E3 ,$A3 ,$CB ,$EF ,$BF ,$E3 ,$FF ,$2F ,$23,$53
  ,$3F ,$2F ,$63 ,$57 ,$2B ,$83 ,$5F ,$3F ,$8F ,$77 ,$4F ,$A7 ,$8F ,$63 ,$CB,$9F
  ,$73 ,$D3 ,$B3 ,$8B ,$DB ,$C3 ,$9F ,$E7 ,$D7 ,$B3 ,$EF ,$E7 ,$CB ,$F7 ,$2B,$0B
  ,$1B ,$37 ,$0F ,$23 ,$47 ,$13 ,$2F ,$5B ,$1B ,$3F ,$6B ,$23 ,$4B ,$7F ,$2F,$5B
  ,$93 ,$3F ,$6F ,$A7 ,$53 ,$87 ,$BF ,$6B ,$9F ,$D3 ,$83 ,$B3 ,$E7 ,$9B ,$CB,$FF
  ,$BB ,$E7 ,$FF ,$CF ,$EF ,$33 ,$1B ,$13 ,$3F ,$1F ,$17 ,$4F ,$27 ,$1F ,$67,$33
  ,$2B ,$83 ,$3F ,$37 ,$9F ,$4F ,$43 ,$BB ,$5B ,$53 ,$CB ,$63 ,$63 ,$D3 ,$73,$7B
  ,$E3 ,$7F ,$83 ,$E3 ,$8F ,$93 ,$1F ,$00 ,$00 ,$2F ,$07 ,$07 ,$3F ,$07 ,$07,$4F
  ,$0B ,$0B ,$63 ,$0F ,$0F ,$77 ,$1F ,$13 ,$8B ,$23 ,$1B ,$A3 ,$33 ,$1F ,$B7,$43
  ,$27 ,$C7 ,$57 ,$2F ,$DF ,$6B ,$37 ,$F7 ,$83 ,$43 ,$FB ,$8F ,$57 ,$FF ,$9F,$6B
  ,$FF ,$AF ,$83 ,$47 ,$17 ,$0B ,$5F ,$1F ,$0F ,$77 ,$27 ,$13 ,$87 ,$33 ,$17,$97
  ,$47 ,$1B ,$A7 ,$5B ,$1F ,$BB ,$6F ,$23 ,$CB ,$87 ,$27 ,$DB ,$A3 ,$2B ,$EF,$BF
  ,$33 ,$F3 ,$DF ,$5F ,$F7 ,$F7 ,$8F ,$F3 ,$FB ,$BF ,$33 ,$23 ,$17 ,$43 ,$33,$1F
  ,$4F ,$47 ,$2B ,$63 ,$57 ,$37 ,$77 ,$67 ,$43 ,$8B ,$7B ,$4F ,$9F ,$8B ,$5B,$AF
  ,$9B ,$67 ,$C3 ,$AF ,$73 ,$D7 ,$BF ,$7F ,$EB ,$D3 ,$8B ,$53 ,$2F ,$13 ,$67,$33
  ,$1F ,$77 ,$3F ,$27 ,$87 ,$4F ,$2F ,$97 ,$5B ,$3B ,$A7 ,$6B ,$47 ,$B7 ,$7B,$53
  ,$C7 ,$8B ,$5F ,$D7 ,$97 ,$73 ,$EB ,$AB ,$83 ,$F7 ,$BB ,$9B ,$FF ,$CF ,$A7,$FF
  ,$DB ,$CF ,$FF ,$EF ,$E7 ,$AF ,$23 ,$23 ,$CB ,$27 ,$27 ,$DB ,$37 ,$37 ,$EB,$47
  ,$47 ,$F7 ,$53 ,$53 ,$FB ,$63 ,$63 ,$FB ,$7B ,$7B ,$FB ,$97 ,$97 ,$FB ,$B3,$B3
  ,$47 ,$0B ,$0B ,$6F ,$0B ,$0B ,$97 ,$0B ,$0B ,$BF ,$07 ,$07 ,$EB ,$00 ,$00,$FF
  ,$63 ,$63 ,$FF ,$8B ,$83 ,$FF ,$B7 ,$AF ,$13 ,$13 ,$43 ,$1B ,$23 ,$83 ,$23,$37
  ,$AF ,$3B ,$5B ,$C3 ,$3F ,$77 ,$EB ,$5B ,$93 ,$EB ,$83 ,$B7 ,$F3 ,$B3 ,$DB,$FF
  ,$2B ,$17 ,$17 ,$37 ,$1F ,$1B ,$3F ,$27 ,$23 ,$4B ,$2F ,$2B ,$57 ,$37 ,$2F,$63
  ,$43 ,$37 ,$6F ,$4B ,$3F ,$7B ,$57 ,$47 ,$87 ,$63 ,$4F ,$4F ,$17 ,$17 ,$5B,$1F
  ,$1B ,$67 ,$2B ,$23 ,$77 ,$3B ,$2B ,$83 ,$4B ,$33 ,$9F ,$6B ,$43 ,$AB ,$7F,$4B
  ,$BB ,$93 ,$57 ,$47 ,$3B ,$2B ,$5F ,$4F ,$3B ,$6B ,$5B ,$47 ,$83 ,$73 ,$5B,$8F
  ,$7F ,$63 ,$9F ,$8F ,$6F ,$4B ,$4B ,$63 ,$57 ,$57 ,$73 ,$67 ,$67 ,$87 ,$73,$73
  ,$9B ,$7F ,$7F ,$AF ,$8F ,$8F ,$BF ,$9B ,$9B ,$D3 ,$A7 ,$A7 ,$E7 ,$B7 ,$B7,$FB
  ,$1B ,$2F ,$27 ,$23 ,$3F ,$2F ,$2B ,$4B ,$37 ,$37 ,$5B ,$3F ,$3F ,$6B ,$47,$47
  ,$7B ,$4B ,$4F ,$8B ,$4F ,$A3 ,$33 ,$1F ,$BB ,$67 ,$2F ,$D7 ,$9F ,$43 ,$F3,$DF
  ,$5F ,$00 ,$00 ,$FF ,$FF ,$00 ,$00 ,$00 ,$00 ,$00 ,$00 ,$FF ,$00 ,$FF ,$FF,$FF
  );

implementation

procedure ReadStream(asrc: TStream; dst: TSTFFile);
begin
  //asrc.Read(dst.hdr, sizeof(dst.hdr));
  //writeln('id:  ', dst.hdr.id);
  //writeln('cnt: ', dst.hdr.res1);
  //writeln('v1:  ', dst.hdr.v1);
  //writeln('v2:  ', dst.hdr.v2);
{  dst.count := asrc.ReadWord;
  dst.size := asrc.ReadDWord;
  SetLength(dst.header, dst.Count);
  if dst.Count>0 then
    asrc.Read(dst.header[0], dst.count * sizeof(TICNSpriteHeader));

  len := Integer(dst.size) - Integer(LongWord(dst.count) * sizeof(TICNSpriteHeader));
  SetLength(dst.data, len);
  if len>0 then
    asrc.Read(dst.data[0], len);}
end;

procedure ReadMAPStream(asrc: TStream; dst: TSTFMap);
begin
  dst.cnt:=asrc.size div sizeof(TSTFMapEntry);
  SetLength(dst.ent, dst.cnt);
  asrc.Read(dst.ent[0], dst.cnt*sizeof(TSTFMapEntry));
  dst.Sort;
end;

function ReadFile(const fn: string; dst: TSTFFile; map: TSTFMap): Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      ReadStream(fs, dst);
    finally
      fs.Free;
    end;
    fs := TFileStream.Create(ChangeFileExt(fn,'.map'), fmOpenRead or fmShareDenyNone);
    try
      ReadMAPStream(fs, map);
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

{ TSTFMap }

type

  { TSortMapEntry }

  TSortMapEntry = class(TObject)
    owner : TSTFMap;
    idx   : Integer;
    ofs   : Integer;
    constructor Create(aidx: integer; aofs: Integer);
  end;

{ TSortMapEntry }

constructor TSortMapEntry.Create(aidx: integer; aofs: Integer);
begin
  idx := aidx;
  ofs := aofs;
end;

function CompareSortEntry(p1,p2: Pointer): Integer;
var
  s1: TSortMapEntry;
  s2: TSortMapEntry;
begin
  s1:=TSortMapEntry(p1);
  s2:=TSortMapEntry(p2);
  if s1.ofs = s2.ofs then Result :=0
  else if s1.ofs < s2.ofs then Result:=-1
  else Result :=1;
end;

procedure TSTFMap.Sort;
var
  l : TList;
  i : integer;
  res : array of TSTFMapEntry;
  s   : TSortMapEntry;
begin
  l := TList.Create;
  try
    for i:=0 to cnt-1 do
      l.Add( TSortMapEntry.Create(i, ent[i].offset));
    l.Sort(@CompareSortEntry);
    SetLength(res, l.Count);
    for i:=0 to l.Count-1 do begin
      s := TSortMapEntry(l[i]);
      res[i] := ent[s.idx];
    end;
    for i:=0 to l.Count-1 do
      TObject(l[i]).Free;

    ent := res;
  finally
    l.Free;
  end;
end;

procedure ReadTextMap(const s: TStream; var mp: TTextMap);
var
  i : integer;
begin
  mp.count := Integer(s.ReadDWord);
  if mp.Count<=0 then begin
    SetLEngth(mp.entries, 0);
    Exit;
  end;
  SetLength(mp.entries, mp.Count);
  s.Read(mp.entries[0], mp.Count * sizeof(TTextEntries));
  SetLength(mp.buf, s.Size - s.Position);
  s.Read(mp.buf[1], length(mp.buf));
  SetLength(mp.texts, mp.Count);
  for i := 0 to mp.Count-1 do
    mp.texts[i] := @mp.buf[mp.entries[i].offset+1];
end;

procedure ReadFontExtaInfo(const s: TStream; var mp: TFontExtraInfo);
begin
  s.Read(mp.info, sizeof(mp.info));
  mp.charCount := (mp.info.maxChar-mp.info.minChar)+1;
  SetLength(mp.chars, mp.charCount);
  s.Read(mp.chars[0], length(mp.chars)*sizeof(TFontChar));
end;

end.


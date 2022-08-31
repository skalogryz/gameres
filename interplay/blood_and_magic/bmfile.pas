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

end.


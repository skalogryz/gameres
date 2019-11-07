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
  TSTFHeader = packed record
    id      : word;
    res1    : longword;
    v1, v2  : Integer;
  end;


  TSTFFile = class(TObject)
    hdr : TSTFHeader;
  end;

  TSTFMapEntry = packed record
    tp     : byte;     // type of the file (can be read in the file header)
    id     : longword; // id of the file (can be read in the file header)
    w2     : byte;     // unknown
    res1   : byte;     // unknown
    offset : longword; // offset in .stf file
    res2   : array [0..$6f-1-11] of byte;
  end;

  TSTFMap = class(TObject)
    ent : array of TSTFMapEntry;
    cnt : integer;
  end;

  TFileHeader = packed record
    id      : Longword;  // id of the file
    fl      : Word;      // ?
    size1   : LongWord;  // size1
    size2   : LongWord;  // size2 (some times it's 0)
    tp      : Word;      // type of the file
    w2      : Word;      //
    extrasz : Word;      // extra bytes following the header! and prior the main data.
                         // should be read together with the data.
                         // used for images files only.
    w3, w4  : Word;      //
    w5, w6  : Word;      //
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
  TImageHeader = packed record
    ofsx : integer;
    ofsy : integer;
    width  : word;
    height : word;
    flags  : word;
    time   : longword;
  end;


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
end;

function ReadFile(const fn: string; dst: TSTFFile; map: TSTFMap): Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      writeln('r');
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


end.


unit prjfile;

interface

{$mode delphi}{$h+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes;

// PRJ+MAP files are used in Activision Heavy Gear game

// PRJ - is the file that stores all content files. In zlib-ed manner
// MAP - the file that contains the name and offsets of the file.
//
// The PRJ and MAP files have the same file size, but different extension:
// * HEAVYG.MAP
// * HEAVYG.PRJ
//
// While PRJ file doesn't have the file names, it still contains
// enough information to extract each file separately.
//
// The zlib streams in PRJ file start with the 2-bytes Zlib magic meaders
// indicating the compression method used (i.e. 0x789C) and thus
// should be skipped before trying to uncompressed.

type
  TPrjFileHeader = packed record
     // type name? null terminated
    typeName : array [0..7] of char; // C:\PRJ\x  - where "x" is some letter
    ext      : array [0..3] of char;
    unk0     : Int16; // flag indicating the pack method? always 0x0002
    realSize : Int32; // real size (after the file is uncompressed)
    packSize : Int32; // pack size includes the 2 bytes magic header
  end;

  // The map file represents the array of file types and the list
  // of compression libraries used, followed by the buffer of names
  // and the file offsets


  TMapFileHeader = packed record
    filesCount : integer;
    typesCount : integer;
  end;

  // if "Ext" name is empty, then it's a trailing
  // record. "count" seems to contain the number of
  // compression dlls used
  TMapTypeEntry = packed record
    ext   : array [0..3] of char;
    count : integer; // the number of files using this count
  end;

  // the name of the dll used for the compression
  TMapCompress = packed record
    dll  : array [0..15] of char;
  end;

  TMapFileName = packed record
    unk    : char; // don't know!. contains letters from 'A' to 'Z'
    name   : array [0..6] of char;
    ext    : array [0..3] of char;
    offset : integer; // the offset to the file header in Prj file
    none   : array [0..3] of char; // always 'NONE'
  end;

  TMap = class
    header    : TMapFileHeader;
    types     : array of TMapTypeEntry;
    compNames : array of string;
    files     : array of TMapFileName;
  end;

function ReadMap(st: TStream): TMap; overload;
procedure ReadMap(st: TStream; mp: TMap); overload;

implementation

function ReadMap(st: TStream): TMap;
begin
  result := TMap.Create;
  ReadMap(st, result);
end;

procedure ReadMap(st: TStream; mp: TMap);
var
  cnt     : integer;
  dllName : TMapCompress;
  i  : integer;
begin
  st.Read(mp.header, sizeof(mp.header));
  SetLength(mp.types, mp.header.typesCount);
  st.Read(mp.types[0], mp.header.typesCount * sizeof(TMapTypeEntry));
  st.Position:=st.Position+4;
  cnt := st.ReadDWord;
  SetLength(mp.compNames, cnt);
  for i:=0 to cnt-1 do begin
    st.Read(dllName, sizeof(dllName));
    mp.compNames[i] := dllName.dll;
  end;
  SetLength(mp.files, mp.header.filesCount);
  st.Read(mp.files[0], mp.header.filesCount * sizeof(TMapFileName));
end;

end.

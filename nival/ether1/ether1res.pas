unit ether1res;

{$mode delphi}

interface 

type
  TResHeader = packed record
    id       : array [0..3] of byte; // 3C E2 9C 01   
    count    : integer; // total files count
    entryofs : integer; 
    namesz   : integer;
  end;

  TEntryInfo = packed record
    flag1   : integer;
    size    : integer;
    offset  : integer;
    flag2   : integer;
    namelen : word;
    nameofs : integer;
  end;

  TFigHeader = packed record
    id        : array [0..3] of char; // FIG1
    count1    : integer;
    count2    : integer; // looks like the same count1
    uvcount   : integer; // "the number of uvs" before the list of indecis
    fcount    : integer; // some number that indiates the number of indiices
    uvcount2  : integer; //  
    unk1      : integer; // unknown 1 = count1 * 4
    unk2      : integer; // unknown 2 = always zero?
    unk3      : integer; // unknown 3 = usually 0x12 or 0x17
    unk4      : integer; // unknown 4 = 1 or 8 or 0x0D
  end;

  TFigVertex = packed record
    x,y,z  : single;
  end;

const
  VtxEntrySize = 12 * 4;
  VtxExtraSize = 12 * 3 + 4;
  CntEntrySize = 16 * 4;
  
implementation

end.                                   
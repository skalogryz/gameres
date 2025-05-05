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
    id     : array [0..3] of char; // FIG1
    count1 : integer;
    count2 : integer;
    count3 : integer;
    vcount : integer; // vertex count
    count5 : integer;
    count6 : integer;
    count7 : integer;
    count8 : integer;
    count9 : integer; 
  end;
  TFigVertex = packed record
    x,y,z  : single;
  end;


implementation

end.
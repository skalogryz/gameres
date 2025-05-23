unit zfsfile;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  THeader = packed record
    id    : array [0..3] of char;
    u1    : int32;
    u2    : int32;
    u3    : int32;
    count : int32;
    u5    : int32;
    u6    : int32;
  end;

  TFileEntry = packed record
    f0 : int32;
    f1 : int32;
    name :array [0..15] of char;
    ofs : int32;
    f6  : int32;
    size : int32;
  end;

implementation

end.


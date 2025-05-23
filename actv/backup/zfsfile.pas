unit zfsfile;
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

// Zfs files are used in a number of Activision games released in the second half of 90s
// Most noteble
//   Interstate 76 (82)
//   Battlezone 1 (2)
//   
// The structure goes as following:
//
// Header
// ListHeader (references to the next ListHedaer)
//   FilesList (no more than 100 files per list. If contains less than 100 files. then the files list is truncated)
// ... files..
// ListHeader
//   FilesList
// ... files
//
// External descriptions:
//   https://gameextractor.sourceforge.net/program/archives.txt
//   https://www.watto.org/specs.html?specs=Archive_ZFS_ZFS
//   https://github.com/BattlezoneUtilities/unzfs/blob/master/main.cpp 	
//

type
  // The file starts with the header
  // 
  THeader = packed record
    id    : array [0..3] of char; // ZSFS
    u1    : int32;
    u2    : int32;
    u3    : int32;
    count : int32;
    u5    : int32;
    u6    : int32; // always C0
  end;

  TFileEntry = packed record
    // null terminated file name. 
    name   : array [0..15] of char; 
    ofs    : int32; // the file offset
    seqnum : int32; // sequence number of file. starts with 0, and increase by 1 on every file
    size   : int32; // the file size in the zfs 
    unk0   : int32; // 
    //f0     : int32; //
    cmpflag: byte;    // compression flag
                      // 0 - no compression, 
                      // 2 - LZO1X
                      // 4 - LZO1Y
    cmpsize: int16;
    unk1   : byte;    // unknown
  end;

  TFilesList = packed record
    nextList  : int32;
    files     : array [0..99] of TFileEntry;
  end;

implementation

end.


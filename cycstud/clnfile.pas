unit clnfile;
{$mode delphi}{$h+}

interface

// Cyclone Games storage file.
// used in Uprising and Uprising 2 games

uses Classes, SysUtils;

// "cln" file starts with
//   int32 - the number of files
// for earch file there information is the following
//   int32 - the length of the file name
//   n-bytes - the actual name (not null terminated
//   int32 - the size of the file
//   int32 - attributes flags of the file
//   int32 - the offset in the cln file, from the start of the file


const
  ATTR_DIR = $01; // directory
  MAGIC_NUMBER = 'MAGIC NUMBER';


type
  TFileAfterName = packed record
    size     : integer; // the file size.
                        // The size doesn't include "MAGIC NUMBER" header

    packsize : integer; // it's 1 for directories.
                        // for unpacked file it's 0
                        // for packed file it's amount of the bytes that needs to be read
                        // from the file in order to unpack.
                        // The size doesn't include "MAGIC NUMBER" header
    offset   : integer; // The offset in the file where the file starts after "MAGIC NUMBER"
                        // The actual offset is offset+len("MAGIC NUMBER")
  end;

  TFileEntry = class(TObject)
  public
    fileName: string;
    info    : TFileAfterName;
  end;

procedure ReadFiles(src: TStream; dst: TList);

// a "small" run-length encoding is used, when the file packed.
// in the following manner:
//   byte - the number of times the next byte needs to be repeated.
//          (quite often this value is "1" meeaning just read the next byte
//           and put it to the output stream)
//   byte - the actual data, that needs to be written to the outpustream
// Once all packed data is written, check the number of bytes written
// if it's smaller than "size" specified in the file entry
// write the last "actual data" byte to the stream the number of times
// that's needed to match the size.
procedure ExtractFile(src: TStream; e: TFileEntry; dst: TStream);

implementation

procedure ExtractFile(src: TStream; e: TFileEntry; dst: TStream);
var
  p : int64;
  sz : integer;
  ch : array [0..length(MAGIC_NUMBER)-1] of char;
  unpack : Boolean;
  mem : TMemoryStream;
  b   : byte;
  b2  : byte;
  total : integer;
  j   : integer;
  z4  : integer;
begin
  p := src.Position;
  try
    src.Position := e.info.offset;

    if (e.info.packsize> 0) then begin
      sz := e.info.packsize;
      unpack := true;
    end else
      sz := e.info.size;

    src.Read(ch, sizeof(ch));
    //if (ch = MAGIC_NUMBER) then dec(sz, length(MAGIC_NUMBER))
    // else
    //src.Position := e.info.offset;

    if not unpack then begin
      dst.CopyFrom(src, sz);
    end else begin
      mem := TMemoryStream.Create;
      total := 0;
      while (sz > 0) do begin
        src.Read(b, 1);
        src.Read(b2, 1);
        dec(sz, 2);
        for j := 1 to b do begin
          mem.write(b2, sizeof(b2));
        end;
        //src.Position:=src.Position+1;
        inc(total, b);
      end;

      if (mem.Size < e.info.size) then begin
        j := e.info.size - mem.Size;
        // filling the last character
        FillChar(z4, sizeof(z4), b2);
        while j > 4 do begin
          mem.Write(z4, sizeof(z4));
          dec(j,4);
        end;
        while (j > 0) do begin
          mem.write(b2, sizeof(b2));
          dec(j);
        end;
      end;

      mem.Position := 0;
      dst.CopyFrom(mem, mem.Size);
      mem.Free;
    end;
  finally
    src.Position := p;
  end;

end;

procedure ReadFiles(src: TStream; dst: TList);
var
  i : integer;
  l : integer;
  c : integer;
  nm : string;
  fe  : TfileEntry;
begin
  src.Read(c, sizeof(c));
  for i := 0 to c-1 do begin
    src.Read(l, sizeof(l));
    SetLength(nm, l);
    src.Read(nm[1], l);
    fe := TfileEntry.Create;
    fe.fileName := nm;
    src.Read(fe.info, sizeof(fe.info));
    dst.Add(fe);
  end;
end;

end.

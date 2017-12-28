unit bf_dattabutils;

interface

uses
  SysUtils, Classes, bf_dattab;

type
  TTabArray = record
    cnt   : Integer;
    items : array of TTabEntry;
  end;

function TabsReadFromStream(st: TStream; var ar: TTabArray): Boolean;
function TabsReadFromFile(const fn: AnsiString; var taba: TTabArray): Boolean;
procedure TabsDump(const ta: TTabArray);

procedure TabsDumpImages(const ta: TTabArray; dat: TStream);

implementation

function TabsReadFromStream(st: TStream; var ar: TTabArray): Boolean;
var
  i : integer;
begin
  i:=0;
  if st.Position<st.Size-sizeof(TTabEntry) then begin
    // the first one is presumed to be empty
    st.Position:=st.Position+sizeof(TTabEntry);
    while st.Position<=st.Size-sizeof(TTabEntry) do begin
      if i=length(ar.items) then begin
        if i=0 then SetLength(ar.items, 8)
        else setLength(ar.items, i*2);
      end;
      st.Read(ar.items[i], sizeof(TTabEntry));
      inc(i);
    end;
    Result:=true
  end else
    Result:=false;
  ar.cnt:=i;
end;

function TabsReadFromFile(const fn: AnsiString; var taba: TTabArray): Boolean;
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    Result:=TabsReadFromStream(fs, taba);
  finally
    fs.Free;
  end;
end;

procedure TabsDump(const ta: TTabArray);
var
  i : integer;
begin
  writeln('tabs: ', ta.cnt);
  for i:=0 to ta.cnt-1 do begin
    writeln(ta.items[i].offset,' ', ta.items[i].width, ' ',ta.items[i].Height);
  end;
end;

procedure TabsDumpImages(const ta: TTabArray; dat: TStream);
var
  i : integer;
  src : array of byte;
  buf : array of byte;
  x,y: integer;
  d  : integer;
begin
  //todo: what if dat file is too big? nah... never happened
  SetLength(src, dat.Size);
  dat.Read(src[0], dat.Size);

  for i:=0 to ta.cnt-1 do begin
    SetLength(buf, ta.items[i].width*ta.items[i].height);
    FillChar(buf[0], length(buf), 0);
    BFDecodeRLEOffset(ta.items[i].width, ta.items[i].height, src, ta.items[i].offset, buf);

    d:=0;
    for y:=0 to ta.items[i].height-1 do begin
      for x:=0 to ta.items[i].width-1 do begin
        if buf[d]=0 then write('..')
        else write(IntToHex(buf[d],2));
        inc(d);
      end;
      writeln;
    end;
  end;
end;

end.

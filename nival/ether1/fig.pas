{$mode delphi}
uses SysUtils, Classes, ether1res;

procedure UnfigFiles(const fn: string);
var
  fs    : TFileStream;
  hdr   : TFigHeader;
  vtx   : array of TFigVertex;
  i,j   : integer;
  check : array [0..15] of single;
  cnt   : integer;  
  uv    : array [0..1] of single;
  ff    : array of single;
  extra : integer;
  w     : array of word;
begin
  fs :=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try 
    fs.Read(hdr, sizeof(hdr));  
    writeln(hdr.id);
    writeln('count1   = ', hdr.count1);
    writeln('         = ', hdr.count2);
    writeln('uvcount  = ', hdr.uvcount);
    writeln('fcount   = ', hdr.fcount);
    writeln('uvcount2 = ', hdr.uvcount2);
    writeln('unk1     = ', hdr.unk1);
    writeln('unk2     = ', hdr.unk2);
    writeln('unk2     = ', hdr.unk3);
    writeln('unk4     = ', hdr.unk4);  
   
    extra := hdr.count1 * VtxEntrySize + VtxExtraSize;
    SetLength(ff, extra div 4);
    fs.Read(ff[0], extra);
    writeln('---prefixed'); 
    for i := 0 to length(ff)-1 do
      writeln(ff[i]:0:6);

    j := 1;
    i := 0;
    while (i < length(ff)) do begin
      if (i mod 3 = 0) then begin
        writeln;
        writeln('# ', j);
        write('v ');
        inc(j);
      end;
      write(ff[i]:0:6,' ');
      inc(i);
    end;
    writeln;
    
    //fs.Position := fs.Position +  ;
    writeln('---starting at: ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
    for i := 0 to hdr.count1-1 do begin
      fs.Read(check[0], CntEntrySize);
      write(i,': ');
      for j := 0 to length(check)-1 do 
         write(check[j]:0:6,' ');
      writeln;
      // writeln(check[12]:0:6 ,' ',check[13]:0:6,' ',check[14]:0:6,' ',check[15]:0:6)
    end;
    writeln('--uvs: (',hdr.uvcount,')');
    for i := 0 to hdr.uvcount-1 do begin
      fs.Read(uv[0], sizeof(uv));
      writeln(i,': ', uv[0]:0:6 ,' ',uv[1]:0:6);
    end;

    cnt := fs.Size-fs.Position;
    writeln('starting at: ', fs.Position,' ',IntToHex(fs.Position,8));
    writeln('size left: ' ,cnt);
    writeln('indecies:  ', cnt div 2);
    writeln('pairs:     ', cnt div 4);
    SetLength(w, cnt div 2);
    fs.Read(w[0], cnt);
    for i := 0 to length(w)-1 do 
      writeln(w[i]);

    for i := 0 to hdr.fcount-1 do begin
      if (i mod 3 = 0) then begin
        if (i >0) then write(w[i-3]+1);
        writeln;
        write('l ');
      end;
      write(w[i]+1,' ');
    end;
    write(w[hdr.fcount-3]+1);
    writeln;
  finally 
    fs.free;
  end;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
     writeln('please provide input res file name');
     exit;
  end;
  fn := Paramstr(1);
  UnfigFiles(fn);
end.
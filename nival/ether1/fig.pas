{$mode delphi}
{$rangechecks on}
uses
  SysUtils, Classes, ether1res, unfigutils,
  export3d, objwaveexport;

type
  TVertexIndex = array [0..2] of word;
  TVertexDescr = packed record  
     coord : word; 
     norm  : word; 
     uv    : word; 
  end;
  TUVIndex = array [0..1] of word; // some extra
  TCoord = array [0..2] of single;
 
procedure GetMinMax(const w: array of word; out min, max: word); forward;

procedure DumpVtxArr(const ff: array of single; const initFn: string; const stp: single = 0.01);
var 
  fn : string;
  f  : Text;
  i  : integer;
  j, k : integer;
  x, y,z: single;
  vn : integer;
begin
  fn := ChangeFileExt(initFn, '.obj');
  AssignFile(f, fn); Rewrite(f);
  j := 0;
  vn := 1;
  for i := 0 to (length(ff) div 3) - 1 do begin
    x:=ff[j]; inc(j);
    y:=ff[j]; inc(j);
    z:=ff[j]; inc(j);
    writeln(f,'v ',x:0:6,' ',y:0:6,' ',z:0:6);
    writeln(f,'v ',(x+stp):0:6,' ',y:0:6,' ',z:0:6);
    writeln(f,'v ',x:0:6,' ',(y+stp):0:6,' ',(z+stp):0:6);
    write(f,'f '); 
    write(f, vn+0,' '); 
    write(f, vn+1,' '); 
    write(f, vn+2,' '); 
    writeln(f);
    inc(vn, 3);
  end;
  CloseFile(f);
end;

procedure FlipCoords(var ff: array of single; ofs: integer);
var
  tmp : array [0..11] of single;
begin
  writelN('flip coord: ', ofs,' length(ff)=',length(ff));
  tmp[0] := ff[ofs+0];
  tmp[1] := ff[ofs+4];
  tmp[2] := ff[ofs+8];

  tmp[3] := ff[ofs+1];
  tmp[4] := ff[ofs+5];
  tmp[5] := ff[ofs+9];

  tmp[6] := ff[ofs+2];
  tmp[7] := ff[ofs+6];
  tmp[8] := ff[ofs+10];

  tmp[9] := ff[ofs+3];
  tmp[10] := ff[ofs+7];
  tmp[11] := ff[ofs+11];

  Move(tmp[0], ff[ofs], sizeof(tmp));

end;

procedure UnfigFiles(const fn: string; skipFloats: integer = 0);
var
  fs    : TFileStream;
  hdr   : TFigHeader;
  i,j   : integer;
  check : array [0..15] of single;
  cnt   : integer;  
  uv    : array [0..1] of single;
  ff    : array of single;
  tf    : array of single;
  extra : integer;
  w     : array of word;
  
  faces : array of TVertexIndex;
  vtx   : array of TVertexDescr;
  u1ind : array of TUVIndex;

  floatbuf : integer;
  n,x      : word;
  coords   : array of TCoord;

  initOfs  : integer;
  restSize : integer;
  resCnt   : integer;
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

    // writeln('---after header ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
    // initOfs := skipFloats * sizeof(single);
    // writeln('initOfs = ',initOfs,' extra size: ', VtxExtraSize);
    // if (skipFloats > 0) then begin
    //   writeln('skipping prefix');
    //   SetLength(tf, skipFloats);
    //   fs.Read(tf[0], skipFloats * sizeof(Single));
    //   for i := 0 to length(tf)-1  do
    //     writeln(tf[i]:0:6);
    //   writeln('---- end of prefix');
    // end;
    //
    //
    // //floatbuf := hdr.count1 * VtxEntrySize + VtxExtraSize + hdr.count1 * CntEntrySize;
    // extra := hdr.count1 * VtxEntrySize; // + VtxExtraSize;
    // SetLength(coords, hdr.count1 * 4);
    // SetLength(ff, extra div 4);
    // fs.Read(ff[0], extra);
    // for i := 0 to length(ff)-1 do
    //   writeln(ff[i]:0:6);
    fs.Position := fs.Position + VtxExtraSize; // 10 * sizeof(single); // skipping unknown use

    SetLength(ff, hdr.count1 * 12);
    fs.Read(ff[0], length(ff)*sizeof(single));
     for i := 0 to length(ff)-1 do
       writeln(ff[i]:0:6);
    for i:=0 to hdr.count1-1 do begin
      FlipCoords(ff, i * 12);
    end;



    // restSize := VtxExtraSize-initOfs;
    // resCnt := restSize div sizeof(single);
    // if (resCnt >0) then begin
    //   writeln('---- start of postfix. count: ', resCnt,' size: ', restSize);
    //   SetLength(tf, resCnt);
    //   fs.Read(tf[0], resCnt * sizeof(Single));
    //   for i := 0 to length(tf)-1  do
    //     writeln(tf[i]:0:6);
    //   writeln('---- end of prefix');
    // end;

    //fs.Position := fs.Position +  ;
    writeln('---starting at: ', fs.Position,' ',IntToHex(fs.Position,8),' ---');
    for i := 0 to hdr.count1-1 do begin
      fs.Read(check[0], CntEntrySize);
      write(i,': ');
      for j := 0 to length(check)-1 do 
         write(check[j]:0:6,' ');
      writeln;
      // Move(check[0], coords[i*4],  sizeof(TCoord)*3*4);
      // writeln(check[12]:0:6 ,' ',check[13]:0:6,' ',check[14]:0:6,' ',check[15]:0:6)
    end;
    
    writeln('--uvs: (',hdr.uvcount,')');
    for i := 0 to hdr.uvcount-1 do begin
      fs.Read(uv[0], sizeof(uv));
      writeln(i,': ', uv[0]:0:6 ,' ',uv[1]:0:6);
    end;

    SetLength(faces, hdr.fcount div 3);
    fs.Read(faces[0], length(faces)*sizeof(TVertexIndex));

    SetLength(vtx, hdr.uvcount2); 
    fs.Read(vtx[0], length(vtx)*sizeof(TVertexDescr));

    SetLength(u1ind, hdr.unk1);
    fs.Read(u1ind[0], length(u1ind)*sizeof(TUVIndex));

    writeln('faces:');
    for i := 0 to length(faces)-1 do begin
      writeln('#',i,': ', faces[i][0], ' ',faces[i][1],' ', faces[i][2]);
    end;
    writeln;

    writeln('vertexes:');
    for i := 0 to length(vtx)-1 do begin
      writeln('#',i,': ', vtx[i].coord, ' ',vtx[i].norm,' ', vtx[i].uv);
    end;
    writeln;

    writeln('unks?');
    for i := 0 to length(u1ind)-1 do begin
      writeln('#',i,': ', u1ind[i][0], ' ',u1ind[i][1]);
    end;

    writeln;

    writeln('# file starts:');
    for i := 0 to length(vtx)-1 do begin
      j := vtx[i].coord;
      // writeln('# j   is ',j);
      write('v ');
      //write(ff[j*3]:0:6);
      //write(' ',ff[j*3+1]:0:6);
      //write(' ',ff[j*3+2]:0:6);

      write(ff[j*3]:0:6);
      write(' ',ff[j*3+1]:0:6);
      write(' ',ff[j*3+2]:0:6);


      //write(coords[j][0]:0:6);      
      //write(' ',coords[j][1]:0:6);
      //write(' ',coords[j][2]:0:6);
      writeln;
    end;
    writeln;
    for i:=0 to length(faces)-1 do begin
      write('l ');
      write(faces[i][0]+1);
      write(' ');
      write(faces[i][1]+1);
      write(' ');
      write(faces[i][2]+1);
      write(' ');
      write(faces[i][0]+1);
      writeln;
    end;
    DumpVtxArr(ff, fn);
  finally 
    fs.free;
  end;
end;

procedure UnfigFilesClass(const fn: string);
var
  fs : TFileStream;
  f  : TFigureData;
  obj : TWaveObjFileExport;
  m   : IMeshExport;
begin
  if (AnsiLowerCase(ExtractFileExt(fn)) = '.mod') then
  begin
    obj := TWaveObjFileExport.Create;
    ExportModel(fn, obj);
    writeln(obj.DumpString);
    exit;
  end;

  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  f := TFigureData.Create;
  try
    ReadFigure(fs, f);
    obj := TWaveObjFileExport.Create;

    m := obj.StartMesh('mesh');
    ExportFigure(f, m);
    obj.FinishMesh(m);

    m := obj.StartMesh('mesh2');
    ExportFigure(f, m);
    obj.FinishMesh(m);

    writeln(obj.DumpString);


    writeln('all ok');
  finally
    fs.Free;
    f.Free;
  end;
end;

procedure GetMinMax(const w: array of word; out min, max: word);
var 
  i : integer;
begin
  if (length(w)= 0) then begin 
    min := 0;
    max := 0;
    exit;
  end;
  min:=w[0];
  max:=min;
  for i := 0 to length(w)-1 do begin
    if (w[i] < min) then min := w[i]
    else if (w[i] > max) then max := w[i];
  end;
  
end;


procedure UnfigFilesPref(const fn: string);
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
  
  vind  : array of TVertexIndex;
  find  : array of TVertexIndex;
  u1ind : array of TUVIndex;

  floatbuf : integer;
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

    writeln('---after header ', fs.Position,' ',IntToHex(fs.Position,8),' ---'); 

    floatbuf := hdr.count1 * VtxEntrySize + VtxExtraSize + hdr.count1 * CntEntrySize;
    SetLength(ff, floatbuf div 4);
    fs.Read(ff[0], floatbuf);
    writeln('  float buffer: ', floatbuf);
    writeln('  count: ', length(ff));

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
    
    writeln('--uvs: (',hdr.uvcount,')');
    for i := 0 to hdr.uvcount-1 do begin
      fs.Read(uv[0], sizeof(uv));
      writeln(i,': ', uv[0]:0:6 ,' ',uv[1]:0:6);
    end;

    SetLength(vind, hdr.fcount div 3);
    fs.Read(vind[0], length(vind)*sizeof(TVertexIndex));
    
    SetLength(find, hdr.uvcount2); 
    fs.Read(find[0], length(find)*sizeof(TVertexIndex));

    SetLength(u1ind, hdr.unk1); 
    fs.Read(u1ind[0], length(u1ind)*sizeof(TUVIndex));

    writeln('faces?');
    for i := 0 to length(vind)-1 do begin
      writeln(i,': ', vind[i][0], ' ',vind[i][1],' ', vind[i][2]);
    end;
    writeln;

    writeln('faces?');
    for i := 0 to length(find)-1 do begin
      writeln(i,': ', find[i][0], ' ',find[i][1],' ', find[i][2]);
    end;
    writeln;

    writeln('uvs?');
    for i := 0 to length(u1ind)-1 do begin
      writeln(i,': ', u1ind[i][0], ' ',u1ind[i][1]);
    end;


    {    
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
    }
  finally 
    fs.free;
  end;
end;


procedure UnfigFilesOrig(const fn: string);
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

  floatbuf : integer;
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

    //floatbuf = hdr.count1 * VtxEntrySize + VtxExtraSize + hdr.count1 * CntEntrySize;
    
    extra := hdr.count1 * VtxEntrySize + VtxExtraSize;
    SetLength(ff, extra div 4);
    writeln('---after header ', fs.Position,' ',IntToHex(fs.Position,8),' ---'); 
    fs.Read(ff[0], extra);
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
  ofs : integer;
begin
  if ParamCount=0 then begin
     writeln('please provide input res file name');
     exit;
  end;
  fn := Paramstr(1);
  ofs := 0;
  if ParamCount>1 then begin
    ofs := StrToIntDef(ParamStr(2), ofs);
  end;
  //UnfigFiles(fn, ofs);
  UnfigFilesClass(fn);
end.

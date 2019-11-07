program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, bmfile
  { you can add units after this };

procedure RunFile(const fn: string);
var
  s: TSTFFile;
  m: TSTFMap;
  i   : integer;
begin
  s:=TSTFFile.Create;
  m:=TSTFMap.Create;
  try
    ReadFile(fn, s, m);
    for i:=0 to m.cnt-1 do begin
      write(m.ent[i].tp,' ', m.ent[i].id:13, IntToHex(m.ent[i].id,8):12,m.ent[i].offset:13,inTtoheX(m.ent[i].offset,8):13);
      if i<m.cnt-1 then
        writeln(' size: ',m.ent[i+1].offset-m.ent[i].offset);
      writeln;
    end;
  finally
    m.Free;
    s.Free;
  end;
end;

procedure RunFileHeader(const fn: string; extractFile: Boolean = false; extractExtra: Boolean = false);
var
  fs : TFileStream;
  h  : TFileHeader;
  dst : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    while fs.Position < fs.Size do begin
      fs.Read(h, sizeof(h));

      write('id: ', h.id,'; tp: ', h.tp);
      write('; size: ', h.size1,' (',h.size2,'); extra: ', h.extrasz);
      writeln;

      if extractExtra and (h.extrasz>0) then begin
        dst := TFileStream.Create(IntToStr(h.id)+'.'+IntToStr(h.tp)+'.extra', fmCreate);
        try
          dst.CopyFrom(fs, h.extrasz);
        finally
          dst.Free;
        end;
      end else
        fs.Position:=fs.Position+h.extrasz;

      if extractFile and (h.size1>0) then begin
        dst := TFileStream.Create(IntToStr(h.id)+'.'+IntToStr(h.tp), fmCreate);
        try
          dst.CopyFrom(fs, h.size1);
        finally
          dst.Free;
        end;
      end else
        fs.Position:=fs.Position+h.size1;
    end;
  finally
    fs.Free;
  end;
end;

procedure RunImageHeader(const fn: string);
var
  fs : TFileStream;
  h  : TImageHeader;
  i  : integer;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    i:=1;
    while fs.Position < fs.Size do begin
      fs.Read(h, sizeof(h));
      writeln('frame: ',i);
      writeln('  ofs:  ', h.ofsx,' ',h.ofsy,' flag: ', h.flags);
      writeln('  size: ', h.width,' ',h.height);
      writeln('  time: ', h.time);
      inc(i);
    end;
  finally
    fs.Free;
  end;
end;

begin
  if ParamCount=0 then begin
    writeln('please specify .stf file name');
    exit;
  end;
  //RunFile(ParamStr(1));
  //RunFileHeader(ParamStr(1));
  RunImageHeader(ParamStr(1));
end.


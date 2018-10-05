program icnreader;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Math, hmm2agg, hmm2utils
  ,fpimage, fpwritebmp;

procedure ReadICNFile(const fn: string);
var
  icn : TICNSpriteFile;
begin
  try
    icn := TICNSpriteFile.Create;
    try
      ICNReadFile(fn, icn);
      ICNDump(icn, true);
    finally
      icn.Free;
    end;
  except
    on e:exception do
      writeln('crash: ', e.message);
  end;
end;

procedure ICNToFPImage(px: TICNPixels; const pal: THMMPalette; dst: TFPCustomImage; dstxofs, dstyofs: integer);
var
  x,y : integer;
  fp  : TFPColor;
  pl  : THMMPalColor;
begin
  for y:=0 to px.Height-1 do
    for x:=0 to px.Width-1 do begin
      case px.Lines[y][x] of
        ICN_PIXEL_EMPTY: fp := FPColor(0,0,0,0);
        ICN_PIXEL_SHADOW: fp := FPColor(0,0,0,$8000);
      else
        pl := pal[ byte(px.Lines[y][x]) ];
        fp := FPColor(pl.r * 255, pl.g * 255, pl.b * 255, $FFFF)
      end;
      dst.Colors[x+dstxofs, y+dstyofs]:=fp;
    end;
end;

procedure ICNtoBMP(const IcnFile: string);
var
  icn   : TICNSpriteFile;
  bmpfn : string;
  img   : TFPMemoryImage;
  w,h   : integer;
  i     : integer;
  x,y   : Integer;
  px    : TICNPixels;
  bmpw  : TFPWriterBMP;
  fs    : TFileStream;
begin
  try
    icn := TICNSpriteFile.Create;
    try
      ICNReadFile(IcnFile, icn);

      w:=icn.header[0].W;
      h:=icn.header[0].H;
      for i := 1 to icn.count-1 do begin
        inc(h, icn.header[i].h);
        w := Max(w, icn.header[i].w);
      end;
      bmpfn := ChangeFileExt(IcnFile,'.bmp');

      x:=0;
      y:=0;
      img := TFPMemoryImage.create(w, h);
      try
        img.UsePalette := false;
        for i := 0 to icn.count-1 do begin
          px := TICNPixels.Create(icn.header[i].W, icn.header[i].H, icn.header[i].tp = ICN_TYPE_MONO) ;
          try
            ICNDataToPixData(icn.data, icn.header[i].ofs - icn.count * sizeof(TICNSpriteHeader), px);
            ICNToFPImage(px, DefaultPal, img, x, y);
          finally
            px.Free;
          end;
          inc(y, icn.header[i].H);
        end;

        fs := TFileStream.Create(bmpfn, fmCreate);
        bmpw  := TFPWriterBMP.Create;
        try
          bmpw.BitsPerPixel:=32;
          bmpw.ImageWrite(fs, img);
        finally
          bmpw.Free;
          fs.Free;
        end;
      finally
        img.Free;
      end;

    finally
      icn.Free;
    end;
  except
    on e:exception do
      writeln('crash: ', e.message);
  end;
end;


var
  gCmd     : string = '';
  gCmdLow  : string;
  gIcnFile : string = '';
  gSubject : TStringList = nil;

procedure ParseParams;
var
  i : integer;
  s : string;
begin
  if not Assigned(gSubject)
    then gSubject:=TStringList.Create
    else gSubject.Clear;
  i:=1;
  while (i <= ParamCount) do begin
    s := ParamStr(i);
    if gCmd='' then gCmd := s
    else if gIcnFile = '' then gIcnFile := s
    else gSubject.Add(s);
    inc(i);
  end;
  if gCmd = '?' then gCmd := 'h';
  gCmdLow := AnsiLowerCase(gCmd);

  // only file name was specified, then let's show the list of files
  if (gCmdLow <> 'h') and (gCmd<>'') and FileExists(gCmd) and (gIcnFile='') then begin
    gIcnFile := gCmd;
    gCmdLow := '';
    gCmd := '';
  end;
end;

procedure ShowHelp;
begin
  writeln('icnreader %command% %source_file.icn%');
  writeln('  h - shows this help');
end;

begin
  ParseParams;
  if ((gIcnFile = '') and (gCmdLow = '')) or (gCmdLow = 'h') then begin
    ShowHelp;
    Exit;
  end;
  try
    if gCmdLow = 'bmp' then begin
      if (gIcnFile = '') then begin
        writeln('please specify ICN file');
        Exit;
      end;
      ICNtoBMP(gIcnFile);
    end else
      ReadICNFile(gIcnFile);
  finally
    gSubject.Free;
  end;
end.


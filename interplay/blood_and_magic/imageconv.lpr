program imageconv;
// the project is using "gamerescommon" package
// which is located at ../../common
// OR
// you should be using LazarusIDE

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Math,
  Classes, SysUtils, bmfile, fpimageutils, spriteutils, fpimage;

procedure ConvertImage(const picFn, extFn, palFn, dstFn: string);
var
  fs : TFileStream;
  cnt : integer;
  h  : array of TFrameInfo;
  i  : integer;
  sz : integer;
  sa : TSpriteAlign;
  ofs : integer;
  img : array of byte;
  pal : array of byte;
begin
  writeln('conerting');
  writeln('picture:  ', picFn);
  writeln('extra:    ', extFn);
  writeln('pallette: ', palFn);
  fs := TFileStream.Create(extFn, fmOpenRead or fmShareDenyNone);
  try
    cnt := fs.Size div SizeOf(TFrameInfo);
    writeln('frames count: ', cnt);
    if (fs.Size mod SizeOf(TFrameInfo)<>0) then
      writeln('WARNING: extra info file might be invalid');
    SetLength(h, cnt);
    i := 0;
    fs.Read(h[0], sizeof(TFrameInfo) * cnt);
  finally
    fs.Free;
  end;
  sz := 0;
  for i:=0 to cnt-1 do begin
    writeln('frame: ',i+1);
    writeln('  ofs:  ', h[i].ofsx,' ',h[i].ofsy,' flag: ', h[i].flags);
    writeln('  size: ', h[i].width,' ',h[i].height);
    writeln('  time: ', h[i].time);
    inc(sz, h[i].width * h[i].height);
  end;
  writeln('expected image size: ', sz);

  sa := TSpriteAlign.Create;
  try
    fs := TFileStream.Create(picFn, fmOpenRead or fmShareDenyNone);
    try
      writeln('actual size: ', fs.Size);
      if fs.Size < sz then
        writeln('WARNING: the picture size is smaller than expected')
      else if fs.Size > sz then
        writeln('NOTE: the picture size is greater than expected');

      SetLength(img, fs.Size);
      fs.Read(img[0], length(img));
    finally
      fs.Free;
    end;

    pal := nil;
    if (palFn<>'') and (FileExists(palFn)) then begin
      fs := TFileStream.Create(palFn, fmOpenRead or fmShareDenyNone);
      try
        SetLength(pal, Min(fs.Size,768));
        fs.Read(pal[0], length(pal));
      finally
        fs.Free;
      end;
    end;

    if (pal = nil) or (length(pal)<768) then begin
      SetLength(pal, length(DefaultPalette));
      Move(DefaultPalette[0], pal[0], length(DefaultPalette));
    end;

    ofs := 0;
    for i:= 0 to cnt-1 do begin
      sa.AddSpriteWithOfs( h[i].width, h[i].height, ofs, i, nil);
      inc(ofs, h[i].width * h[i].height);
    end;

    SpritesAlignToBmpFile(sa, img, pal, TRANSP_COLOR, dstFn);

  finally
    sa.Free;
  end;
end;

procedure PrintHelp;
begin
  writeln('please specify .pic, .ext file extracted earlier');
  writeln(' and optionally .pal file');
end;

var
  picFn : string;
  extFn : string;
  palFn : string;
  pi : integer;
begin
  if ParamCount=0 then begin
    PrintHelp;
    Exit;
  end;
  pi := 1;
  picFn := ParamStr(pi);
  if (uppercase(ExtractFileExt(picFn))='.HDR') then begin
    extFn := ChangeFileExt(picFn, '.ext');
    picFn := ChangeFileExt(picFn, '.pic');
  end else if (uppercase(ExtractFileExt(picFn))='.EXT') then begin
    extFn := picFn;
    picFn := ChangeFileExt(extFn, '.pic');
  end else
    extFn := ChangeFileExt(picFn, '.ext');
  inc(pi);

  writelN('p=',ParamCount,' ',pi);
  if (ParamCount>=pi) then
    palFn := ParamStr(pi)
  else
    palFn := '';

  if not FileExists(picFn) then begin
    writeln('picture file: ',picFn,' not found or not especified');
    exit;
  end;

  if not FileExists(extFn) then begin
    writeln('extended header file: ',extFn,' not found or not especified');
    exit;
  end;

  ConvertImage(picFn, extFn, palFn, ChangeFileExt(picFn,'.bmp'));

end.


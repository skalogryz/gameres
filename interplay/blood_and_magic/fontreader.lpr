program fontreader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, bmfile, spriteutils
  { you can add units after this };

procedure ConvertFont(const fontfn, extfn : string);
var
  fs  : TfileStream;
  f   : TFontExtraInfo;
  buf : array of byte;
  i   : integer;
  sp  : TSpriteAlign;
  palBuf : array of byte;
  c      : byte;
  mx     : integer;
begin
  fs := TfileStream.Create(extfn, fmOpenRead or fmShareDenyNone);
  try
    ReadFontExtaInfo(fs, f);
    writeln('count:  ', f.info.maxChar -f.info.minChar+1);
    writeln('flag1:  ', f.info.flag1);
    writeln('flag2:  ', f.info.flag2);
    writeln('flag3:  ', f.info.flag3);
    writeln('flag4:  ', f.info.flag4);
    writeln('flag5:  ', f.info.flag5);
    mx := 0;
    for i := 0 to f.charCount-1 do
      if f.chars[i].Width > mx then
        mx := f.chars[i].Width;
    writeln('size:   ', mx,' x ',f.info.Height);
  finally
    fs.Free;
  end;

  SetLength(palBuf, 768);
  for i:=1 to 12 do begin
    c := (i-1)*12;
    palBuf[i*3+0]:=c;
    palBuf[i*3+1]:=c;
    palBuf[i*3+2]:=c;
  end;
  fs := TfileStream.Create(fontfn, fmOpenRead or fmShareDenyNone);
  try
    SetLength(buf, fs.Size);
    fs.Read(buf[0], length(buf));
    sp := TSpriteAlign.Create;
    try
      for i := 0 to f.charCount-1 do
        sp.AddSpriteWithOfs(f.chars[i].Width, f.info.Height, f.chars[i].offset);

      sp.Sort;
      AlignInEqColumns(sp, 16);
      SpritesAlignToBmpFile(sp, buf, palBuf, 0, ChangeFileExt('font_'+ExtractFileName(fontfn),'.bmp'));
    finally
      sp.Free;
    end;
  finally
    fs.Free;
  end;
end;

var
  fs : TFileStream;
  font: string;
  ext : string;
begin
  if ParamCount=0 then begin
    writeln('please sepcify extracted .font file ');
    exit;
  end;
  font := ParamStr(1);
  if (UpperCase(ExtractFileExt(font))='.EXT') then
    font := ChangeFileExt(font, '.font');
  ext := ChangeFileExt(font, '.ext');

  ConvertFont(font, ext);
end.


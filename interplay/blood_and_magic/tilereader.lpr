program tilereader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, bmfile, spriteutils, fpimage, fpimageutils
  { you can add units after this };

procedure ParseTiles(const fn: string; separateTiles: Boolean = false);
var
  fs : TFileStream;
  h  : TTileHeader;
  i  : integer;
  t  : TTileInfo;
  buf : array of byte;
  sa  : TSpriteAlign;
  pt  : PTile;
  cnt : integer;
  bits : array [0..7] of integer;
  j    : integer;
  tfn  : string;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  for j:=0 to length(bits)-1 do bits[j]:=0;
  try
    cnt := 0;
    fs.Read(h, sizeof(h));
    writeln('img tiles: ', h.count1);
    writelN('log parts: ', h.count2);
    for i:=0 to h.count2-1 do begin
      fs.Read(t, sizeof(t));
      write(t.name:20);
      write(',',t.moveCost:6);
      write(',',t.flag2:6);
      write(',',t.flag3:6);
      write(',',t.defBonus:6);
      write(',',t.healthCost:6);
      write(',',BinStr(t.flag6,8):10);
      for j := 0 to 7 do begin
        if ((1 shl j) and t.flag6 >0) then inc(bits[j]);
      end;
      write(',',t.flag7:6);
      write(',',t.iconId,' ',IntToHex(t.iconId, 8));
      if (t.flag6=255) then inc(cnt);

      writeln;
    end;
    writeln(fs.Size-fs.Position);
    // file 6 2404 - doesn't have 500 bytes, instead it was 384
    writeln('  remain: ', fs.size - sizeof(TTileInfo) * h.count2 - 773*h.count1);
    writeln('   count: ', h.count1);
    writeln('     ofs: ', IntToHex(sizeof(TTileHeader)+2468+sizeof(TTileInfo) * h.count2,8));
    writeln('real ofs: ', IntToHex(fs.size - sizeof(TTile)*h.count1,8));
    writeln('real  sz: ', fs.Size-fs.Position-sizeof(TTile)*h.count1);
    writeln('tile  zz: ', sizeof(TTile));
    writeln('info  sz:  ', sizeof(TTileInfo));

    for j:=length(bits)-1 downto 0 do
      writeln(j,' ',bits[j]);

    SetLength(buf, sizeof(TTile)*h.count1);
    fs.Position:=fs.size {%H-}- sizeof(TTile)*h.count1;
    fs.Read(buf[0], length(buf));
    sa := TSpriteAlign.Create;
    for i:=0 to h.count1-1 do begin
      pt := PTile(@buf[i*sizeof(TTile)]);
      writeln(i,': ',pt^.id,' ',IntToHex(pt^.id, 8));
      sa.AddSpriteWithOfs(TileWidth, TileHeight, i*sizeof(TTile)+13);
    end;
    AlignInSquareByMaxSpr(sa, TileWidth, TileHeight);
    SpritesAlignToBmpFile(sa, buf, DefaultPalette, TRANSP_COLOR, ChangeFileExt(fn,'.bmp'));

    if (separateTiles) then begin
      writelN('extracing files: ',h.count1);
      for i := 0 to h.count1-1 do begin
        pt := PTile(@buf[i*sizeof(TTile)]);
        try
          tfn := Format('tile_%d_%s_%s_%s_%s.bmp', [i,IntToHex(i,4),IntToHex(pt^.id,8),IntToHex(pt^.flag1,8),IntToHex(pt^.flag2,8),IntToHex(pt^.flag4,8)]);
          writeln(tfn);
          if not PalBytesToBmpFile( PByteArray(@buf[i*sizeof(TTile)+13])^, TileWidth, TileHeight, DefaultPalette, tfn) then
            writeln('failed!');
        except
          writeln('failure on extraction of ',i);
        end;
      end;
    end;

    //fs.Position:=fs.size - sizeof(TTileHeader) * h.count2;

  finally
    fs.Free;
  end;
end;

begin
  if ParamCount=0 then begin
    writeln('please specify extract tile file (18)');
    exit;
  end;
  ParseTiles(ParamStr(1));
end.


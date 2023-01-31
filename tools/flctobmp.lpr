program flctobmp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, flic, fpimageutils, fpimage
  ,spriteutils
  ,FPCanvas;
  { you can add units after this }

var
  fs : TFileStream;
  fd : TDecoder;
  h  : THeader;
  res : Boolean;
  f   : TFrame;
  i   : integer;
  fp  : TFPCustomImage;
  fin : TFPCustomImage;

  nm  : string;
  dst : string;


  al : TSpriteAlign;
begin
  if ParamCount=0 then begin
    writeln('please specify input file name');
    exit;
  end;
  nm := ChangeFileExt(ParamStr(1),'');
  fs := TFileStream.Create(PAramStr(1), fmOpenRead or fmShareDenyNone);
  fd := TDecoder.Create(fs, false);
  al := TSpriteAlign.Create;
  try
    res := fd.readHeader(h);
    if not res then begin
      writeln('error');
      exit;
    end;


    writeln('width:  ',h.width);
    writeln('height: ',h.height);
    writeln('frames: ',h.frames);
    writeln('speed:  ',h.speed);
    writeln(fd.m_offsetFrame1);
    writeln(fd.m_offsetFrame2);

    SetLength(f.pixels, h.width*h.height);
    f.rowstride := h.width;

    for i := 0 to h.frames - 1 do begin
      if not fd.readFrame(f) then begin
        writeln('failed to read frame');
        exit;
      end else
        writeln('success!');
      fp := PalBytesToFpImage(f.pixels, h.width, h.height, PByteArray(@f.colormap[0])^);
      al.AddSprite(fp.Width, fp.Height, i, fp);
      //dst :=nm+'_'+IntTostr(i)+'.bmp';
      //writeln('saving: ', dst);
      //FPImageToBmp(fp, dst);
      //fp.Free;
    end;
    AlignInSquareByMaxSpr(al, h.width, h.width);
    fin := SpritesAsFPImages(al);
    FPImageToBmp(fin,nm+'.bmp');
    fin.Free;
  finally
    fs.Free;
    al.Free;
  end;
end.


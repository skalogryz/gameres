program tpwmtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, tpwmdecompress
  { you can add units after this };

var
  fs : TFileStream;
  inp : array of byte;
  ofs : integer;
  sz  : integer;
  otp : array of byte;
  succ : boolean;
begin
  if ParamCount=0 then begin
    writeln('please provide the file name');
    exit;
  end;
  fs := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone);
  try
    SetLength(inp, fs.Size);
    fs.Read(inp[0], length(inp));
  finally
    fs.Free;
  end;
  succ := ParseHeader(inp, ofs, sz);
  if not succ then begin
    writeln('this is not TPWM file');
    exit;
  end;
  writeln('unpack size: ',sz);
  writeln('pack size:   ',length(inp));

  SetLength(otp, sz);
  Decompress(inp, ofs, otp, sz);

  fs := TFileStream.Create(ParamStr(1)+'.result', fmCreate);
  try
    fs.Write(otp[0], sz);
  finally
    fs.Free;
  end;
end.


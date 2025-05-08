program fbxout;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fbxtypes, fbxread
  { you can add units after this };

procedure ReadFile(st: TStream);
var
  rdr : TFbxParser;
begin
  rdr := TFbxParser.Create();
  try
    rdr.StartRead(st);
    while rdr.Next do begin
      case rdr.state of
        fpsHeader: writeln(rdr.hdr.Id,' ',rdr.hdr.Version);
        fpsNodeStart: begin
          writeln(rdr.nodeName);
          writeln('  props:  ', rdr.node.NumProperties);
          //writeln('end at: ', rdr.node.EndOffset);
        end;
      end;
    end;
  finally
    rdr.Free;
  end;
end;

var
  fs : TFileStream;
  fn : string;
begin
  if ParamCount=0 then begin
    writeln('please provide the input file name');
    Exit;
  end;
  fn := ParamStr(1);
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    ReadFile(fs);
  finally
    fs.Free;
  end;

end.


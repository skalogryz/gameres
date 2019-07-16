program cslist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, csfiles
  { you can add units after this };

begin
  if ParamCount=0 then begin
    writeln('please provide xun file');
    exit;
  end;
  XunDumpFile(ParamStr(1));
end.


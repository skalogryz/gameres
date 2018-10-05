program icnreader;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, hmm2agg;

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
    ReadICNFile(gIcnFile);
  finally
    gSubject.Free;
  end;
end.


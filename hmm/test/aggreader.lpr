program aggreader;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, hmm2agg;

procedure ListFiles(const src: string);
var
  agg : THHM2AggFile;
begin
  agg := THHM2AggFile.Create;
  try
    if not AGG2ReadFile(src, agg) then
      writeln('reading file failed');
    Dump(agg);
  finally
    agg.Free;
  end;
end;

function IndexOf(agg: THHM2AggFile; const fn: string): Integer;
var
  i : integer;
  n : string;
  lfn : string;
begin
  if (fn='') then begin
    Result := -1;
    Exit;
  end;
  lfn := AnsiLowerCase(fn);
  for i := 0 to agg.count-1 do
  begin
    n := AnsiLowerCase(GetName(agg.name[i]));
    if lfn = n then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure ExtractFiles(const src: string; extr: TStrings);
var
  agg : THHM2AggFile;
  i   : integer;
  sr  : TFileStream;
  ds  : TFileStream;
  j   : integer;
begin
  if not Assigned(extr) then Exit;
  agg := THHM2AggFile.Create;
  try
    if not AGG2ReadFile(src, agg) then begin
      writeln('reading file failed');
      Exit;
    end;
    if extr.Count=0 then
      for i:=0 to agg.count-1 do
        extr.Add(agg.name[i].name);

    if extr.Count=0 then Exit;

    sr := TFileStream.Create(src, fmOpenRead or fmShareDenyNone);
    try
      for i:=0 to extr.Count-1 do begin
        j := IndexOf(agg, extr[i]);
        if j<0 then Continue;

        try
          ds := TFileStream.Create(agg.name[j].name, fmCreate);
          try
            sr.Position := agg.info[j].offset;
            ds.CopyFrom(sr, agg.info[j].size);
          finally
            ds.Free;
          end;
        except
          writeln('failed to write: ', agg.name[j].name);
        end;

      end;
    finally
      sr.Free;
    end;
  finally
    agg.Free;
  end;
end;


var
  gCmd     : string = '';
  gCmdLow  : string;
  gAggFile : string = '';
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
    else if gAggFile = '' then gAggFile := s
    else gSubject.Add(s);
    inc(i);
  end;
  if gCmd = '?' then gCmd := 'h';
  gCmdLow := AnsiLowerCase(gCmd);

  // only file name was specified, then let's show the list of files
  if (gCmdLow <> 'h') and (gCmd<>'') and FileExists(gCmd) and (gAggFile='') then begin
    gAggFile := gCmd;
    gCmdLow := 'l';
    gCmd := 'l';
  end;
end;

procedure ShowHelp;
begin
  writeln('aggreader %command% %source_file.agg% [%file_to_extract%]');
  writeln('  l - list files');
  writeln('  x - extract file by name. If files_to_extract specified - ALL files are extracted');
  writeln('  h - shows this help');
end;

begin
  ParseParams;
  if (gCmdLow = '') or (gCmdLow = 'h') then begin
    ShowHelp;
    Exit;
  end;
  try
    if gCmdLow = 'l' then
      ListFiles(gAggFile)
    else if gCmdLow = 'x' then
      ExtractFiles(gAggFile, gSubject)
    else
      writeln('unknown command: ', gCmd);
  finally
    gSubject.Free;
  end;
end.


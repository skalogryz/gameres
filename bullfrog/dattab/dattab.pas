program dattab;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, bf_dattab, bf_dattabutils;


var
  ta  : TTabArray;
  tab : string;
  dat : string;
  ds  : TFileStream;
begin
  try
    if ParamCount=0 then
      writeln('please specify .tab file')
    else begin
      tab:=ParamStr(1);
      if AnsiUpperCase( ExtractFileExt(tab))<>'.TAB' then
        tab:=ChangeFileExt(tab,'.TAB');
      dat:=ChangeFileExt(tab,'.DAT');

      if TabsReadFromFile(tab, ta) then begin
        TabsDump(ta);
        if FileExists(dat) then begin
          try
            ds := TFileStream.Create(dat, fmOpenRead or fmShareDenyNone);
            try
              TabsDumpImages(ta, ds);
            finally
              ds.Free;
            end;
          except
          end;
        end;
      end else
        Writeln('failed to read the file ', ParamStr(1));
    end;
  except
    on e:exception do
      writeln(e.message);
  end;
end.


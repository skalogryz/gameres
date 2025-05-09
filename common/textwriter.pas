unit textwriter;
{$mode delphi}

interface

uses Classes, SysUtils;

type

  { TTextWriter }

  TTextWriter = class(TObject)
  public
    dst : TStream;
    LineBreak : string;
    constructor Create;

    procedure Wr(const s: string); overload;
    procedure Wr(const s: string; const params : array of const); overload;
    procedure WrLn(const s: string); overload;
    procedure WrLn(); overload;
    procedure WrLn(const s: string; const params : array of const); overload;
  end;


implementation

{ TTextWriter }

constructor TTextWriter.Create;
begin
  LineBreak := #13#10;
end;

procedure TTextWriter.Wr(const s: string);
begin
  if (s = '') then Exit;
  dst.Write(s[1], length(s));
end;

procedure TTextWriter.WrLn(const s: string); overload;
begin
  Wr(s);
  WrLn();
end;

procedure TTextWriter.WrLn(); overload;
begin
  Wr(LineBreak);
end;

procedure TTextWriter.Wr(const s: string; const params: array of const); overload;
begin
  Wr(Format(s, params));
end;

procedure TTextWriter.WrLn(const s: string; const params: array of const); overload;
begin
  WrLn(Format(s, params));
end;

end.

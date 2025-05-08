unit fbxread;

interface

uses
  Classes, SysUtils, fbxtypes;


type
  TFbxParseState =
  (
     fpsNone,
     fpsHeader,
     fpsNodeStart,
     fpsEndOfStream
  );

  { TFbxParser }

  TFbxParser = class(TObject)
  public
    src   : TStream;
    state : TFbxParseState;
    hdr   : TFbxFileHeader;

    node  : TFbxNode;
    nodeName : string;

    procedure StartRead(Source: TStream);

    function Next(skipContent: Boolean = true): Boolean;
  end;

implementation

{ TFbxParser }

procedure TFbxParser.StartRead(Source: TStream);
begin
  src := Source;
  state := fpsNone;
end;

function TFbxParser.Next(skipContent: Boolean): Boolean;
begin
  if ((state = fpsEndOfStream) or (src = nil) or (src.Position >= src.Size)) then
  begin
    state := fpsEndOfStream;
    Result := false;
    Exit;
  end;

  if (state = fpsNodeStart) and (skipContent) then begin
    state := fpsHeader;
    src.Position:=node.EndOffset;
    if (src.Position >= src.Size) then begin
      state := fpsEndOfStream;
      Result := false;
      Exit;
    end;
  end;

  Result := true;

  case state of
    fpsNone: begin
      src.Read(hdr, sizeof(hdr));
      state := fpsHeader;
    end;

    fpsHeader: begin
      src.Read(node, sizeof(node));
      if (IsNullNode(node)) then begin
        state := fpsEndOfStream;
        Result := false;
      end;

      //todo: mem check
      // name can be empty
      SetLength(nodeName, node.NameLen);
      if (node.NameLen > 0) then
        src.read(nodeName[1], node.NameLen);
      state := fpsNodeStart;
    end;
  else
      Result := false;
  end;

end;

end.


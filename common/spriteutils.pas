unit spriteutils;

interface

{$mode objfpc}{$H+}

uses
  Types, Math, Classes, SysUtils, FPimage, fpimageutils;

type

  { TSpriteAlignPos }

  TSpriteAlignPos = class(TObject)
  private
    fSeq: Integer;
  public
    width     : integer;
    height    : Integer;
    targetX   : Integer;
    targetY   : Integer;

    tag       : TObject;
    tagIndex  : Integer;
    tagOffset : Integer;
    ownTag    : Boolean;
    property Seq: Integer read fSeq;
    constructor Create(ASeq: Integer);
    destructor Destroy; override;
  end;

  { TSpriteAlign }

  TSpriteAlign = class(TObject)
  private
    fsprites: TList;
    function GetCount: integer;
    function GetSprite(i: integer): TSpriteAlignPos;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSprite( width, height: integer;
      tagIndex : integer = 0;
      tag: TObject = nil;
      tagOffset: Integer = -1
      ): TSpriteAlignPos;
    function AddSpriteWithOfs( width, height: integer;
      ofs: Integer;
      tagIndex : integer = 0;
      tag: TObject = nil
      ): TSpriteAlignPos;
    function AlignSprites(out sz: TSize): Boolean;
    property Count: integer read GetCount;
    property Sprite[i: integer]: TSpriteAlignPos read GetSprite; default;
    procedure Clear;
  end;

function SpritesAlignToFPImage(sa: TSpriteAlign; const imgBuf, palBuf: array of byte): TFPCustomImage;
function SpritesAlignToBmpFile(sa: TSpriteAlign; const imgBuf, palBuf: array of byte; const dstFn: string): Boolean;

implementation

function CompareInt(a,b: integer): integer;
begin
  if a=b then Result:=0
  else if a<b then Result:=-1
  else Result:=1;
end;

function SortByIndexSeq(p1,p2: Pointer): integer;
var
  s1 : TSpriteAlignPos;
  s2 : TSpriteAlignPos;
begin
  s1 := TSpriteAlignPos(p1);
  s2 := TSpriteAlignPos(p2);
  Result := CompareInt(s1.tagIndex, s2.tagIndex);
  if Result=0 then
    Result:=CompareInt(s1.Seq, s2.Seq);
end;

{ TSpriteAlignPos }

constructor TSpriteAlignPos.Create(ASeq: Integer);
begin
  inherited Create;
  fSeq:=ASeq;
  tagOffset := -1;
end;

destructor TSpriteAlignPos.Destroy;
begin
  if ownTag then tag.Free;
  inherited Destroy;
end;

{ TSpriteAlign }

function TSpriteAlign.GetSprite(i: integer): TSpriteAlignPos;
begin
  Result := TSpriteAlignPos(fsprites[i]);
end;

function TSpriteAlign.GetCount: integer;
begin
  Result := fsprites.Count;
end;

constructor TSpriteAlign.Create;
begin
  fsprites:=TList.Create;
end;

destructor TSpriteAlign.Destroy;
begin
  Clear;
  fsprites.Free;
  inherited Destroy;
end;

function TSpriteAlign.AddSprite(width, height: integer; tagIndex: integer;
  tag: TObject; tagOffset: Integer = -1): TSpriteAlignPos;
begin
  if (width<0) or (height<0) then begin
    Result:=nil;
    Exit;
  end;
  Result := TSpriteAlignPos.Create(fsprites.Count);
  Result.width := width;
  Result.height := height;
  Result.tag := tag;
  Result.tagIndex := tagIndex;
  Result.tagOffset := tagOffset;
  fsprites.Add(Result);
end;

function TSpriteAlign.AddSpriteWithOfs(width, height: integer; ofs: Integer;
  tagIndex: integer; tag: TObject): TSpriteAlignPos;
begin
  Result := AddSprite(width, height, tagIndex, tag, ofs);
end;

function TSpriteAlign.AlignSprites(out sz: TSize): Boolean;
var
  i  : integer;
  sp : TSpriteAlignPos;
  x,y: integer;
begin
  sz.cx:=0;
  sz.cy:=0;
  fsprites.Sort(@SortByIndexSeq);
  x:=0;
  y:=0;
  for i := 0 to fsprites.Count-1 do begin
    sp := TSpriteAlignPos(fsprites[i]);
    sp.targetX := x;
    sp.targetY := y;
    sz.cx := Max(sz.cx, sp.Width);
    inc(y, sp.Height);
  end;
  sz.cy := y;
  Result := true;
end;

procedure TSpriteAlign.Clear;
var
  i : integer;
begin
  for i:=0 to fsprites.Count-1 do
    TObject(fsprites[i]).Free;
  fsprites.Clear;
end;

function SpritesAlignToFPImage(sa: TSpriteAlign; const imgBuf, palBuf: array of byte): TFPCustomImage;
var
  sz : TSize;
  sp : TSpriteAlignPos;
  i  : integer;
begin
  sa.AlignSprites(sz);
  Result := TFPMemoryImage.Create(sz.Width, sz.Height);
  for i:=0 to sa.Count-1 do begin
    sp := sa.Sprite[i];
    if sp.tagOffset<0 then continue;
    PalBytesToFpImage(imgBuf, sp.tagOffset,
      sp.width, sp.height,
      palBuf, Result, sp.targetX, sp.targetY);
  end;
end;

function SpritesAlignToBmpFile(sa: TSpriteAlign; const imgBuf, palBuf: array of byte; const dstFn: string): Boolean;
var
  fp : TFPCustomImage;
begin
  fp := SpritesAlignToFPImage(sa, imgBuf, palBuf);
  Result := Assigned(fp);
  if not Result then Exit;
  try
    FPImageToBmp(fp, dstFn);
  finally
    fp.Free;
  end;
end;

end.

unit spriteutils;

interface

{$mode objfpc}{$H+}

uses
  Types, Math, Classes, SysUtils, FPimage, fpimageutils
  ,FPCanvas, FPImgCanv;

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
    property Count: integer read GetCount;
    property Sprite[i: integer]: TSpriteAlignPos read GetSprite; default;
    procedure Clear;
    function GetMaxSize: TSize;
    function GetMaxSpriteSize: TSize;
    function AllPlaced: Boolean;
    function AllOffseted: Boolean;
    procedure Sort;
  end;

function SpritesAlignToFPImage(sa: TSpriteAlign; const imgBuf, palBuf: array of byte; palTransIdx: Integer): TFPCustomImage;
function SpritesAlignToBmpFile(sa: TSpriteAlign; const imgBuf, palBuf: array of byte;
  palTransIdx: Integer; const dstFn: string): Boolean;

procedure AlignVertically(sa: TSpriteAlign);
procedure AlignInSquareByMaxSpr(sa: TSpriteAlign; maxSprWidth, maxSprHeight: Integer);
procedure AlignInEqColumns(sa: TSpriteAlign; colNum: integer);

function SpritesAsFPImages(sa: TSpriteAlign): TFPCustomImage;

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
  targetX := -1;
  targetY := -1;
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

procedure AlignVertically(sa: TSpriteAlign);
var
  i  : integer;
  sp : TSpriteAlignPos;
  x,y: integer;
begin
  x:=0;
  y:=0;
  for i := 0 to sa.Count-1 do begin
    sp := sa[i];
    sp.targetX := x;
    sp.targetY := y;
    inc(y, sp.Height);
  end;
end;

procedure TSpriteAlign.Clear;
var
  i : integer;
begin
  for i:=0 to fsprites.Count-1 do
    TObject(fsprites[i]).Free;
  fsprites.Clear;
end;

function TSpriteAlign.GetMaxSize: TSize;
var
  i   : integer;
  x,y : integer;
  s   : TSpriteAlignPos;
begin
  Result.cx:=0;
  Result.cy:=0;
  for i := 0 to Count-1 do begin
    s := sprite[i];
    x := s.targetX + s.width;
    y := s.targetY + s.height;
    if x > Result.cx then Result.cx := x;
    if y > Result.cy then Result.cy := y;
  end;
end;

function TSpriteAlign.GetMaxSpriteSize: TSize;
var
  i  : integer;
  sp : TSpriteAlignPos;
begin
  Result := size(0,0);
  for i:=0 to count-1 do begin
    sp := sprite[i];
    Result.cx := Max(Result.cx, sp.width);
    Result.cy := Max(Result.cy, sp.height);
  end;
end;

function TSpriteAlign.AllPlaced: Boolean;
var
  i  : integer;
  sp : TSpriteAlignPos;
begin
  for i:=0 to Count-1 do begin
    sp := sprite[i];
    if (sp.targetX < 0) or (sp.targetY < 0) then begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

function TSpriteAlign.AllOffseted: Boolean;
var
  i  : integer;
  sp : TSpriteAlignPos;
begin
  for i:=0 to Count-1 do begin
    sp := sprite[i];
    if (sp.tagOffset < 0) then begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

procedure TSpriteAlign.Sort;
begin
  fsprites.Sort(@SortByIndexSeq);
end;

function SpritesAlignToFPImage(sa: TSpriteAlign; const imgBuf, palBuf: array of byte; palTransIdx: Integer): TFPCustomImage;
var
  sz : TSize;
  sp : TSpriteAlignPos;
  i  : integer;
begin
  if not sa.AllPlaced then begin
    sa.Sort;
    AlignVertically(sa);
  end;

  sz := sa.GetMaxSize;
  Result := TFPMemoryImage.Create(sz.Width, sz.Height);
  for i:=0 to sa.Count-1 do begin
    sp := sa.Sprite[i];
    if sp.tagOffset<0 then continue;
    PalBytesToFpImage(imgBuf, sp.tagOffset,
      sp.width, sp.height,
      palBuf, palTransIdx,
      Result, sp.targetX, sp.targetY);
  end;
end;

function SpritesAlignToBmpFile(sa: TSpriteAlign; const imgBuf, palBuf: array of byte;
  palTransIdx: Integer; const dstFn: string): Boolean;
var
  fp : TFPCustomImage;
begin
  fp := SpritesAlignToFPImage(sa, imgBuf, palBuf, palTransIdx);
  Result := Assigned(fp);
  if not Result then Exit;
  try
    FPImageToBmp(fp, dstFn);
  finally
    fp.Free;
  end;
end;

procedure AlignInSquareByMaxSpr(sa: TSpriteAlign; maxSprWidth, maxSprHeight: Integer);
var
  i    : integer;
  maxw : integer;
  col  : integer;
  y    : integer;
  x    : integer;
  sp   : TSpriteAlignPos;
begin
  maxw := Round(sqrt(sa.Count));
  y := 0;
  x := 0;
  col := 0;
  for i := 0 to sa.count-1 do begin
    sp := sa.Sprite[i];
    sp.targetX := x;
    sp.targetY := y;
    inc(x, maxSprWidth);
    inc(col);
    if col > maxw then begin
      inc(y, maxSprHeight);
      x := 0;
      col := 0;
    end;
  end;
end;

procedure AlignInEqColumns(sa: TSpriteAlign; colNum: integer);
var
  sz : TSize;
  i  : integer;
  sp : TSpriteAlignPos;
begin
  if not Assigned(sa) then Exit;
  if colNum<=0 then colNum :=1;
  sz := sa.GetMaxSpriteSize;
  for i:=0 to sa.Count-1 do begin
    sp := sa[i];
    sp.targetX := i mod colNum * sz.cx;
    sp.targetY := i div colNum * sz.cy;
  end;
end;


function SpritesAsFPImages(sa: TSpriteAlign): TFPCustomImage;
var
  sz : TSize;
  sp : TSpriteAlignPos;
  i  : integer;
  c  : TFPImageCanvas;
begin
  sz := sa.GetMaxSize;
  Result := TFPMemoryImage.Create(sz.Width, sz.Height);
  c := TFPImageCanvas.create(Result);
  for i:=0 to sa.Count-1 do begin
    sp := sa.Sprite[i];
    c.Draw(sp.targetX,sp.targetY, TFPCustomImage(sp.Tag));
  end;

end;

end.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Shape1: TShape;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;


function GetC(const a:TRGBTriple):TColor;
begin
  Result:=(a.rgbtRed)or(a.rgbtGreen shl 8)or (a.rgbtBlue shl 16);
end;
procedure FloodBmp(ABmp:TBitmap;ax, ay: integer; AColor: TColor;var Bds:TRect);
var
  S: TColor;
  Dst:TRGBTriple;
  H,W,Cap,Len : integer;
  Px : TPoint;
  L,R : integer;
  Arr:array of TPoint;
  ScanLine:PRGBArray;
  function UpdatePixel(ax: integer):boolean;
  begin
    Result:=GetC(ScanLine[ax])=S;
    if Result then
       ScanLine[ax]:=Dst;
  end;

  procedure Add(ax,ay : integer);
  begin
    if Cap=Len then
    begin
      Cap:=Cap+64;
      Setlength(Arr,Cap);
    end;
    Arr[Len]:=Point(ax,ay);
    Inc(Len);
  end;
  procedure ProcessLine(ay,l,r:integer);
  var
   I:integer;
   b,v:boolean;
   Line:PRGBArray;
  begin
    if (ay < 0) or (ay > H) then
       Exit;
    b:=false;
    Line:=ABmp.ScanLine[ay];
    for I := l to r do
    begin
      V:= GetC(Line[I])=S;
      if v and not b then
         Add(I,ay);
      b:=v;
    end;
  end;
begin
  W := ABmp.width-1;
  H := ABmp.height-1;
  if (ax < 0) or (ay < 0) or(ax > W) or (ay >H) then
     Exit;
  ABmp.PixelFormat:=pf24Bit;
  S :=ABmp.Canvas.Pixels[ax,ay];
  if S=AColor then
     Exit;
  Dst.rgbtRed  :=  AColor and $FF;
  Dst.rgbtGreen:= (AColor shr 8)and $FF;
  Dst.rgbtBlue := (AColor shr 16)and $FF;
  Bds:=Rect(ax,ay,ax,ay);
  Cap:=0;
  Len:=0;
  Add(ax,ay);
  while Len <> 0 do
  begin
    Dec(Len);
    Px:=Arr[Len];
    ScanLine:=ABmp.ScanLine[Px.Y];
    L:=Px.X;
    while (L > 0)and UpdatePixel(L-1) do
      Dec(L);
    R:=Px.X-1;
    while (R < W)and UpdatePixel(R+1) do
      Inc(R);
    ProcessLine(Px.Y-1,L,R);
    ProcessLine(Px.Y+1,L,R);
    if Bds.Left > L then
       Bds.Left:= L;
    if Bds.Right < R then
       Bds.Right:= R;
    if(Bds.Top >Px.Y-1) then
       Bds.Top:= Px.Y-1;
    if(Bds.Bottom< Px.Y+1) then
       Bds.bottom:= Px.Y+1;
  end;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 R:Trect;
 FillColor:TColor;
begin
  //random color;
  FillColor:=rgb(random(128)+120,
                 random(128)+120,
                 random(128)+120); 
  Floodbmp(Image1.Picture.Bitmap,x,y,FillColor,R);
  with R do
   Shape1.SetBounds(Image1.Left+Left,
                    Image1.Top+Top,
                    Right-Left+1,
                    Bottom-Top+1);
  image1.Invalidate;
end;


end.

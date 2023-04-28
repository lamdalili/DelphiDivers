//  Unit Reflets (like Lake applet) - Don't forget Jpeg unit
//  Jean Yves Quéinec 28/10/2000 - j.y.q@wanadoo.fr
unit Ureflets;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, jpeg, ExtDlgs;
type
  TAniSplash = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    PaintBox1: TPaintBox;
    MLicence: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtQuitterClick(Sender: TObject);
    procedure BtopenClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure FormResize(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    procedure Initmirror;
    procedure mirror(ph : integer);
  end;

var
  AniSplash: TAniSplash;

implementation
uses Anim;
{$R *.DFM}

type
  TRGBArray = ARRAY[0..0] OF TRGBTriple;   // bitmap pixel (API windows)
  pRGBArray = ^TRGBArray;     // pointer to 3 bytes pixel(24 bits)

Var
  limitebmp2 : integer;   // adjustable mirror position
  savebmp2H  : integer;   // Bmp2 height for clic

  //   bitmap loaded (bmp1)
  //   +---------------+        bmp2             lake bmp3
  //   |               |    +-----------+       +---------+
  //   |               |    |   top     |       |  bottom |
  //   |               |===>|           | ===>  |         |
  //   |               |    |           |       +---------+
  //   |               |    |           |           ||
  //   |               |    |   bottom  |           ||
  //   |               |    +-----------+           ||
  //   |               |    \  mirror   \  Bmp4 <===//
  //   +---------------+    /           /
  //                        +-----------+
  bmp1 : Tbitmap;   // bitmap read
  bmp2 : Tbitmap;   // Bitmap loaded (on top of  paintbox1)
  bmp3 : Tbitmap;   // Bitmap (vertical mirror)
  bmp4 : Tbitmap;   // bitmap lake (paintbox bottom)
  // scanlines arrays for optimisation
  Tscan3 : array[0..1024] of pRGBArray;
  Tscan4 : array[0..1024] of PRGBArray;
  // Frames : numer of lake sector   Phase = current frame
  Frames : integer;
  Phase  : integer;
  stop : boolean;
  // calculated sinus (degrees)
  zsin : array[0..360] of single;

procedure TAniSplash.FormCreate(Sender: TObject);
var
  i : integer;
  a : single;
begin
  timer1.enabled := false;
  phase  := 0;
  for i := 0 to 360 do
  begin
    a := (i * 180) / pi;
    zsin[i] := sin(i*180);
  end;
  bmp1 := tbitmap.create; bmp1.width  := 8;  Bmp1.height := 8;
  bmp2 := Tbitmap.create; bmp2.width  := 8;  Bmp2.height := 8;
  bmp3 := tbitmap.create; bmp3.width  := 8;  Bmp3.height := 8;
  bmp4 := Tbitmap.create; bmp4.width  := 8;  Bmp4.height := 8;
end;

procedure TAniSplash.FormDestroy(Sender: TObject);
begin
  bmp1.free;
  bmp2.free;
  bmp3.free;
  bmp4.free;
end;

procedure TAniSplash.BtQuitterClick(Sender: TObject);
begin
  close;
end;

//---------  Buttons
procedure TAniSplash.BtopenClick(Sender: TObject);
var
 s :string;
begin
  stop := true;
  timer1.enabled := false;
 // If Promptforfilename(s) then
  begin
   // image1.Picture.LoadFromFile(s);
    bmp1.width := image1.picture.graphic.width;
    bmp1.height := image1.picture.graphic.height;
    bmp1.pixelformat := pf24bit;
    bmp1.canvas.draw(0,0,image1.picture.graphic);
    limitebmp2 := 0;
    initmirror;
    timer1.enabled := true;
  end;
end;

procedure TAniSplash.Initmirror;
var
  i : integer;
  h : integer;            // Max Form height
  k1, k2 : integer;       // lake parameters
begin
  frames := 16;
 // case radiogroup1.itemindex of
 // 0 : begin k1 := 1; k2 := 3; end;
 // 1 : begin k1 := 1; k2 := 2; end;
   k1 := 2; k2 := 3;
 // 3 : begin k1 := 4; k2 := 5; end;
 // end;
  //  Bmp2 must fit in client form area
  bmp2.free;
  bmp2 := tbitmap.create;

  begin
    bmp2.width := bmp1.width;
    bmp2.height := bmp1.height;
  end;
  If limitebmp2 = 0 then limitebmp2 := bmp2.height  // 0 => height initialize
  else
    limitebmp2 := (limitebmp2*bmp2.height) div savebmp2H;
  savebmp2H := bmp2.height;      // clic limits
  bmp2.canvas.stretchdraw(rect(0,0,bmp2.width, bmp2.height), bmp1);
  bmp2.height := limitebmp2;
  bmp3.free;
  Bmp3 := Tbitmap.create;
  bmp3.width  := Bmp2.width;
  // decrease height to simulate point of view
  bmp3.height := (Bmp2.height *k1) div k2;
  Bmp3.pixelformat := pf24bit;       // 24 bits per pixel
  // vertical mirror
  Bmp3.Canvas.stretchDraw(Rect(0, Bmp2.height-1,Bmp2.width,-1), bmp2);
  bmp4.free;
  Bmp4 := Tbitmap.create;
  bmp4.width  := bmp3.width;
  bmp4.height := bmp3.height;
  Bmp4.pixelformat := pf24bit;
  bmp4.canvas.draw(0,0,bmp3);

  paintbox1.width  := bmp3.width;
  paintbox1.height := limitebmp2+ (bmp2.height*k1) div k2;
  Paintbox1.left := 0;
  Paintbox1.top  := 0;
  Paintbox1.canvas.draw(0,0,bmp2);
  Paintbox1.canvas.draw(0, limitebmp2, bmp4);
  //  scanline pointers optimization
  For i := 0 to bmp3.height-1 do
  begin
    Tscan3[i] := bmp3.scanline[i];
    Tscan4[i] := bmp4.scanline[i];
  end;
end;

procedure TAniSplash.mirror(ph : integer);
var
  h : single;          // bitmap height
  a : single;          // angle in radians
  dy :  single;        // compute with reals
  y3 :  integer;       // pixel source
  x4, y4 :  integer;   // pixel destination
  f : single;          // frames
  p : single;          // phase
  k0 : single;
  k1 : single;
  k2 : single;
  za : integer;
begin
  // phase (0..Frames-1) in radians for sinus variation
  p := ph;                  // phase
  h := bmp4.height;         // height into real
  f := frames;              // frames into real
  a := (2*pi*p) / f;
  k0 := 16;
  k1 := h/k0;
  k2 := k0*1.5;
  for y4 := 0 to bmp4.height -1 do   // from destination image
  begin
    dy := y4;  // en réel
    y3 := trunc( k1*(dy+k2) * sin((h/k1*(h-dy))/(dy+1)+a)/h );
    y3 := y3+y4;
    begin
      for x4 := 0 to bmp4.width-1 do
      begin
        if (y3 > 0) and (y3 < Bmp3.height) then
        Tscan4[y4,x4] := Tscan3[y3, x4] else Tscan4[y4,x4] := Tscan3[y4, x4];
      end;
    end;
  end;
end;

procedure TAniSplash.PaintBox1Paint(Sender: TObject);
begin
  with paintbox1.canvas do
  begin
    draw(0,0,bmp2);
    draw(0, bmp2.height-1, bmp4);
  end;
end;

procedure TAniSplash.Timer1Timer(Sender: TObject);
begin
  inc(phase);
  if phase >= frames then phase := 0;
  mirror(phase);
  paintbox1.canvas.draw(0, limitebmp2,bmp4);
end;

procedure TAniSplash.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  timer1.interval := 100;
end;

procedure TAniSplash.FormResize(Sender: TObject);
begin
 { timer1.enabled := false;
  limitebmp2 := 0;
  initmirror;
  IF not stop then timer1.enabled := true; }
end;

procedure TAniSplash.Panel2Click(Sender: TObject);
begin
timer1.Enabled:=true
end;

procedure TAniSplash.Image1Click(Sender: TObject);
begin
Close();
end;
const Lis :string =
  'CONTRAT DE LICENCE UTILISATEUR FINAL POUR PRINTABLE BOOK :'#13#10
 +#13#10
 +'- Printable Book est un logiciel Freeware (gratuit).   '#13#10
 +'- Il est interdit de le traduire, décompiler, modifier, adapter, '#13#10
 +'  et corriger. Seul l’auteur est habilité à effectuer ces opérations. '#13#10
 +'- En utilisant ce logiciel, vous vous engagez à respecter les droits '#13#10
 +'  d’auteur, et à veiller à ce que les autres utilisateurs les respectent'#13#10
 +'  eux-mêmes. '#13#10
 +'- Le logiciel est fourni tel quel, sans aucune garantie. L''auteur ne '#13#10
 +'  saurait voir sa responsabilité engagée en cas de dommages de quelque '#13#10
 +'  nature que ce soit subis par l''utilisateur ou des tiers et résultant '#13#10
 +'  directement ou indirectement de son utilisation, notamment la perte de '#13#10
 +'  données, ou toute perte financière résultant de son utilisation ou de '#13#10
 +'  l’impossibilité de l’utiliser, et ceci même si l’auteur a été prévenu '#13#10
 +'  de la possibilité de tels dommages.'#13#10
 +'- Vous êtes autorisé à copier et à distribuer le programme à d''autres '#13#10
 +'  utilisateurs de manière totalement gratuite dans son intégralité.'#13#10
 +#13#10
 +'Copyright © 2020 tous droits réservés.'#13#10
 +#13#10
 +'le 29/01/2020' ;
procedure TAniSplash.FormShow(Sender: TObject);
begin
    btopenclick(sender);
    ThreadAnim.Animate(Lis, MLicence);
end;

end.

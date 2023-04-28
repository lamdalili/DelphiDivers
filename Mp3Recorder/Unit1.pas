unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, Gauges, Buttons, ExtCtrls, Spin;

type
  TForm1 = class(TForm)  
    TimeLab: TLabel;
    BStop: TSpeedButton;
    BRec: TSpeedButton;
    Gauge1: TGauge;
    Timer1: TTimer;
    LevelBar: TScrollBar;
    SpinFeedBack: TSpinEdit;
    Label1: TLabel;
    BProp: TSpeedButton;
    procedure ButtzClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LevelBarChange(Sender: TObject);
    procedure BPropClick(Sender: TObject);
  private
    procedure SaveParams;
    procedure LoadParams;

  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;
  Looping:boolean;
implementation
uses MMSystem,UMP3Encode,ULameMP3API,Registry;
{$R *.dfm}
type
TRecThread =class(TThread)
  private
    procedure OnCapture(P1: Pointer; Size1: LongWord; P2: Pointer;
      Size2: LongWord);
protected
 FMp:TFileMp3;
 FAudioCapture:TAudioCapture;
 FPeek:integer;
 FRecLevel:integer;
 FFeedBack:1..100;
 procedure Execute;override;
public
 constructor Create(const AFilename:string);
 destructor Destroy();override;
end;


var
  tm:Cardinal;
  RecThread:TRecThread;

procedure TForm1.ButtzClick(Sender: TObject);
var
 S:string;
begin
 if not Promptforfilename(s,'*.mp3|*.mp3','mp3','','',true)then
    Exit;
 TControl(Sender).Enabled:=False;
 BProp.Enabled :=False;
 ClientHeight := Gauge1.BoundsRect.Bottom + 2;
 SaveParams();
 tm:=GetTickCount();
 RecThread:=TRecThread.Create(S);
 RecThread.FRecLevel := LevelBar.Position;
 RecThread.FFeedBack := SpinFeedBack.Value;
 RecThread.Resume;
 Timer1.Enabled :=True;
end;

procedure TForm1.SaveParams();
begin
    with TRegistry.Create do
    try
       RootKey :=HKEY_CURRENT_USER;
       if OpenKey('\Software\Mp3Rec',True)then
       begin
         WriteInteger('Level',LevelBar.Position);
         WriteInteger('Feed',SpinFeedBack.Value);
         CloseKey;
       end;
    finally
      Free;
    end;
end;

procedure TForm1.LoadParams();
begin
    with TRegistry.Create do
    try
       RootKey :=HKEY_CURRENT_USER;
       if OpenKey('\Software\Mp3Rec',False)then
       begin
         LevelBar.Position  := ReadInteger('Level');
         SpinFeedBack.Value := ReadInteger('Feed');
         CloseKey;
       end;
    finally
      Free;
    end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
G:Cardinal;
begin
 if not Assigned(RecThread)then
    Exit;
  G:= (GetTickCount() - tm) div 1000;
  TimeLab.Caption:=Format('%.2d:%.2d',[G div 60,G mod 60]);
  Gauge1.Progress:= RecThread.FPeek;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
    FreeAndNil(RecThread);
    BRec.Enabled :=true;
    Timer1.Enabled :=False;
    Bprop.Enabled :=True;
    TimeLab.Caption:='';
    Gauge1.Progress := Gauge1.MinValue;
end;

procedure TForm1.BPropClick(Sender: TObject);
begin
     ClientHeight := SpinFeedBack.BoundsRect.bottom + 2
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
     ReleaseCapture();
       Perform(WM_SYSCOMMAND,  $F012, 0);
end;


{ TRecThread }

constructor TRecThread.Create(const AFilename: string);
begin
    inherited Create(True);
    FAudioCapture:=TAudioCapture.Create;
    FMp:=TFileMp3.Create(AFilename);
    FFeedBack:=1;
end;

destructor TRecThread.Destroy;
begin
  Terminate;
  WaitFor;
  FMp.Free;
  FAudioCapture.Free;
  inherited;
end;

procedure TRecThread.Execute;
var
 wfx :TWaveFormatEx;
begin
    MakePCMFormat(wfx,44100,16,2);
    FAudioCapture.OnCapture:=OnCapture;
    FMp.SetFormat(44100,96,true);
    FMp.Open(true);
    FAudioCapture.Start(wfx);
end;

procedure TRecThread.OnCapture(P1:Pointer; Size1:LongWord; P2:Pointer;Size2:LongWord);
var
 D:Pword;
 I,Lv:integer;
begin
   if Terminated then
   begin
     FAudioCapture.Stop;
     Exit;
   end;

  D:=P1;
  Lv:=0;
  for I:= 0 to ((Size1 div 2)div 2)-1 do
  begin
     Lv:=Lv+D^;
     inc(D,2);
  end;
  Lv:=(Lv div(integer(Size1) div (2*2)));
  if FPeek < Lv then
     FPeek:= Lv
  else
     FPeek := FPeek - ((FPeek * FFeedBack) div 100 );
  if FPeek < 0 then
    FPeek:=0;
  if (FPeek >= FRecLevel) then
  begin
    FMp.Write(P1^, Size1);
    if Size2 > 0 then
        FMp.Write(P2^, Size2);
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
 LevelBar.Max :=Gauge1.MaxValue;
 LevelBar.Min :=Gauge1.MinValue;
 LoadParams();
 ClientHeight := Gauge1.BoundsRect.Bottom + 2;
end;

procedure TForm1.LevelBarChange(Sender: TObject);
begin
   Gauge1.Progress := LevelBar.Position
end;


end.


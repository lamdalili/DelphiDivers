unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ActnList;

type

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    Action1: TAction;
    BStart: TButton;
    GroupBox1: TGroupBox;
    VirtualPort: TEdit;
    VirBaud: TComboBox;
    GroupBox2: TGroupBox;
    PhyPort: TEdit;
    PhyBaud: TComboBox;
    BCancel: TButton;
    procedure BStartClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function WaitSignal():boolean;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    { Déclarations publiques }
  end;
 { TComThd=class(TThread)
  protected
    procedure Execute();override;
  end;}
var
  Form1: TForm1;

implementation
uses UWinSerial,Registry;
{$R *.dfm}
Function CreatePair(Port1, Port2 : PChar) : Boolean; stdcall; external 'VSPDCTL.DLL';
Function DeleteAll : Boolean; stdcall; external 'VSPDCTL.DLL';
var
 exitloop:boolean;

procedure TForm1.BStartClick(Sender: TObject);
var
 c,b,f,z:byte;
 rs1,rs2:TSerialLite;
begin
   exitloop:=false;
   rs1:=TSerialLite.Create;
   rs2:=TSerialLite.Create;
   BStart.Enabled:=False;
   BCancel.Enabled:=true;
   try
       rs1.Open(VirtualPort.Text,StrToint(VirBaud.Text));
       rs2.Open(PhyPort.Text,StrToint(PhyBaud.Text));
       rs1.OnWaitChar:=WaitSignal;
       rs2.OnWaitChar:=WaitSignal;
       repeat
         c:=rs1.read;
         rs2.write(c);
         case c of
          104:begin
            b:=rs1.read;
            rs2.write(b);
          end;
          105:begin
            //Hserin word1
            //Gosub send14
             b:=rs1.read;
             f:=rs1.read;
             rs2.write(b);
             rs2.write(f);
          end;
          106:begin
               // Gosub get14
               //	Hserout word1
             b:=rs2.read;
             f:=rs2.read;
             rs1.write(b);
             rs1.write(f);
           end;
          100:;
         else
            caption:='invalid:'+inttostr(c);
         end;
         z:=rs2.read;
         rs1.write(z);
         if z <> c then
           showmessagefmt('error %d:%d',[c,z]);
         application.ProcessMessages;
     until exitloop;
 finally
   BStart.Enabled:=true;
   BCancel.Enabled:=False;
   rs1.Free;
   rs2.Free;
 end;
end;

function TForm1.WaitSignal: boolean;
begin
   Result:=   exitloop;
   application.ProcessMessages;
end;

procedure TForm1.BCancelClick(Sender: TObject);
begin
  exitloop:=true
end;

procedure TForm1.LoadSettings;
var
  Reg:TRegistry;
begin
  Reg :=TRegistry.Create;
  try
      Reg.RootKey :=HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Virtual_Prog_com',False)then
      begin
         VirBaud.Text := Reg.ReadString('AppRate');
         PhyBaud.Text := Reg.ReadString('HardwareRate');
         Reg.CloseKey;
      end;
  finally
      Reg.Free;
  end;
end;

procedure TForm1.SaveSettings;
var
  Reg:TRegistry;
begin
  Reg :=TRegistry.Create;
  try
      Reg.RootKey :=HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Virtual_Prog_com',True)then
      begin
         Reg.WriteString('AppRate',VirBaud.Text);
         Reg.WriteString('HardwareRate',PhyBaud.Text);
         Reg.CloseKey;
      end;
  finally
      Reg.Free;
  end;

end;
procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadSettings();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveSettings();
end;

end.


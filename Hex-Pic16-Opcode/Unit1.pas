unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button2: TButton;
    BParse: TButton;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    GroupBox2: TGroupBox;
    Memo2: TMemo;
    StatusBar1: TStatusBar;
    procedure Button2Click(Sender: TObject);
    procedure BParseClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation
uses UIntelHex,UPicOpcode;
{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
var
 L:string;
 I:integer;
 checkSum:integer;
begin
   checkSum :=0;
  L:=edit1.Text;
  if L[1] <> ':' then
     raise Exception.Create('');
   for i := 0 to ((Length(L) -3) div 2)-1 do
     checkSum :=checkSum + Strtoint('$'+Copy(L,i*2+2,2));

    showmessagefmt('%x',[(256-checkSum )and $ff ]); //

end;

procedure TForm1.BParseClick(Sender: TObject);
var
kk:TDynBytearray;
s:string;
I:integer;
vv:TMcOpcodeKind;
 k:TIntelHex;

begin
     IntelHexToBin(Memo1.Lines,kk,true);
    // setlength(s,length(kk));
    // move(pointer(kk)^,pointer(s)^,length(kk));
    // memo1.Lines.Text:=s;
      memo2.Lines.Clear;
     MCDecode(kk,length(kk),memo2.Lines) ;

  { k:=TIntelHex.Create;
   k.WriteComment('test');

   k.WriteData(0,pointer(kk)^,length(kk)-2);
   k.WriteData(0,pchar(kk)[length(kk)-2],2);
   k.EndWriteData;
   k.SaveToFile('c:\oo.hex'); }
end;




function  BinToInt(const ABinStr:string):integer;
var
  I,BInc :integer;
begin
  Result := 0;
  BInc  :=1;
  for I := Length(ABinStr) downto 1 do
  begin
     case ABinStr[I] of
       '0':;
       '1': Result :=Result + BInc;
     else
        raise Exception.Create('Invalid char');
     end;
     BInc :=  BInc shl 1;
  end;
end;

end.

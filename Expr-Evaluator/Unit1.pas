unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, math, UExprEval;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    function EvalQuery(var Dest: Double; const AName: string;
      const Args: TExpArgs;nArgs:integer; ACall: boolean): Boolean;

    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
function TForm1.EvalQuery(var Dest:Double; const AName:string;const Args:TExpArgs;nArgs:integer; ACall:boolean):Boolean;
var
 I:integer;
begin
   Dest := 0;
   Result := True;
   if (AName = 'SQR') and (nArgs = 1 ) then
      Dest := Args[0] * Args[0]
   else if (AName = 'AVG') and (nArgs > 0) then
   begin
       for I:=0 to nArgs - 1 do
          Dest := Dest + Args[I];
       Dest := Dest / nArgs;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 Showmessagefmt('%.6f',[Eval(Edit1.Text, EvalQuery)]);
end;

end.

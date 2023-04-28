unit UExprEval;
{
    <Number> ::= [0-9]+ (.[0-9]+)? (E|e(-|+)? [0-9]+)?
    <Args>   ::= Expression(',' Expression)*
    ArgsOptional:=('(' Args? ')')
    <ParseIdent> ::= [A-Z]+
    <ReadIdentifier> ::= <ParseIdent> ArgsOptional?
    <Factor> ::= (+|-)<Factor> | <Number> | ReadIdentifier |'('<Expression>')'
    <Power>  ::= <Factor>(^<Factor>)*      //priorité 1
    <Term>   ::= <Power> (*|/ <Power>)*    //priorité 2
    <Expression> ::= <Term> (+|- <Term>)* //priorité 3
}
interface
uses
  SysUtils, Variants, Classes, math;
type
  EExpr=class(Exception);
  TExpArgs= array of Double;

  TEvalQuery=function (var Dest:Double; const AName:string;const Args:TExpArgs; nArgs:integer; ACall:boolean):Boolean of object;

  function Eval(const aTxt:string;const AEvalQuery:TEvalQuery = nil):Double; overload;

implementation
var
 fmtSetting: TFormatSettings;

type TStdOpration=(stNone, stSqrt, stSin, stCos, stTan, stExp, stLn, stAbs, stPi);
const STD_OPERATIONS:array [TStdOpration] of Ansistring =('','SQRT','SIN','COS','TAN','EXP','LN','ABS', 'PI');

function GetFunctionID(const AName:Ansistring):TStdOpration;
begin
   for Result := stSqrt to High(TStdOpration) do
     if AName = STD_OPERATIONS[Result] then
        Exit;
   Result := stNone;
end;

function GetConstant(AConstantID:TStdOpration):Double;
begin
   Result := 0;
   if AConstantID = stPi then
      Result:= PI;
end;

function Exec(AOprID:TStdOpration; const Args:TExpArgs; ACall:boolean):Double;
var
 i,ArgsLen:integer;
begin
    if not ACall then
    begin
      Result := GetConstant(AOprID);
      Exit;
    end;
    Result:=0;
    ArgsLen:= Length(Args);
    if ArgsLen = 1 then
    begin
      case AOprID of
         stSqrt: Result:= Sqrt(Args[0]);
          stSin: Result:= Sin(Args[0]);
          stCos: Result:= Cos(Args[0]);
          stTan: Result:= Tan(Args[0]);
          stExp: Result:= Exp(Args[0]);
           stLn: Result:= LN(Args[0]);
          stAbs: Result:= ABS(Args[0]);
       end;
    end;
end;

function UnairyOperation(OP:AnsiChar;const Value:Double):Double;pascal;
begin
    case OP of
        '-': Result:= - Value;
    else
        Result:= Value;
    end;
end;

procedure BinaryOperation(OP:AnsiChar;var Value1:Double;const Value2:Double);pascal;
begin
    case OP of
         '+':  Value1 := Value1 + Value2;
         '-':  Value1 := Value1 - Value2;
         '*':  Value1 := Value1 * Value2;
         '/':  Value1 := Value1 / Value2;
         '^':  Value1 := Power(Value1, Value2);
     end;
end;

function Eval(const aTxt:string;const AEvalQuery:TEvalQuery):Double;
var
    CurrentChar:AnsiChar;
    fCode:Ansistring;
    fPos :integer;

    function GetExpression(): Double; forward;
    function NextChar():AnsiChar;
    begin
       if fPos < Length(FCode) then
       begin
         repeat
            inc(fPos);
            Result := fCode[fPos];
         until Result <> ' ';
       end else begin
         Result := #0;
         fPos := Length(FCode)+1;
       end;
       CurrentChar := Result;
    end;

    procedure SynErr(ARaise:boolean = True);
    begin
       if ARaise then
          raise EExpr.CreateFmtHelp('Erreur à la position %d',[fPos], 0);
    end;
    //<NUMBER> ::= [0-9]+ (.[0-9]+)? (E|e(-|+)? [0-9]+)?
    function ReadDouble:Double;
    var
       Start:integer;
    begin
         Start:=fPos;
         while CurrentChar in ['0'..'9'] do
            NextChar();
         if CurrentChar = '.' then
         begin
            while NextChar() in ['0'..'9'] do
               ;
         end;
         If CurrentChar = 'E' then
         begin
            if NextChar() in ['-','+'] Then
               NextChar();
            while NextChar() in ['0'..'9'] do
               ;
         end;
         Result := Strtofloat(string(Copy(fCode, Start, fPos - Start)), fmtSetting);
    end;
    function ParseIdent:AnsiString;
    var
       Start,Len:integer;
    begin
        Start := fPos;
        while CurrentChar in ['A'..'Z','_','0'..'9'] do
           NextChar;
        Len :=fPos - Start;
        SynErr(Len = 0);
        Result := Copy(fCode, Start, Len);
    end;
    function GetArgsList():TExpArgs;
    var
      nArgs:integer;
    begin
        Result:=nil;
        if CurrentChar = '(' then
        begin
           nArgs:=0;
           if fCode[fPos+1] = ')' then // empty arguments
              NextChar()
           else
             repeat
                Setlength(Result,nArgs + 1);
                Result[nArgs] := GetExpression();
                inc(nArgs);
             until CurrentChar <> ',';
           SynErr(CurrentChar <> ')');
           NextChar();
        end;
    end;
    //<IDENT> ::= [A..Z]+
    function ReadIdentifier:Double;
    var
      Name:Ansistring;
      Opr:TStdOpration;
      IsCall:boolean;
      Args:TExpArgs;
    begin
        Name:= ParseIdent();
        IsCall := CurrentChar = '(';
        Args := GetArgsList();
        Opr:= GetFunctionID(Name);
        if Opr <> stNone then //std name
        begin
           Result := Exec(Opr, Args, IsCall)
        end else
        begin
           if not Assigned(AEvalQuery)
               or not AEvalQuery(Result, string(Name),Args, Length(Args), IsCall) then
                 raise EExpr.CreateFmt('Unknown variable "%s"',[Name]);
        end;
    end;

    //<Factor> ::= (+|-)<Factor>| <NUMBER> |'('<Expression>')'
    function GetFactor(): Double;
    begin
        case NextChar of
          '0'..'9': Result := ReadDouble();// <NUMBER>
          'A'..'Z': Result := ReadIdentifier(); //<IDENT>  
           '+','-': Result := UnairyOperation(CurrentChar, GetFactor()); // (+|-)<Factor>
               '(':begin // '('<Expression>')'
                      Result := GetExpression();
                      SynErr(CurrentChar <> ')');
                      NextChar();
                   end;
            else
                SynErr;
            Result := 0; // dummy
        end;
    end;
    //<Power> ::= <Factor>(^<Factor>)*
    function GetPower():Double;
    begin
         Result := GetFactor();
         while CurrentChar = '^' do
            BinaryOperation('^', Result, GetFactor());
    end;
    //<Term> ::= <Factor>(^<Factor>)*
    function GetTerm():Double;
    begin
         Result :=  GetPower();
         while CurrentChar in ['*','/'] do // must called in this order to avoid side effet; BinaryOperation uses pascal directeve
             BinaryOperation(CurrentChar, Result, GetPower());
    end;
    //<Expression>  ::= <Term> (+|- <Term>)*
    function GetExpression():Double;
    begin
         Result := GetTerm();
         while CurrentChar in ['+','-'] do // must called in this order to avoid side effet; BinaryOperation uses pascal directeve
             BinaryOperation(CurrentChar, Result, GetTerm());
    end;
begin
   fPos:=0;
   FCode := Ansistring(UpperCase(aTxt));
   Result := GetExpression();
   SynErr(CurrentChar <> #0);
end;


begin
  fmtSetting := FormatSettings;
  fmtSetting.DecimalSeparator := '.';
end.


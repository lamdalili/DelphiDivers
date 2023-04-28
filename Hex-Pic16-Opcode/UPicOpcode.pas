unit UPicOpcode;

interface
uses
   SysUtils, Classes,dialogs;

type
    TMcOpcodeKind =(mkReg,mkBit,mkLitControl,mkJump,mkNoOperand);

    TMcDest =(md_W,md_F);

    TMcOpcode =(opInvalid,opADDWF, opANDWF, opCLRF, opCLRW, opCOMF, opDECF, opDECFSZ,
    opINCF,opINCFSZ,opIORWF, opMOVF, opMOVWF, opNOP, opRLF, opRRF, opSUBWF,
    opSWAPF, opXORWF,opBCF,opBSF, opBTFSC, opBTFSS, opADDLW, opANDLW, opCALL,
    opCLRWDT, opGOTO, opIORLW,opMOVLW , opRETFIE, opRETLW, opRETURN, opSLEEP,
    opSUBLW, opXORLW);

    TInstData = record
       mOpCode:TMcOpcode;
       mInstink:TMcOpcodeKind;
       case integer of
         0:(mFile:integer;
            case integer of
                0:( mDest:TMcDest);
                1:( mBitIdx:integer);
           );
          1:( mLiteral:integer);
    end;

    TPicOpcode =class
    private
      FList :TStrings;
      FLabelsList:TList;
   //   procedure Add(AOpcode:integer;const AName:string);
    public
      constructor Create;
      destructor Destroy;override;
    //  procedure Decode(AOpList:array of byte;ACount:integer);
    end;

   function InstDecode(AOpcode:Integer;var AOpkink:TMcOpcodeKind):TMcOpcode;
   function ExpandInst(AOpcode:Integer;var AOut:TInstData):boolean;
   procedure MCDecode(AOpList:array of byte;ACount:integer;AList:TStrings);

const OPCODENAMES:array[TMcOpcode] of string =
('Invalid','addwf','andwf','clrf','clrw','comf','decf','decfsz','incf','incfsz','iorwf',
'movf','movwf','nop','rlf','rrf','subwf','swapf','xorwf','bcf','bsf','btfsc',
'btfss','addlw','andlw','call','clrwdt','goto','iorlw','movlw ','retfie','retlw',
'return','sleep','sublw','xorlw');

const OPCODVALUES:array[TMcOpcode] of integer =
 ($FFFF,$0700,$0500,$0180,$0100,$0900,$0300,$0B00,$0A00,$0F00,$0400,$0800,$0080,
 $0000,$0D00,$0C00,$0200,$0E00,$0600,$1000,$1400,$1800,$1C00,$3E00,$3900,
 $2000,$0064,$2800,$3800,$3000,$0009,$3400,$0008,$0063,$3C00,$3A00);
implementation

var
  FSRTable:TList;

type
  PFSRReg=^TFSRReg;
  TFSRReg=record
    mName:string;
    Bits:array[0..7]of string;
  end;
{ TPicOpcode }

constructor TPicOpcode.Create;
begin
   FLabelsList:= TList.Create;
   FList:= TStringList.Create;
end;

destructor TPicOpcode.Destroy;
begin
  FList.Free;
  FLabelsList.Free;
  inherited;
end;

function InstDecode(AOpcode: Integer;var AOpkink:TMcOpcodeKind): TMcOpcode;
const OPARR:array [0..15] of TMcOpcode =(opInvalid,opInvalid,opSUBWF, opDECF,opIORWF ,
     opANDWF ,opXORWF ,opADDWF ,opMOVF ,opCOMF ,opINCF,opDECFSZ,
    opRRF ,opRLF ,opSWAPF ,opINCFSZ);
var
 t:integer;
begin
    case  AOpcode and $3000 of
      0:begin
         AOpkink := mkReg;
         t  := AOpcode and $F00;
         if t >= $200 then
            Result := OPARR[t shr 8]
        else
          case AOpcode and $180 of
             $180: Result := opCLRF;
             $100: Result := opCLRW;
             $080: Result := opMOVWF;
           else
                case AOpcode and $7F of
                   $09: Result := opRETFIE;
                   $08: Result := opRETURN;
                   $63: Result := opSLEEP;
                   $64: Result := opCLRWDT;
                   $00: Result := opNOP;
                 else
                   Result := opInvalid;
               end;
          end;
      end;
      $1000:begin
           AOpkink := mkBit;
           case  AOpcode and $C00 of
               $000: Result := opBCF;
               $400: Result := opBSF;
               $800: Result := opBTFSC;
               $C00: Result := opBTFSS;
           end;
      end;
      $3000:begin
           AOpkink := mkLitControl;
           case  AOpcode and $C00 of
               $000: Result := opMOVLW;
               $400: Result := opRETLW;
               $800: case  AOpcode and $300 of
                       $000: Result := opIORLW;
                       $100: Result := opANDLW;
                       $200: Result := opXORLW;
                     else
                       Result := opInvalid;
                     end;
               $C00:if (AOpcode and $200 )<> 0 then
                        Result := opADDLW
                     else
                        Result := opSUBLW;
           end;
      end;
      $2000:begin
            AOpkink := mkJump;
            if (AOpcode and $800 )<> 0 then
                 Result := opGOTO
            else
                  Result := opCALL;
            end;
     end;
end;

function ExpandInst(AOpcode:Integer;var AOut:TInstData):boolean;
var
  Opkink:TMcOpcodeKind;
begin
   with AOut do
   begin
        mOpCode  :=  InstDecode(AOpcode,mInstink);
        Result := mOpCode <>opInvalid;
        if not Result then
            Exit;
        if mOpCode in [opNop,opReturn,opClrW,opCLRWDT,opSleep,opRetfie ] then
           mInstink := mkNoOperand
        else
          case mInstink of
                mkReg:begin
                   mFile :=  AOpcode and $7F;
                   mDest := TMcDest((AOpcode shr 7) and 1) ;
                end;
                mkBit:begin
                   mFile :=  AOpcode and $7F;
                   mBitIdx := (AOpcode shr 7) and 7 ;
                end;
                mkLitControl:begin
                   mLiteral :=  AOpcode and $FF;
                end;
                mkJump:begin
                   mLiteral :=  AOpcode and $7FF;
                end;
          end;
  end;
end;

function FileDecode(Address:integer):string;
begin
   if Address >= FSRTable.Count then
      Result:='0x'+InttoHex(Address,2)
   else
      Result:=PFSRReg(FSRTable[Address]).mName;
end;

function FileBitsDecode(Address,Bit:integer):string;
var
 t:string;
begin
   if Address >= FSRTable.Count then
      Result:='0x'+InttoHex(Address,2)+','+inttostr(Bit)
   else with PFSRReg(FSRTable[Address])^ do
   begin
      t:=Bits[7-Bit];
      if t='' then
         t:=inttostr(Bit);
      Result:=mName+','+t;
   end;
end;

procedure MCDecode(AOpList:array of byte;ACount:integer;AList:TStrings);
type
  TLabelName= array[0..3 ] of char;

const DESTLOC :array[TMcDest] of char =('W','F');
var
 LabelsList:TList;
 DumpList:TStrings;
 I,L,Pz,Lab:integer;
 V:Word;
 InstData:TInstData;
 S:string;
 P:Pointer;
begin
   if Length(AOpList) < ACount then
      Exit;
   LabelsList:=TList.Create;
   DumpList:=TStringList.Create;
   AList.BeginUpdate;
   try
     Pz:=1;
     for I :=  0 to ACount div 2 -1  do
     begin
         V:=AOpList[I*2]or (AOpList[I*2+1] shl 8);
        if ExpandInst(V,InstData )then
          with InstData do
            case mInstink of
                  mkReg:begin
                     if InstData.mOpCode in [opMOVWF,opCLRF] then
                       S := Format('%-7s%s',[OPCODENAMES[mOpCode],FileDecode(mFile)])
                     else
                       S := Format('%-7s%s,%s',[OPCODENAMES[mOpCode],FileDecode(mFile),DESTLOC[mDest]])
                  end;
                  mkBit:begin
                     S := Format('%-7s%s',[OPCODENAMES[mOpCode],FileBitsDecode(mFile,mBitIdx)]);
                  end;
                  mkLitControl:begin
                     S := Format('%-7s0x%.2x',[OPCODENAMES[mOpCode],mLiteral]);
                  end;
                  mkJump:begin
                   // S := Format('%s 0x%.x',[OPCODENAMES[mOpCode],mLiteral]);
                    if mLiteral >= LabelsList.Count then
                       LabelsList.Count:=mLiteral+1;
                    Lab:=Integer(LabelsList[mLiteral]);
                    if Lab=0 then
                    begin
                       Lab:=Pz;
                       Inc(Pz);
                       LabelsList[mLiteral]:=Pointer(Lab);
                    end;
                    S := Format('%-7sL%d',[OPCODENAMES[mOpCode],Lab]);
                  end;
                  mkNoOperand:begin
                     S := Format('%s',[OPCODENAMES[mOpCode]]);
                  end;
            end
          else
             S := Format('unknown: 0x%.4x',[V]);
          DumpList.Add(S);
     end;
   //  DumpList.Count:= Pz;
     LabelsList.Count:= DumpList.Count;
     for I :=  0 to DumpList.Count -1  do
     begin
       if LabelsList[I] <> nil then
            AList.Add( Format('L%-4s %s',[Inttostr(Integer(LabelsList[I]))+':',DumpList[I]]))
        else
           AList.Add('      '+DumpList[I]);
     end;

   finally
      AList.EndUpdate;
      DumpList.Free;
      LabelsList.Free;
   end;
end;

procedure LoadTables();
var
  tsp:TStringList;
  procedure AddFSR(Address:integer;const S:string);
  var
   I:integer;
   P:PFSRReg;
  begin
      if Address >= FSRTable.Count then
      begin
        FSRTable.Count:=Address+1;
      end;
      New(P);
      FSRTable[Address]:=P;
      tsp.DelimitedText := S;
      P.mName :=tsp[0];
      for I:= 1 to tsp.Count-1 do
      begin
         if I = 9 then
            break;
         P.Bits[I-1]:=tsp[I];
      end;
  end;
begin
   FSRTable:=TList.Create;
   tsp:=TStringList.Create;
   try
   //  tsp.Delimiter:=';';
     AddFSR(0,'INDF');
     AddFSR(1,'TMR0');
     AddFSR(2,'PCL');
     AddFSR(3,'STATUS IRP RP1 RP0 TO PD Z DC C');
     AddFSR(4,'FSR');
     AddFSR(5,'PORTA');
     AddFSR(6,'PORTB');
     AddFSR(7,'PORTC');
     AddFSR(8,'PORTD');
     AddFSR(9,'PORTE');
     AddFSR($A,'PCLATH');
   //  AddFSR($B,'');
  //   AddFSR(,'');
  //   AddFSR(,'');
   finally
     tsp.Free;
   end;
end;

initialization

  LoadTables;
end.

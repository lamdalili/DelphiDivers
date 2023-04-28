unit UIntelHex;

interface

uses
 SysUtils, Variants, Classes;

type

TDynBytearray =array of byte;
PHexByte =^THexByte;
THexByte = array [0..1] of char;
THexWord = packed record
  case integer of
   0:(Chars:array [0..3] of char);
   1:(mHi,mLo: THexByte );
 end;

TIntelHexData =packed record
  mLineBom :char;
 case integer of
 0:(HexBytes:array[0..0] of THexByte);
 1:(
  mDataSize:THexByte;
  mAddress  :THexWord;
  mLineType:THexByte;
   case integer of
   0:( mExtraAddr : THexWord);
   1:( mHexData :array [0..0] of THexByte)
  );
end;
  TIntelHexRecord = packed record
     mAddress:integer;
     mSize:integer;
     mData:array[0..15]of byte;
  end;
TIntelHexLineType=(ltUnknown,ltData,ltExtraAddress,ltEnd);
TIntelHexOnReadBuffer = procedure (const AData;ACount,Adress:integer);

TIntelHex= class
private
  FHexStrings:TStrings;
  FData:TDynBytearray;
  FExpandAddress:boolean;
  procedure InternalWriteData(address:word;AData:PByte;ACount:integer);
public
  constructor Create();
  destructor Destroy();override;
  procedure LoadFomFile(const AFilename:string);
  procedure SaveToFile(const AFilename:string);
  procedure Clear;
  procedure WriteComment(const AStr:string);
  procedure WriteData(const AData;ACount:integer;address:word);
  procedure EndWriteData();
  property Data:TDynBytearray read FData;
  property ExpandAddress :boolean read FExpandAddress write FExpandAddress;
end;

function IntelHexParseLine(const AHexLine:string;var AOut:TIntelHexRecord):TIntelHexLineType;
procedure IntelHexToBin(AHexStrings:TStrings;var AOut:TDynBytearray;ExpandAdress:boolean);

implementation

function HexToInt(const AHex :string):integer;
begin
   Result :=  Strtoint('$'+AHex);
end;

function IntelHexParseLine(const AHexLine:string;var AOut:TIntelHexRecord):TIntelHexLineType;
var
 Hd:^TIntelHexData absolute AHexLine;
 I,J,Len:integer;
 recType,checkSum:integer;
begin
   Result :=ltUnknown;
   with AOut do
   begin
           if  Length(AHexLine) < 2  then
               Exit;
           if Hd.mLineBom <> ':' then
               Exit;;
           mSize  := HexToInt(Hd.mDataSize);

           if  Length(AHexLine) <> ((mSize + 5  )*2 +1) then
              Exit;

           mAddress  := HexToInt(Hd.mAddress.Chars);
           recType := HexToInt(Hd.mLineType) ;
           checkSum := 0 ;
            for i := 0 to (mSize + 5)-1 do
                checkSum :=checkSum + HexToInt(Hd.HexBytes[I]);

           if ( checkSum and $ff) <> 0 then
               Exit;
           case recType of
               0:begin
                    Result :=ltData;
                    for i := 0 to mSize-1 do
                      mData[i] := HexToInt(HD.mHexData[i]);
                 end;
               1:begin
                   Result :=ltEnd;
                 end;
               2:begin
                   Result :=ltExtraAddress;
                   mAddress := HexToInt(HD.mExtraAddr.Chars)*16;
                 end;
            end;
   end;

end;

procedure IntelHexToBin(AHexStrings:TStrings;var AOut:TDynBytearray;ExpandAdress:boolean);
const
  MAX_MEM=$4000;// for test
var
 I,J,ddr,extraAddr:integer;
 L:string;
 D:TIntelHexRecord;
begin
   extraAddr:=0;
   for i := 0 to AHexStrings.Count-1 do
   begin
           L:=AHexStrings[i];
           if  Length(L) < 2  then
               Break;

           if L[1] = ';' then
               continue;

           case IntelHexParseLine(L,D) of
               ltUnknown:begin
                      raise Exception.Create('Error Hex file');
                 end;
               ltData:begin
                     if ExpandAdress then
                        ddr  :=D.mAddress + extraAddr
                     else
                        ddr  := Length(AOut);
                     // ddr may contain special address :$400E  fuses
                     if ddr > MAX_MEM then
                        continue;
                     if (Length(AOut)< ddr + D.mSize) then
                        SetLength(AOut, ddr + D.mSize);
                     for J := 0 to D.mSize -1 do
                         AOut[ddr + J] := D.mData[J];

                 end;
               ltExtraAddress:begin
                     extraAddr := D.mAddress * 16;
                 end;
               ltEnd:begin
                  Break;
               end;
            end;
    end;
end;

function WordToHex(AValue:integer):THexWord;
var
 S :string[4];
begin
   s:=sysutils.Inttohex(word(AValue),4);
   Result.Chars[0]:=S[1];
   Result.Chars[1]:=S[2];
   Result.Chars[2]:=S[3];
   Result.Chars[3]:=S[4];
end;

function ByteToHex(AValue:integer):THexbyte;
var
 S :string[4];
begin
   s:=sysutils.Inttohex(byte(AValue),2);
   Result[0]:=S[1];
   Result[1]:=S[2];
end;

{ TIntelHex }

constructor TIntelHex.Create;
begin
   FHexStrings:=TStringList.Create;
end;

destructor TIntelHex.Destroy;
begin
  FHexStrings.Free;
  inherited;
end;

procedure TIntelHex.LoadFomFile(const AFilename: string);
begin
   Clear;
   FHexStrings.LoadFromFile(AFilename);
   IntelHexToBin(FHexStrings,FData,FExpandAddress);
end;

procedure TIntelHex.SaveToFile(const AFilename: string);
begin
   FHexStrings.SaveToFile(AFilename);
end;

procedure TIntelHex.Clear();
begin
   FHexStrings.Clear;
   Setlength(FData,0);
end;

procedure TIntelHex.EndWriteData;
begin
    FHexStrings.Add(':00000001FF');
end;



procedure TIntelHex.WriteComment(const AStr: string);
begin
    FHexStrings.Add(';'+AStr);
end;

procedure TIntelHex.InternalWriteData(address: word; AData:PByte;ACount: integer);
var
  L:string;
  I,DataLen,Sum,Value:integer;
  Hd:^TIntelHexData absolute L;
  W:THexWord;
begin
   DataLen := (ACount + 5  )*2 + 1;
   Setlength(L,DataLen);
   Hd.mLineBom  := ':';
 //  W := IntToHex(ACount);

   Hd.mDataSize := ByteToHex(ACount);
   Hd.mAddress   := WordTohex(address);
   Hd.mLineType :='00';
   Sum := ACount + Hi(address)+ Lo(address);
   for I := 0 to ACount -1 do
   begin
      Value := AData^;
      Sum := Sum + Value;
      Hd.mHexData[I]:=ByteToHex(Value);
      inc(AData);
   end;
   PHexByte(@L[DataLen-1])^:= ByteToHex((256-Sum));
   FHexStrings.Add(L);
end;

procedure TIntelHex.WriteData(const AData;ACount:integer;address:word);
var
   I,C,M:integer;
   P:PByte;
begin
    P:=@AData;
    C  := ACount div 16;
    M  := ACount mod 16;
    for I := 0 to C -1 do
    begin
        InternalWriteData(address,P,16);
        address :=address +16;
        inc(P,16);
    end;

    if M <> 0 then
    begin
      InternalWriteData(address,P,M);
    end;
end;


end.

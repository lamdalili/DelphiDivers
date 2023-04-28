unit UWinSerial;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes;
type // no overlap
  TWaitCharEvent=function ():boolean of object;
  TSerialLite=class
  protected
    FHandle:THANDLE;
    function Dummy():boolean;
  public
    OnWaitChar:TWaitCharEvent;
    function Open(const APort:string;ABaudRate:integer=CBR_9600):boolean;
    function read():integer;
    function write(c:byte):boolean;
    procedure Close();
    destructor Destroy();override;
    property Handle:THANDLE read FHandle;
  end;

implementation
const SERIAL_TIMEOUT =150000;


function TSerialLite.Open(const APort:string;ABaudRate:integer):boolean;
var
  portState:DCB;
begin
  Result:=False;
	FHandle := CreateFile(PChar(APort), GENERIC_READ or GENERIC_WRITE,
		0, 0, OPEN_EXISTING, 0, 0);
	if (FHandle = INVALID_HANDLE_VALUE)then
	   raise Exception.Create('can not use port :'+APort);



	if not GetCommState(FHandle, portState) then
     raise Exception.Create('can not use port :'+APort);


	portState.BaudRate := ABaudRate;
	portState.StopBits := ONESTOPBIT;
	portState.ByteSize := 8;
	portState.Flags:=0;
	portState.Parity := NOPARITY;
	if  not SetCommState(FHandle, portState) then
    Exit;
  OnWaitChar:=Dummy;

  if not PurgeComm(FHandle, PURGE_TXCLEAR or PURGE_RXCLEAR or PURGE_TXABORT or PURGE_RXABORT)then
     Exit;
	Result:=True;
end;

function TSerialLite.write(  c:byte):boolean;
var
  written:DWORD;
begin
  Result:=False;

	if not WriteFile(FHandle, c, 1, written, nil) then
  begin
    Exit;
	end;

	Result:= True;
end;

function TSerialLite.read():integer;
var
  bytesRead:DWORD;
  c:byte;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  Result:=0;
  repeat
    if not ClearCommError(FHandle, Errors, @ComStat)or  OnWaitChar() then
       Exit;
    if (ComStat.cbInQue >0) then
        break;
    sleep(0);
  until False;

	if not ReadFile(FHandle, c, 1, bytesRead,nil) then
      Exit;

	Result:= c;
end;

procedure TSerialLite.Close;
begin
   if (FHandle <> 0)and (FHandle <> INVALID_HANDLE_VALUE) then
      Closehandle(FHandle);
   FHandle:=0;
end;

function TSerialLite.Dummy: boolean;
begin
   Result:= False;
end;

destructor TSerialLite.Destroy;
begin
  Close;
  inherited;
end;

end.

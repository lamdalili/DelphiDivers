unit UWaveOut;

interface
uses
     Windows, Messages, SysUtils, Variants, Classes,MMSystem;
type
   TGetWaveData =procedure (P:Pointer;ABufSize:integer;var Readed:integer) of object;
   TWaveOut=class
   private
    FWFormat:PWaveFormatEx;
    FCurBuf:integer;
    FBufLen:integer;
    FHeaders: array of TWaveHdr;
    FHandle: HWaveOut;
    FWindow: HWND;
    FOnGetWaveData: TGetWaveData;
    procedure WinProc(var Message: TMessage);
   public
    constructor Create(ABufLen:integer;pWF:PWaveFormatEx);
    procedure Init();
    procedure WriteBuffer();
    procedure Close;
    property OnGetWaveData:TGetWaveData read FOnGetWaveData write FOnGetWaveData ;
    property WFormat:PWaveFormatEx read FWFormat;
   end;

implementation
uses dialogs;
{procedure waveOutProc(Handle:HWaveOut; Msg:integer; WaveOut:TWaveOut; WaveHdr:PWaveHdr; dwReserved:integer); stdcall;
begin
 case Msg of
  WOM_CLOSE:;
  WOM_DONE :WaveOut.WriteBuffer;
  WOM_OPEN : ;
 end;

end;  }

{ TWaveOut }
const
 // BUFFER_LENGTH = 100;
  NUM_BUFFERS = 2;

constructor TWaveOut.Create(ABufLen:integer;pWF:PWaveFormatEx);
begin
  if (pWF =nil) or (FBufLen mod pWF.nBlockAlign<>0)then
     raise Exception.Create('Invalid param');
  FWFormat:=pWF;
  FBufLen :=ABufLen;
  
end;

procedure TWaveOut.WinProc(var Message: TMessage);
begin
   with Message do
   if Msg=  WOM_DONE then
     WriteBuffer()
   else
    Result:=DefWindowProc(FWindow,Msg,WParam,LParam);

end;

procedure TWaveOut.Init;
var
  I:Integer;
begin
   FCurBuf:=0;
  // with FWFormat^ do
  //   FBufLen := ((nAvgBytesPerSec * BUFFER_LENGTH)div (1000 * nBlockAlign)) * nBlockAlign;

  { waveOutOpen( @FHandle, WAVE_MAPPER,FWFormat, DWord(@waveOutProc),
      Integer(self), CALLBACK_FUNCTION or WAVE_ALLOWSYNC );  }
  FWindow :=AllocateHWnd(WinProc);
    waveOutOpen( @FHandle, WAVE_MAPPER,FWFormat, FWindow,
      0, CALLBACK_WINDOW or WAVE_ALLOWSYNC );
    Setlength(FHeaders,NUM_BUFFERS);
    ZeroMemory(Pointer(FHeaders), SizeOf(TWaveHdr)*NUM_BUFFERS);
    for I := 0 to NUM_BUFFERS-1 do
    begin
          with FHeaders[I] do
          begin
            GetMem(lpData, FBufLen );
            dwBufferLength := FBufLen;
            dwFlags := WHDR_DONE;
          end;
          waveOutPrepareHeader(FHandle, @(FHeaders[I]), SizeOf(TWaveHdr));
    end;

    for I := 0 to NUM_BUFFERS-1 do
      WriteBuffer;
end;

procedure TWaveOut.WriteBuffer;
var
  R: Integer;
begin
  //if (Closing) then exit;
 // with  FWFormat^  do
 // D := (FBufLen div nBlockAlign )* nBlockAlign;
  R:=0;
  FOnGetWaveData(FHeaders[FCurBuf].lpData,FBufLen,R);

  if (R > 0) then
  begin
    if FBufLen <> R then
       ZeroMemory( PAnsiChar(FHeaders[FCurBuf].lpData) + R,FBufLen - R );
    waveOutWrite( FHandle, @FHeaders[FCurBuf],SizeOf(TWaveHdr));
  end
  else
   Close;
 {   if (Ending < NUM_BUFFERS) then
      Inc( Ending )
    else
      PostMessage( Handle, WM_USER, 0, 0 );
  }
  FCurBuf := (FCurBuf + 1) mod NUM_BUFFERS;

end;
procedure TWaveOut.Close;
var
  I: Integer;
  P: Pointer;
begin
  if (FHandle <> 0) then
  begin
    //Closing := True;

    waveOutReset(FHandle);

    for I := 0 to NUM_BUFFERS-1 do
    begin
      P := FHeaders[I].lpData;
      waveOutUnprepareHeader( FHandle, @FHeaders[I], SizeOf(TWaveHdr) );
      FreeMem(P);
    end;
    waveOutClose(FHandle);
    DeallocateHWnd( FWindow);
    FHandle := 0;

  end;
end;



end.


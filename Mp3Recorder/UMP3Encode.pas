unit UMP3Encode;


interface

uses windows,SysUtils,Classes,Forms,MMSystem,ULameMP3API;


type

  TStreamModes = set of (smRead, smWrite, smSetSize);
  TCustomLinkStream = class(TStream)
  private
    FStream : TStream;
    FOffset : Integer;
    FSize   : Integer;
    FModes   : TStreamModes;
    procedure SetLimitSize(const Value: integer);
    procedure SetOffset(const Value: integer);
 protected
    function ReadData(var Buffer; Count: Integer): Integer; virtual;
    function WriteData(const Buffer; Count: Integer): Integer; virtual;
    function GetSize():Int64;override;
    procedure SetSize(const NewSize: Int64);override;
    property Modes:TStreamModes read FModes write FModes;
    property Offset:integer read FOffset write SetOffset;

  public
    constructor Create(AStream:TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property LimitSize:integer read FSize write SetLimitSize;
 end;
 TStreamHeaded=class(TCustomLinkStream)
 protected
   FOpened: Boolean;
   FWriteMode: Boolean;
   function OpenCreate:boolean;virtual;abstract;
   function OpenRead:boolean;virtual;abstract;
   procedure InternalClose;virtual;
 public
   procedure Open(WriteMode :boolean);
   procedure Close();
 end;

 TPCMWave=class(TStreamHeaded)
 private
    FFormat: TWaveFormatEx;
    FDataSize: integer;
    procedure SetWaveFormat(const Value: TWaveFormatEx);
 protected
   FDataHeaderPos:integer;
   FDataPos:integer;
   procedure ReadProp(Prop:Cardinal;var Buff;Size:integer);
   procedure WriteProp(Prop:Cardinal;const Buff;Size:integer);
   function OpenCreate:boolean;override;
   function OpenRead:boolean;override;
   procedure InternalClose;override;
 public
   property WaveFormat:TWaveFormatEx read FFormat write SetWaveFormat;
   property DataSize:integer read FDataSize;
   procedure SetPCMFormat(SamplesPerSec, BitsPerSample, Channels: Integer);
 end;

 TLameMp3Encoder=class(TStreamHeaded)
 private
   FFormat: TBE_Config;
   FReadLen, FWriteLen: LongWord;
   FHandle :THandle;
   FBuff:array of Byte;
 protected
   function WriteData(const Buffer; Count: Integer): Integer; override;
   function OpenCreate:boolean;override;
   function OpenRead:boolean;override;
   procedure InternalClose;override;
 public
   procedure SetFormat(SamplesPerSec, Bitrate:integer;Stereo: boolean);

 end;

 TFileWave=class(TPCMWave)
 private
  FStream:TStream;
 public
   constructor Create(const Filename:string;AFileMode:Word;ALimitSize:integer=0);
   destructor Destroy();override;
 end;

 TFileMp3=class(TLameMp3Encoder)
 private
  FFilename:string;
  FStream:TStream;
 public
   constructor Create(const Filename:string);
   destructor Destroy();override;
   property Filename :string read FFilename;
 end;
 TCaptureEvent=procedure (P1:Pointer; Size1:LongWord; P2:Pointer;Size2:LongWord)of object;
 TAudioCapture=class
 private
   FLooping:boolean;
   FCaptureEvent:TCaptureEvent;
   FOnStartCapture:TNotifyEvent;
   FOnEndCapture:TNotifyEvent;
 public
   procedure Start(const WFormat:TWaveFormatEx);
   procedure Stop();
   property IsCapturing :boolean  read FLooping;
   property OnCapture :TCaptureEvent  read FCaptureEvent write FCaptureEvent ;
   property OnStartCapture :TNotifyEvent  read FOnStartCapture write FOnStartCapture ;
   property OnEndCapture :TNotifyEvent  read FOnEndCapture write FOnEndCapture ;
 end;

  TAudioPlay=class
 private
   FLooping:boolean;
   FPlayEvent:TCaptureEvent;
   FOnStartPlay:TNotifyEvent;
   FOnEndPlay:TNotifyEvent;
 public
   procedure Start(const WFormat:TWaveFormatEx);
   procedure Stop();
   property IsCapturing :boolean  read FLooping;
   property OnPlay :TCaptureEvent  read FPlayEvent write FPlayEvent ;
   property OnStartPlay:TNotifyEvent  read FOnStartPlay write FOnStartPlay ;
   property OnEndPlay :TNotifyEvent  read FOnEndPlay write FOnEndPlay ;
 end;

procedure MakePCMFormat(var FormatEx: TWaveFormatEx;SamplesPerSec, BitsPerSample, Channels: Integer);

implementation
uses dialogs,DirectSound;

const
  ID_RIFF = Ord('R') + Ord('I')*$100 + Ord('F')*$10000 + Ord('F')*$1000000;
  ID_WAVE = Ord('W') + Ord('A')*$100 + Ord('V')*$10000 + Ord('E')*$1000000;
  ID_FMT  = Ord('f') + Ord('m')*$100 + Ord('t')*$10000 + Ord(' ')*$1000000;
  ID_FACT = Ord('f') + Ord('a')*$100 + Ord('c')*$10000 + Ord('t')*$1000000;
  ID_DATA = Ord('d') + Ord('a')*$100 + Ord('t')*$10000 + Ord('a')*$1000000;

procedure MakePCMFormat(var FormatEx: TWaveFormatEx;SamplesPerSec, BitsPerSample, Channels: Integer);
begin
  with FormatEx do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := Channels;
    nSamplesPerSec := SamplesPerSec;
    wBitsPerSample := BitsPerSample;
    nBlockAlign := nChannels*(wBitsPerSample div 8);
    nAvgBytesPerSec := nBlockAlign*nSamplesPerSec;
    cbSize := 0;
  end;
end;

{ TLinkStream }

constructor TCustomLinkStream.Create(AStream: TStream);
begin
  if not Assigned(AStream) then
     raise Exception.Create('Invalid stream');
  FStream   := AStream;
  Offset    := FStream.Position;
  Modes     := [smRead,smWrite,smSetSize];
end;

function TCustomLinkStream.GetSize: Int64;
begin
    Result:=FSize;
end;

function TCustomLinkStream.Read(var Buffer; Count: Integer): Longint;
begin
   if not (smRead in FModes) then
      raise Exception.Create('No read');
   Result:= FSize - Position;
   if Count > Result then
      Count :=Result;
   Result:= ReadData(Buffer,Count);
end;

function TCustomLinkStream.Write(const Buffer; Count: Integer): Longint;
begin
    if not (smWrite in FModes) then
      raise Exception.Create('No Write');
    Result:= Position + Count;

     if (FSize < Result) then
     begin
        if not (smSetSize in FModes) then
          raise Exception.Create('No SetSize');
     end;

     Result:= WriteData(Buffer,Count);

     if FStream.Position > FOffset+FSize then
     begin
         FSize:= FStream.Position;
     end;

end;

function TCustomLinkStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result:=FStream.Position;
  case Origin of
    soBeginning:  result := FOffset+Offset;
    soCurrent  :  Inc(result, Offset);
    soEnd      :  result := FSize + Offset;
  end;
  FStream.Position:=result;
  result:=result-FOffset;
end;

procedure TCustomLinkStream.SetSize(const NewSize: Int64);
begin
   if not (smSetSize in FModes) then
      raise Exception.Create('No SetSize');
  // if FStream.Size < (NewSize + FOffset) then
      FStream.Size :=(NewSize + FOffset);
   if NewSize >= 0 then
      FSize := NewSize;
end;

procedure TCustomLinkStream.SetLimitSize(const Value: integer);
begin
  if Value = FSize then
     Exit;
  if Value < 0 then
     raise Exception.Createfmt('Invalid size value %d',[Value]);
  FSize := Value;
end;

procedure TCustomLinkStream.SetOffset(const Value: integer);
begin
  if (Value = FOffset)then 
     Exit;
  if (Value < 0)or (FOffset > FStream.Size) then
      raise Exception.Createfmt('Invalid offset value %d',[Value]);
  FOffset := Value;
end;

function TCustomLinkStream.ReadData(var Buffer; Count: Integer): Integer;
begin
   Result:=FStream.Read(Buffer,Count);
end;

function TCustomLinkStream.WriteData(const Buffer; Count: Integer): Integer;
begin
    Result:=FStream.Write(Buffer,Count);
end;

{ TStreamHeaded }

procedure TStreamHeaded.Close;
begin
   if FOpened then
      InternalClose
end;

procedure TStreamHeaded.InternalClose;
begin
   FOpened  := False;
   FWriteMode:= False;
end;

procedure TStreamHeaded.Open(WriteMode: boolean);
begin
    if WriteMode then
    begin
       FOpened :=OpenCreate;
       if FOpened then
          FWriteMode :=True;
    end
      else
       FOpened :=OpenRead;

end;


{ TPCMWave }

function TPCMWave.OpenCreate: boolean;
var
  Value: Cardinal;
begin
 if FOpened then
    raise Exception.Create('Stream alredy opened');

  Position := 0;
  Value := 0;
  WriteProp(ID_RIFF,Value,SizeOf(Value));
  WriteProp(ID_WAVE,Value,0);
  Value := SizeOf(FFormat)-2;
  WriteProp(ID_FMT,Value,SizeOf(Value));
  WriteBuffer(FFormat,Value);
  FDataHeaderPos := Position;
  WriteProp(ID_DATA,Value,SizeOf(Value));
  FDataPos := Position;
  Result := True;
end;

function TPCMWave.OpenRead: boolean;
var
  Value: Cardinal;
begin
  ReadProp(ID_RIFF,Value,SizeOf(Value));
  ReadProp(ID_WAVE,Value,0);
  ReadProp(ID_FMT,Value,SizeOf(Value));
  ReadBuffer(FFormat,SizeOf(FFormat)-2);
  Seek(Value-(SizeOf(FFormat)-2),1);
  FDataHeaderPos := Position;
  ReadProp(ID_DATA,Value,SizeOf(Value));
  FDataPos := Position;
  FDataSize := Value;
  LimitSize:=FDataPos+FDataSize;
  Result:=True;
end;

procedure TPCMWave.InternalClose;
var
  Value: Cardinal;
begin
  if FWriteMode then
  begin
     Position := 0;
     Value:=Size-8;
     WriteProp(ID_RIFF,Value,SizeOf(Value));
     Position :=FDataHeaderPos;
     Value:=Size-FDataPos;
     WriteProp(ID_DATA,Value,SizeOf(Value));
  end;
  inherited
end;

procedure TPCMWave.ReadProp(Prop: Cardinal; var Buff;
  Size: integer);
var
  D:Cardinal;
begin
  ReadBuffer(D,SizeOf(D));
  if (D <> Prop)then
    raise Exception.Create('Invalid prop');
  ReadBuffer(Buff,Size);
end;

procedure TPCMWave.WriteProp(Prop: Cardinal; const Buff;
  Size: integer);
begin
   WriteBuffer(Prop,SizeOf(Prop));
   WriteBuffer(Buff,Size);
end;

procedure TPCMWave.SetPCMFormat(SamplesPerSec, BitsPerSample,
  Channels: Integer);
begin
   MakePCMFormat(FFormat,SamplesPerSec, BitsPerSample, Channels);
end;

procedure TPCMWave.SetWaveFormat(const Value: TWaveFormatEx);
begin
   with Value do
    SetPCMFormat(nSamplesPerSec,wBitsPerSample,nChannels);
end;

{ TLameMp3Encoder }

function TLameMp3Encoder.WriteData(const Buffer; Count: Integer): Integer;
var
  PData:PByte;
  DLen ,toWrite,Readed:integer;
begin
   if not FOpened then
      raise Exception.Create('Stream not opened');
   PData:=@Buffer;
   DLen :=Count;
   Result:=0;
   repeat
        Readed:= FReadLen;
        if DLen < integer(FReadLen) then
           Readed:= DLen;

         beEncodeChunk(FHandle,Readed div 2, PData^, FBuff[0], Longword(toWrite));
         if inherited WriteData(FBuff[0],toWrite) <> toWrite then
            Exit;
         dec(DLen,Readed);
         inc(PData,Readed);
    until Readed < integer(FReadLen);
    Result := Count;
end;

procedure TLameMp3Encoder.InternalClose;
begin
  // beDeInitStream(FHandle, pMP3Buffer^, dwWrite );
   beCloseStream(FHandle);
   FHandle:=0;
   FReadLen:=0;
   FWriteLen:=0;
   SetLength(FBuff,0);
   inherited;
end;

function TLameMp3Encoder.OpenCreate: boolean;
begin
  FReadLen:=0;
  FWriteLen:=0;
  Result:= beInitStream(FFormat, FReadLen, FWriteLen,FHandle)=BE_ERR_SUCCESSFUL;
  SetLength(FBuff,FWriteLen);
end;

function TLameMp3Encoder.OpenRead: boolean;
begin
   Result :=False;
end;

procedure TLameMp3Encoder.SetFormat(SamplesPerSec, Bitrate:integer;
  Stereo: boolean);
const CMODE:array[boolean]of integer=(BE_MP3_MODE_MONO,BE_MP3_MODE_STEREO);
begin
   Fillchar(FFormat,sizeof(FFormat),0);
   FFormat.dwConfig := BE_CONFIG_LAME;
    with  FFormat.Format.lhv1 do
   begin
         dwStructVersion := 1;
         dwStructSize := SizeOf(TBE_Config);
         dwSampleRate := SamplesPerSec;
         nMode := CMODE[Stereo];
         dwBitrate := Bitrate;
         dwMaxBitrate := Bitrate;
         dwMPegVersion := 1; //MPEG1
    end;
end;

{ TFileMp3 }

constructor TFileMp3.Create(const Filename: string);
begin
   FStream:=TFilestream.Create(Filename,fmCreate);
   inherited Create(FStream);
   FFilename:=Filename;
end;

destructor TFileMp3.Destroy;
begin
   Close;
   FStream.Free;
   inherited;
end;

{ TFileWave }

constructor TFileWave.Create(const Filename: string;AFileMode:Word;ALimitSize:integer);
begin
  FStream:=TFilestream.Create(Filename,AFileMode);
  inherited Create(FStream);
   //  ((AFileMode and fmOpenWrite)=0)and
  if ALimitSize=0  then
  begin
    LimitSize:=FStream.Size;
  end else begin
     LimitSize := AFileMode;
     Modes :=[smRead];
  end;
end;

destructor TFileWave.Destroy;
begin
   Close;
   FStream.Free;
   inherited;
end;

{ TAudioCapture }

procedure TAudioCapture.Start(const WFormat: TWaveFormatEx);
var
    OutDS:IDirectSoundCapture;
    OutBuffer:IDirectSoundCaptureBuffer;
    DCP:TDSCCaps;
    dscbd:TDSCBUFFERDESC;
    FBufferPos,Size,I:integer;
    Data1, Data2: Pointer;
    Data1Size, Data2Size: DWORD;
    FHandle:THandle;
    DSNTF:IDirectSoundNotify;
    NTF:array[0..1]of TDSBPositionNotify;
 begin
 if not Assigned(FCaptureEvent) then
   Exit;
 DirectSoundCaptureCreate(nil,OutDS,nil);
 DCP.dwSize:=Sizeof(DCP);
 FillChar(dscbd,sizeof(TDSCBUFFERDESC),0);
 dscbd.dwSize := sizeof(TDSCBUFFERDESC);
 dscbd.dwFlags := 0;
 dscbd.dwBufferBytes := WFormat.nAvgBytesPerSec div 2;
 dscbd.lpwfxFormat := @WFormat;
 OutDS._AddRef;

 if OutDS.CreateCaptureBuffer(dscbd,OutBuffer,nil)=S_OK then
 begin
      if Assigned(FOnStartCapture) then
         FOnStartCapture(Self);
      FHandle := CreateEvent(nil, False, False, nil);
      NTF[0].dwOffset:= (dscbd.dwBufferBytes div 2) -1;
      NTF[0].hEventNotify:=FHandle;
      NTF[1].dwOffset:=dscbd.dwBufferBytes - 1;
      NTF[1].hEventNotify:=FHandle;
      DSNTF:=OutBuffer as IDirectSoundNotify;
      DSNTF.SetNotificationPositions(2,@NTF);
      OutBuffer.Start(DSCBSTART_LOOPING);
      FLooping  :=True;
      I:= 0;
      Size:=(dscbd.dwBufferBytes div 2);
      repeat
          FBufferPos := 0;
          if (I and 1)=1then
             FBufferPos :=Size;
          WaitForSingleObject(FHandle, INFINITE);
          if OutBuffer.Lock(FBufferPos,Size ,Data1 , Data1Size, Data2, Data2Size, 0)=DS_OK then
          begin
              FCaptureEvent(Data1, Data1Size, Data2, Data2Size);
              OutBuffer.UnLock(Data1, Data1Size, Data2, Data2Size);
          end;

      //  Application.ProcessMessages;
        if not FLooping then
        begin
           OutBuffer.Stop;
           if Assigned(FOnEndCapture) then
              FOnEndCapture(Self);
           break;
        end;
        inc(I);
        //Sleep(40);
      until False;
      SetEvent(FHandle);
      CloseHandle(FHandle);
   end;
end;

procedure TAudioCapture.Stop;
begin
  FLooping:=False;
end;

{ TAudioPlay }

procedure TAudioPlay.Start(const WFormat: TWaveFormatEx);
var
    OutDS:IDirectSound;
    OutBuffer:IDirectSoundBuffer;
    dscbd:TDSBufferDesc;
    FBufferPos,Size,I:integer;
    Data1, Data2: Pointer;
    Data1Size, Data2Size: DWORD;
    FHandle:THandle;
    DSNTF:IDirectSoundNotify;
    NTF:array[0..1]of TDSBPositionNotify;
 begin
 if not Assigned(FPlayEvent) then
   Exit;
 DirectSoundCreate(nil,OutDS,nil);
 FillChar(dscbd,sizeof(TDSBufferDesc),0);
 dscbd.dwSize := sizeof(TDSBufferDesc);
 dscbd.dwFlags :=(DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLPOSITIONNOTIFY);;
 dscbd.dwBufferBytes := WFormat.nAvgBytesPerSec;
 dscbd.lpwfxFormat := @WFormat;
 OutDS._AddRef;
 {case  OutDS.CreateSoundBuffer(dscbd, OutBuffer, nil) of
  DS_OK: ;
  DSERR_BADFORMAT: ShowMessage('DSERR_BADFORMAT');
  DSERR_INVALIDPARAM: ShowMessage('DSERR_INVALIDPARAM');
  end;
 }
 OutDS.SetCooperativeLevel(GetDesktopWindow, DSSCL_PRIORITY);
 if OutDS.CreateSoundBuffer(dscbd,OutBuffer,nil)=DS_OK then
 begin
      if Assigned(FOnStartPlay) then
         FOnStartPlay(Self);
      FHandle := CreateEvent(nil, False, False, nil);
      NTF[0].dwOffset:=0;
      NTF[0].hEventNotify:=FHandle;
      NTF[1].dwOffset:= (dscbd.dwBufferBytes div 2) -1;
      NTF[1].hEventNotify:=FHandle;



      DSNTF:=OutBuffer as IDirectSoundNotify;
      DSNTF.SetNotificationPositions(2,@NTF);

      FLooping  :=True;
      I:= 0;
      Size:=(dscbd.dwBufferBytes div 2);
      repeat
          FBufferPos := 0;
          if (I and 1)= 1 then
             FBufferPos :=Size;


//          OutBuffer.SetCurrentPosition(0);

          if OutBuffer.Lock(FBufferPos,Size ,Data1 , Data1Size, Data2, Data2Size, DSBLOCK_FROMWRITECURSOR)=DS_OK then
          begin
              FPlayEvent(Data1, Data1Size, Data2, Data2Size);
              OutBuffer.UnLock(Data1, Data1Size, Data2, Data2Size);
          end;


          OutBuffer.Play(0,0,0);
          WaitForSingleObject(FHandle, INFINITE);
        if not FLooping then
        begin
           OutBuffer.Stop;
           if Assigned(FOnEndPlay) then
              FOnEndPlay(Self);
           break;
        end;
        inc(I);
      until False;
      SetEvent(FHandle);
      CloseHandle(FHandle);
   end;

end;

procedure TAudioPlay.Stop;
begin
   FLooping:=False;
end;

end.
 

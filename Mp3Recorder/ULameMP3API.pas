unit ULameMP3API;

interface
 uses Windows;
const

  BE_CONFIG_MP3 = 0;
  BE_CONFIG_LAME = 256;

  BE_ERR_SUCCESSFUL: LongWord = 0;
  BE_ERR_INVALID_FORMAT: LongWord = 1;
  BE_ERR_INVALID_FORMAT_PARAMETERS: LongWord = 2;
  BE_ERR_NO_MORE_HANDLES: LongWord = 3;
  BE_ERR_INVALID_HANDLE: LongWord = 4;

  BE_MAX_HOMEPAGE = 256;

  BE_MP3_MODE_STEREO = 0;
  BE_MP3_MODE_DUALCHANNEL = 2;
  BE_MP3_MODE_MONO = 3;

type
  THBE_STREAM = LongWord;
  PHBE_STREAM = ^PHBE_STREAM;
  BE_ERR = LongWord;

  TMP3 = packed record
    dwSampleRate: LongWord;
    byMode: Byte;
    wBitRate: Word;
    bPrivate: LongWord;
    bCRC: LongWord;
    bCopyright: LongWord;
    bOriginal: LongWord;
  end;

  TLHV1 = packed record
    // STRUCTURE INFORMATION
    dwStructVersion: DWORD;
    dwStructSize: DWORD;
    // BASIC ENCODER SETTINGS
    dwSampleRate: DWORD; // ALLOWED SAMPLERATE VALUES DEPENDS ON dwMPEGVersion
    dwReSampleRate: DWORD; // DOWNSAMPLERATE, 0=ENCODER DECIDES
    nMode: Integer; // BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
    dwBitrate: DWORD; // CBR bitrate, VBR min bitrate
    dwMaxBitrate: DWORD; // CBR ignored, VBR Max bitrate
    nQuality: Integer; // Quality setting (NORMAL,HIGH,LOW,VOICE)
    dwMpegVersion: DWORD; // MPEG-1 OR MPEG-2
    dwPsyModel: DWORD; // FUTURE USE, SET TO 0
    dwEmphasis: DWORD; // FUTURE USE, SET TO 0
    bPrivate: LONGBOOL; // Set Private Bit (TRUE/FALSE)
    bCRC: LONGBOOL; // Insert CRC (TRUE/FALSE)
    bCopyright: LONGBOOL; // Set Copyright Bit (TRUE/FALSE)
    bOriginal: LONGBOOL; // Set Original Bit (TRUE/FALSE_
    bWriteVBRHeader: LONGBOOL; // WRITE XING VBR HEADER (TRUE/FALSE)
    bEnableVBR: LONGBOOL; // USE VBR ENCODING (TRUE/FALSE)
    nVBRQuality: Integer; // VBR QUALITY 0..9
    btReserved: array[ 0..255 ] of Byte; // FUTURE USE, SET TO 0
  end;

  TAAC = packed record
    dwSampleRate: LongWord;
    byMode: Byte;
    wBitRate: Word;
    byEncodingMethod: Byte;
  end;

  TFormat = packed record
    case byte of
      1: ( mp3: TMP3 );
      2: ( lhv1: TLHV1 );
      3: ( aac: TAAC );
  end;

  TBE_Config = packed record
    dwConfig: LongWord;
    format: TFormat;
  end;

  PBE_Config = ^TBE_Config;

  TBE_Version = record
    byDLLMajorVersion: Byte;
    byDLLMinorVersion: Byte;
    byMajorVersion: Byte;
    byMinorVersion: Byte;
    byDay: Byte;
    byMonth: Byte;
    wYear: Word;
    zHomePage: array[0..BE_MAX_HOMEPAGE + 1] of Char;
  end;

  PBE_Version = ^TBE_Version;

//function beWriteVBRHeader(lpszFileN): BE_Err; cdecl; external 'Lame_enc.dll';
function beInitStream( var pbeConfig: TBE_CONFIG; var dwSample: LongWord; var dwBufferSize: LongWord; var phbeStream: THBE_STREAM ): BE_Err; cdecl; external 'Lame_enc.dll';
function beEncodeChunk( hbeStream: THBE_STREAM; nSamples: LongWord; var pSample; var pOutput; var pdwOutput: LongWord ): BE_Err; cdecl; external 'Lame_enc.dll';
function beDeinitStream( hbeStream: THBE_STREAM; var pOutput; var pdwOutput: LongWord ): BE_Err; cdecl; external 'Lame_enc.dll';
function beCloseStream( hbeStream: THBE_STREAM ): BE_Err; cdecl; external 'Lame_enc.dll';
procedure beVersion( var pbeVersion: TBE_VERSION ); cdecl; external 'Lame_enc.dll';



implementation


end.


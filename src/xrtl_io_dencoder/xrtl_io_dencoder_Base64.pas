unit xrtl_io_dencoder_Base64;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLCustomBase64Dencoder = class(TXRTLDencoder)
  private
    procedure  Encode(const InData: PByteArray; const InDataAvail: Byte; var OutData: PByteArray);
    function   Decode(const InData: PByteArray; var OutData: PByteArray): Integer;
  protected
    FDigits: string;
    function   engineUpdateEncode(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  var OutBuffer: PByteArray;
                                  var OutAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateDecode(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  var OutBuffer: PByteArray;
                                  var OutAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineGetDecodedTextBlockSize: Integer; override;
    function   engineGetEncodedTextBlockSize: Integer; override;
  public
  end;

  TXRTLBase64Dencoder = class(TXRTLCustomBase64Dencoder)
  public
    constructor Create;
  end;

  TXRTLURLSafeBase64Dencoder = class(TXRTLCustomBase64Dencoder)
  public
    constructor Create;
  end;

const
  XRTLBase64Digits         = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  XRTLURLSafeBase64Digits  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
  XRTLBase64Padding        = '=';

implementation

uses
  Windows,
  xrtl_util_CPUUtils;

{ TXRTLCustomBase64Dencoder }

function TXRTLCustomBase64Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 2) and (OutAvail > 3) do
  begin
    Encode(InBuffer, 3, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, 3);
    Dec(InAvail, 3);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 4);
    Dec(OutAvail, 4);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
  begin
    if OutAvail < 4 then
      raise EXRTLDencoderException.CreateFmt('Invalid output buffer length in %s.engineUpdateEncode: %d', [ClassName, OutAvail]);
    Encode(InBuffer, InAvail, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, InAvail);
    Dec(InAvail, InAvail);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 4);
    Dec(OutAvail, 4);
  end;
  Result:= True;
end;

function TXRTLCustomBase64Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  DataCount: Integer;
begin
  while (InAvail > 3) and (OutAvail > 2) do
  begin
    DataCount:= Decode(InBuffer, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, 4);
    Dec(InAvail, 4);
    if DataCount < 0 then
      raise EXRTLDencoderException.CreateFmt('Invalid padding in %s.engineUpdateDecode: %d', [ClassName, OutAvail]);
    OutBuffer:= XRTLPointerAdd(OutBuffer, DataCount);
    Dec(OutAvail, DataCount);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateDecode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLCustomBase64Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 3;
end;

function TXRTLCustomBase64Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 4;
end;

procedure TXRTLCustomBase64Dencoder.Encode(const InData: PByteArray; const InDataAvail: Byte;
  var OutData: PByteArray);
const
  PadCount: array[0 .. 2] of Integer = (0, 2, 1);
var
  I: Integer;
  LInData: Cardinal;
begin
  LInData:= XRTLSwapHiLo32(PCardinal(InData)^);
  OutData[0]:= Ord(FDigits[(LInData and $FC000000) shr 24]);
  OutData[1]:= Ord(FDigits[(LInData and $03F00000) shr 20]);
  OutData[2]:= Ord(FDigits[(LInData and $000FC000) shr 12]);
  OutData[3]:= Ord(FDigits[(LInData and $00003F00) shr  8]);
  for I:= 3 downto 4 - PadCount[InDataAvail] do
  begin
    OutData[I]:= Ord(XRTLBase64Padding);
  end;
end;

function TXRTLCustomBase64Dencoder.Decode(const InData: PByteArray; var OutData: PByteArray): Integer;
const
  DataCount: array[0 .. 3] of Integer = (3, 2, 1, -1);
var
  LIndex: Integer;
  LData: Cardinal;
  I, PadCount: Integer;
begin
  ZeroMemory(@LIndex, SizeOf(LIndex));
  PadCount:= 0;
  LData:= 0;
  for I:= 0 to 3 do
  begin
    if Char(InData[I]) = XRTLBase64Padding then
    begin
      LIndex:= 0;
      Inc(PadCount);
    end
    else
    begin
      LIndex:= Pos(Char(InData[I]), FDigits) - 1;
      if LIndex < 0 then
        raise EXRTLDencoderException.CreateFmt('Invalid character in %s.engineUpdateDecode: %c', [ClassName, Chr(InData[I])])
    end;
    LData:= (LData shl 6) or Cardinal(LIndex);
  end;
  Result:= DataCount[PadCount];
  OutData[0]:= (LData and $00FF0000) shr 16;
  OutData[1]:= (LData and $0000FF00) shr  8;
  OutData[2]:= (LData and $000000FF);
end;

{ TXRTLBase64Dencoder }

constructor TXRTLBase64Dencoder.Create;
begin
  inherited;
  FDigits:= XRTLBase64Digits;
end;

{ TXRTLURLSafeBase64Dencoder }

constructor TXRTLURLSafeBase64Dencoder.Create;
begin
  inherited;
  FDigits:= XRTLURLSafeBase64Digits;
end;

end.

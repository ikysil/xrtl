unit xrtl_io_dencoder_Base32;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLBase32Dencoder = class(TXRTLDencoder)
  private
    procedure  Encode(const InData: PByteArray; const InDataAvail: Byte; var OutData: PByteArray);
    function   Decode(const InData: PByteArray; var OutData: PByteArray): Integer;
  protected
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

const
  XRTLBase32Digits  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
  XRTLBase32Padding = '=';

implementation

uses
  Windows,
  xrtl_util_CPUUtils;

{ TXRTLBase32Dencoder }

function TXRTLBase32Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 4) and (OutAvail > 7) do
  begin
    Encode(InBuffer, 5, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, 5);
    Dec(InAvail, 5);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 8);
    Dec(OutAvail, 8);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
  begin
    if OutAvail < 8 then
      raise EXRTLDencoderException.CreateFmt('Invalid output buffer length in %s.engineUpdateEncode: %d', [ClassName, OutAvail]);
    Encode(InBuffer, InAvail, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, InAvail);
    Dec(InAvail, InAvail);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 8);
    Dec(OutAvail, 8);
  end;
  Result:= True;
end;

function TXRTLBase32Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  DataCount: Integer;
begin
  while (InAvail > 7) and (OutAvail > 4) do
  begin
    DataCount:= Decode(InBuffer, OutBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, 8);
    Dec(InAvail, 8);
    if DataCount < 0 then
      raise EXRTLDencoderException.CreateFmt('Invalid padding in %s.engineUpdateDecode: %d', [ClassName, OutAvail]);
    OutBuffer:= XRTLPointerAdd(OutBuffer, DataCount);
    Dec(OutAvail, DataCount);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateDecode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLBase32Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 5;
end;

function TXRTLBase32Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 8;
end;

procedure TXRTLBase32Dencoder.Encode(const InData: PByteArray; const InDataAvail: Byte;
  var OutData: PByteArray);
const
  PadCount: array[0 .. 4] of Integer = (0, 6, 4, 3, 1);
var
  I: Integer;
begin
  OutData[0]:= Ord(XRTLBase32Digits[(InData[0] and $F8) shr 3]);
  OutData[1]:= Ord(XRTLBase32Digits[((InData[0] and $07) shl 2) or ((InData[1] and $C0) shr 6)]);
  OutData[2]:= Ord(XRTLBase32Digits[(InData[1] and $3E) shr 1]);
  OutData[3]:= Ord(XRTLBase32Digits[((InData[1] and $01) shl 4) or ((InData[2] and $F0) shr 4)]);
  OutData[4]:= Ord(XRTLBase32Digits[((InData[2] and $0F) shl 1) or ((InData[3] and $80) shr 7)]);
  OutData[5]:= Ord(XRTLBase32Digits[(InData[3] and $7C) shr 2]);
  OutData[6]:= Ord(XRTLBase32Digits[((InData[3] and $03) shl 3) or ((InData[4] and $E0) shr 5)]);
  OutData[7]:= Ord(XRTLBase32Digits[InData[4] and $1F]);
  for I:= 7 downto 8 - PadCount[InDataAvail] do
  begin
    OutData[I]:= Ord(XRTLBase32Padding);
  end;
end;

function TXRTLBase32Dencoder.Decode(const InData: PByteArray; var OutData: PByteArray): Integer;
const
  DataCount: array[0 .. 7] of Integer = (8, 4, -1, 3, 2, -1, 1, -1);
var
  LIndex: array[0 .. 7] of Integer;
  LData: array[0 .. 3] of Word;
  I, PadCount: Integer;
begin
  ZeroMemory(@LIndex, SizeOf(LIndex));
  ZeroMemory(@LData, SizeOf(LData));
  PadCount:= 0;
  for I:= 0 to 7 do
  begin
    if Char(InData[I]) = XRTLBase32Padding then
    begin
      LIndex[I]:= 0;
      Inc(PadCount);
    end
    else
    begin
      LIndex[I]:= Pos(Char(InData[I]), XRTLBase32Digits) - 1;
      if LIndex[I] < 0 then
        raise EXRTLDencoderException.CreateFmt('Invalid character in %s.engineUpdateDecode: %c', [ClassName, Chr(InData[I])])
    end;
  end;
  for I:= 0 to 3 do
  begin
    LData[I]:= (LIndex[I * 2] shl 5) or (LIndex[I * 2 + 1]);
  end;
  Result:= DataCount[PadCount];
  OutData[0]:= LData[0] and $03FC;
  OutData[1]:= ((LData[0] and $0003) shl 6) or ((LData[1] and $03F0) shr 4);
  OutData[2]:= ((LData[1] and $000F) shl 4) or ((LData[2] and $03C0) shr 6);
  OutData[3]:= ((LData[2] and $003F) shl 2) or ((LData[3] and $0300) shr 8);
  OutData[4]:= LData[3] and $00FF;
end;

end.

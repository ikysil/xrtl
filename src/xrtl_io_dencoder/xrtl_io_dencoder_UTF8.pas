unit xrtl_io_dencoder_UTF8;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLUTF8Dencoder = class(TXRTLDencoder)
  private
    function   GetEncodeLength(const C: Cardinal): Byte;
    function   GetDecodeLength(const B: Byte): Byte;
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

implementation

uses
  xrtl_util_CPUUtils;

const
  XRTLUTF8ContMask = $80;

  XRTLUTF8OrMask:  array[1 .. 6] of Byte = ($00, $C0, $E0, $F0, $F8, $FC);
  XRTLUTF8AndMask: array[1 .. 6] of Byte = ($7F, $1F, $0F, $07, $03, $01);
  XRTLUTF8BitLen:  array[1 .. 6] of Byte = (7, 5, 4, 3, 2, 1);

{ TXRTLUTF8Dencoder }

function TXRTLUTF8Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  I, Len: Byte;
  C: Cardinal;
begin
  while (InAvail > 3) do
  begin
    C:= PCardinal(InBuffer)^;
    Len:= GetEncodeLength(C);
    if OutAvail < Len then
      Break;
    for I:= Len downto 2 do
    begin
      OutBuffer[I - 1]:= C and $3F or XRTLUTF8ContMask;
      C:= C shr 6;
    end;
    OutBuffer[0]:= C and XRTLUTF8AndMask[Len] or XRTLUTF8OrMask[Len];
    InBuffer:= XRTLPointerAdd(InBuffer, 4);
    Dec(InAvail, 4);
    OutBuffer:= XRTLPointerAdd(OutBuffer, Len);
    Dec(OutAvail, Len);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid output buffer length in %s.engineUpdateEncode: %d', [ClassName, OutAvail]);
  Result:= True;
end;

function TXRTLUTF8Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;

  procedure CheckLength(C: Cardinal; Len: Byte);
  var
    I: Byte;
    S: string;
  begin
    if Len <> GetEncodeLength(C) then
    begin
      S:= '';
      for I:= Len downto 1 do
      begin
        S:= Format('%.8x %s', [InBuffer[I - 1], S]);
      end;
      raise EXRTLDencoderException.CreateFmt('Invalid sequence %s in %s.engineUpdateDecode', [S, ClassName]);
    end;
  end;

var
  I, Len: Byte;
  C: Cardinal;
begin
  while (OutAvail > 3) and (InAvail > 0) do
  begin
    Len:= GetDecodeLength(InBuffer[0]);
    if InAvail < Len then
      Break;
    C:= 0;
    for I:= Len downto 2 do
    begin
      if InBuffer[I - 1] and not XRTLUTF8ContMask >= $40 then
        raise EXRTLDencoderException.CreateFmt('Invalid input byte 0x%.8x in %s.engineUpdateDecode', [InBuffer[I - 1], ClassName]);
      C:= (C shl 6) or (InBuffer[I - 1] and $3F);
    end;
    C:= (C shl XRTLUTF8BitLen[Len]) or (InBuffer[0] and XRTLUTF8AndMask[Len]);
    CheckLength(C, Len);
    PCardinal(OutBuffer)^:= C;
    InBuffer:= XRTLPointerAdd(InBuffer, Len);
    Dec(InAvail, Len);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 4);
    Dec(OutAvail, 4);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid output buffer length in %s.engineUpdateDecode: %d', [ClassName, OutAvail]);
  Result:= True;
end;

function TXRTLUTF8Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 4;
end;

function TXRTLUTF8Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 6;
end;

function TXRTLUTF8Dencoder.GetEncodeLength(const C: Cardinal): Byte;
const
  CharLimits: array[1 .. 6] of Int64 =
    ($00000080, $00000800, $00001000, $00200000, $04000000, $80000000);
begin
  if C >= $80000000 then
    raise EXRTLDencoderException.CreateFmt('Can''t encode 0x%.8x in %s.engineUpdateEncode', [C, ClassName]);
  for Result:= 1 to 6 do
  begin
    if C < CharLimits[Result] then
      Break;
  end;
end;

function TXRTLUTF8Dencoder.GetDecodeLength(const B: Byte): Byte;
begin
  for Result:= 1 to 6 do
  begin
    if B and not XRTLUTF8AndMask[Result] = XRTLUTF8OrMask[Result] then
      Exit;
  end;
  raise EXRTLDencoderException.CreateFmt('Invalid input byte 0x%.8x in %s.engineUpdateDecode', [B, ClassName]);
end;

end.

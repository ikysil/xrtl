unit xrtl_io_dencoder_Base16;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLBase16Dencoder = class(TXRTLDencoder)
  private
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
  XRTLBase16Digits = '0123456789ABCDEF';

implementation

uses
  xrtl_util_CPUUtils;

{ TXRTLBase16Dencoder }

function TXRTLBase16Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  Value: Byte;
begin
  while (InAvail > 0) and (OutAvail > 1) do
  begin
    Value:= InBuffer[0];
    InBuffer:= XRTLPointerAdd(InBuffer, 1);
    Dec(InAvail);
    OutBuffer[0]:= Ord(XRTLBase16Digits[Value shr 4]);
    OutBuffer[1]:= Ord(XRTLBase16Digits[Value and $0F]);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 2);
    Dec(OutAvail, 2);
  end;
  Result:= True;
end;

function TXRTLBase16Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;

  function Digit(Value: Byte): Byte;
  var
    Index: Integer;
  begin
    Index:= Pos(Char(Value), XRTLBase16Digits) - 1;
    if Index < 0 then
{
    if Value in [Ord('0') .. Ord('9')] then
      Result:= Value - Ord('0')
    else
      if Value in [Ord('A') .. Ord('F')] then
        Result:= Value - Ord('A') + $0A
      else
}
        raise EXRTLDencoderException.CreateFmt('Invalid character in %s.engineUpdateDecode: %c', [ClassName, Chr(Value)])
    else
      Result:= Index;
  end;

begin
  while (InAvail > 1) and (OutAvail > 0) do
  begin
    OutBuffer[0]:= (Digit(InBuffer[0]) shl 4) or Digit(InBuffer[1]);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 1);
    Dec(OutAvail);
    InBuffer:= XRTLPointerAdd(InBuffer, 2);
    Dec(InAvail, 2);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateDecode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLBase16Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 1;
end;

function TXRTLBase16Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 2;
end;

end.

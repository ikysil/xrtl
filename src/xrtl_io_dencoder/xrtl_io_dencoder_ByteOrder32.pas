unit xrtl_io_dencoder_ByteOrder32;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Compat,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLByteOrder32Dencoder = class(TXRTLDencoder)
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

implementation

uses
  xrtl_util_CPUUtils;

{ TXRTLByteOrder32Dencoder }

function TXRTLByteOrder32Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 3) and (OutAvail > 3) do
  begin
    PCardinal(OutBuffer)^:= XRTLSwapHiLo32(PCardinal(InBuffer)^);
    InBuffer:= XRTLPointerAdd(InBuffer, 4);
    Dec(InAvail, 4);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 4);
    Dec(OutAvail, 4);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateEncode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLByteOrder32Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 3) and (OutAvail > 3) do
  begin
    PCardinal(OutBuffer)^:= XRTLSwapHiLo32(PCardinal(InBuffer)^);
    InBuffer:= XRTLPointerAdd(InBuffer, 4);
    Dec(InAvail, 4);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 4);
    Dec(OutAvail, 4);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateDecode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLByteOrder32Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 4;
end;

function TXRTLByteOrder32Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 4;
end;

end.


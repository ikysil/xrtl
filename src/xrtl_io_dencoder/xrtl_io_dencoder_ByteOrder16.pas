unit xrtl_io_dencoder_ByteOrder16;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_io_dencoder_Dencoder;

type
  TXRTLByteOrder16Dencoder = class(TXRTLDencoder)
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
  Windows,
  xrtl_util_CPUUtils;

{ TXRTLByteOrder16Dencoder }

function TXRTLByteOrder16Dencoder.engineUpdateEncode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 1) and (OutAvail > 1) do
  begin
    PWord(OutBuffer)^:= XRTLSwapHiLo16(PWord(InBuffer)^);
    InBuffer:= XRTLPointerAdd(InBuffer, 2);
    Dec(InAvail, 2);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 2);
    Dec(OutAvail, 2);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateEncode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLByteOrder16Dencoder.engineUpdateDecode(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail > 1) and (OutAvail > 1) do
  begin
    PWord(OutBuffer)^:= XRTLSwapHiLo16(PWord(InBuffer)^);
    InBuffer:= XRTLPointerAdd(InBuffer, 2);
    Dec(InAvail, 2);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 2);
    Dec(OutAvail, 2);
  end;
  if (InAvail > 0) and (Operation = spoFinish) then
    raise EXRTLDencoderException.CreateFmt('Invalid stream length in %s.engineUpdateDecode: %d', [ClassName, InAvail]);
  Result:= True;
end;

function TXRTLByteOrder16Dencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= 2;
end;

function TXRTLByteOrder16Dencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= 2;
end;

end.

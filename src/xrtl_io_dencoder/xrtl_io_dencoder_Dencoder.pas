unit xrtl_io_dencoder_Dencoder;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_io_StreamProcessor;

type
  EXRTLDencoderException = class(EXRTLException);

  TXRTLDencoder = class(TXRTLConvertingStreamProcessor)
  protected
    procedure  engineInitEncoder; virtual; abstract;
    procedure  engineInitDecoder; virtual; abstract;
    function   engineUpdate(var InBuffer: PByteArray;
                            var InAvail: Integer;
                            var OutBuffer: PByteArray;
                            var OutAvail: Integer;
                            const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateEncode(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  var OutBuffer: PByteArray;
                                  var OutAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    function   engineUpdateDecode(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  var OutBuffer: PByteArray;
                                  var OutAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
// size of dencoder block in BYTES
    function   engineGetBlockSize: Integer; virtual;
// size of decoded text block in BYTES
// decoded text block = encoded text block = dencoder block size by default
    function   engineGetDecodedTextBlockSize: Integer; virtual;
// size of encoded text block in BYTES
// decoded text block = encoded text block = dencoder block size by default
    function   engineGetEncodedTextBlockSize: Integer; virtual;
  public
    procedure  InitEncoder;
    procedure  InitDecoder;
    function   GetInputBlockSize: Integer;
    function   GetOutputBlockSize: Integer;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetBlockSize: Integer;
    function   GetDecodedTextBlockSize: Integer;
    function   GetEncodedTextBlockSize: Integer;
  end;

implementation

{ TXRTLDencoder }

procedure TXRTLDencoder.InitEncoder;
begin
  SetState(spsUnknown);
  engineInitEncoder;
  SetMode(cspmDirect);
end;

procedure TXRTLDencoder.InitDecoder;
begin
  SetState(spsUnknown);
  engineInitDecoder;
  SetMode(cspmDirect);
end;

function TXRTLDencoder.engineUpdate(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  case Mode of
    cspmDirect:  Result:= engineUpdateEncode(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
    cspmReverse: Result:= engineUpdateDecode(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
  else
    raise EXRTLInvalidStreamProcessorState.CreateFmt('Invalid %s mode', [ClassName]);
  end;
  if Operation = spoFinish then
    SetState(spsClosed);
end;

function TXRTLDencoder.engineGetBlockSize: Integer;
begin
  Result:= 1;
end;

function TXRTLDencoder.engineGetDecodedTextBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

function TXRTLDencoder.engineGetEncodedTextBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

function TXRTLDencoder.GetInputBlockSize: Integer;
begin
  CheckState;
  Result:= -1;
  case Mode of
    cspmDirect:  Result:= engineGetDecodedTextBlockSize;
    cspmReverse: Result:= engineGetEncodedTextBlockSize;
  end;
end;

function TXRTLDencoder.GetOutputBlockSize: Integer;
begin
  CheckState;
  Result:= -1;
  case Mode of
    cspmDirect:  Result:= engineGetEncodedTextBlockSize;
    cspmReverse: Result:= engineGetDecodedTextBlockSize;
  end;
end;

function TXRTLDencoder.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
var
  LInputBlockSize: Integer;
  LOutputBlockSize: Integer;
begin
  LInputBlockSize:= GetInputBlockSize;
  LOutputBlockSize:= GetOutputBlockSize;
  Result:= OutputSize * LInputBlockSize div LOutputBlockSize;
  if (OutputSize mod LOutputBlockSize) <> 0 then
    Inc(Result, LInputBlockSize);
end;

function TXRTLDencoder.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
var
  LInputBlockSize: Integer;
  LOutputBlockSize: Integer;
begin
  LInputBlockSize:= GetInputBlockSize;
  LOutputBlockSize:= GetOutputBlockSize;
  Result:= InputSize * LOutputBlockSize div LInputBlockSize;
  if (InputSize mod LInputBlockSize) <> 0 then
    Inc(Result, LOutputBlockSize);
end;

function TXRTLDencoder.GetBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

function TXRTLDencoder.GetDecodedTextBlockSize: Integer;
begin
  Result:= engineGetDecodedTextBlockSize;
end;

function TXRTLDencoder.GetEncodedTextBlockSize: Integer;
begin
  Result:= engineGetEncodedTextBlockSize;
end;

end.

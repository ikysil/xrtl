unit xrtl_io_StreamProcessor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception;

type
  EXRTLStreamProcessorException = class(EXRTLException);

  EXRTLInvalidStreamProcessorState = class(EXRTLStreamProcessorException);

  TXRTLStreamProcessorOperation = (spoRun, spoFlush, spoFinish);
  TXRTLStreamProcessorState     = (spsUnknown, spsClosed, spsRunning);

  TXRTLStreamProcessor = class
  private
    FState: TXRTLStreamProcessorState;
  protected
    procedure  SetState(AState: TXRTLStreamProcessorState);
    procedure  engineReset; virtual;
    function   engineUpdate(var InBuffer: PByteArray;
                            var InAvail: Integer;
                            var OutBuffer: PByteArray;
                            var OutAvail: Integer;
                            const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
  public
    constructor Create;
    property   State: TXRTLStreamProcessorState read FState;
    procedure  CheckState; virtual;
//  Returns the length in bytes that an output buffer would need to be in order
//  to hold the result of the next update operation, given the input length (in bytes).
//  This call takes into account any unprocessed (buffered) data from
//  a previous update call, and padding.
//  The actual output length of the next update call may be smaller
//  than the length returned by this method.
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; virtual; abstract;
//  Returns the length in bytes that an input buffer would need to be in order
//  to provide OutputSize bytes on next update operation, given the output length (in bytes).
//  This call takes into account any unprocessed (buffered) data from
//  a previous update call, and padding.
//  The actual input length of the next update call may be smaller
//  than the length returned by this method.
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; virtual; abstract;
//  Returns: true if processor has more data to transfer, false if all data processed
    function   Update(var InBuffer: PByteArray;
                      var InAvail: Integer;
                      var OutBuffer: PByteArray;
                      var OutAvail: Integer;
                      const Operation: TXRTLStreamProcessorOperation): Boolean;
    function   IsFlushSupported: Boolean; virtual; abstract;
    procedure  Reset;
    class function GetDisplayName: string; virtual;
  end;

  TXRTLConvertingStreamProcessorMode = (cspmUnknown, cspmDirect, cspmReverse);

  TXRTLConvertingStreamProcessor = class(TXRTLStreamProcessor)
  private
    FMode: TXRTLConvertingStreamProcessorMode;
  protected
    procedure  SetMode(AMode: TXRTLConvertingStreamProcessorMode);
  public
    constructor Create;
    property   Mode: TXRTLConvertingStreamProcessorMode read FMode;
    procedure  CheckState; override;
  end;

  TXRTLCompressingStreamProcessor = class(TXRTLConvertingStreamProcessor)
  private
  protected
    function   engineUpdate(var InBuffer: PByteArray;
                            var InAvail: Integer;
                            var OutBuffer: PByteArray;
                            var OutAvail: Integer;
                            const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateCompress(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    function   engineUpdateDecompress(var InBuffer: PByteArray;
                                      var InAvail: Integer;
                                      var OutBuffer: PByteArray;
                                      var OutAvail: Integer;
                                      const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    function   engineInOutRatio: Double; virtual; abstract;
    function   engineOutInRatio: Double; virtual; abstract;
  public
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
  end;

implementation

{ TXRTLStreamProcessor }

constructor TXRTLStreamProcessor.Create;
begin
  inherited;
  FState:= spsUnknown;
end;

procedure TXRTLStreamProcessor.CheckState;
begin
  if FState = spsUnknown then
    raise EXRTLInvalidStreamProcessorState.CreateFmt('Invalid %s state', [ClassName]);
end;

procedure TXRTLStreamProcessor.SetState(AState: TXRTLStreamProcessorState);
begin
  FState:= AState;
end;

function TXRTLStreamProcessor.Update(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  CheckState;
  if State = spsClosed then
    Result:= False
  else
    Result:= engineUpdate(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
end;

procedure TXRTLStreamProcessor.engineReset;
begin
end;

procedure TXRTLStreamProcessor.Reset;
begin
  if FState = spsUnknown then
    Exit;
  try
    engineReset;
  finally
    SetState(spsUnknown);
  end;
end;

class function TXRTLStreamProcessor.GetDisplayName: string;
begin
  Result:= ClassName;
end;

{ TXRTLConvertingStreamProcessor }

constructor TXRTLConvertingStreamProcessor.Create;
begin
  inherited;
  FMode:= cspmUnknown;
end;

procedure TXRTLConvertingStreamProcessor.CheckState;
begin
  inherited;
  if (FState = spsRunning) and (FMode = cspmUnknown) then
    raise EXRTLInvalidStreamProcessorState.CreateFmt('Invalid %s mode', [ClassName]);
end;

procedure TXRTLConvertingStreamProcessor.SetMode(AMode: TXRTLConvertingStreamProcessorMode);
begin
  FMode:= AMode;
  if (FState = spsUnknown) and (FMode <> cspmUnknown) then
    SetState(spsRunning);
end;

{ TXRTLCompressingStreamProcessor }

function TXRTLCompressingStreamProcessor.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= Round(engineInOutRatio * OutputSize);
end;

function TXRTLCompressingStreamProcessor.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= Round(engineOutInRatio * InputSize);
end;

function TXRTLCompressingStreamProcessor.engineUpdate(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  case Mode of
    cspmDirect:   Result:= engineUpdateCompress(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
    cspmReverse:  Result:= engineUpdateDecompress(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
  else
    raise EXRTLInvalidStreamProcessorState.CreateFmt('Invalid %s mode', [ClassName]);
  end;
end;

end.

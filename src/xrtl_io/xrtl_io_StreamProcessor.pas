{ @author(Illya Kysil <ikysil at users.berlios.de>)
}
unit xrtl_io_StreamProcessor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception;

type
  EXRTLStreamProcessorException = class(EXRTLException);

  EXRTLInvalidStreamProcessorState = class(EXRTLStreamProcessorException);

{ Requested stream processor operating mode.
}
  TXRTLStreamProcessorOperation = (
// Process as much as possible, buffer output data if required
    spoRun,
// Process as much as possible, flush output data buffer.
// Flush operation doesn't have input buffer defined.
// Note: not all stream processors support flush operation, see
// @link(TXRTLStreamProcessor.IsFlushSupported).
    spoFlush,
// Process as much as possible, flush output data buffer, free internal state data
// Finish operation doesn't have input buffer defined.
    spoFinish
  );

{ Stream processor state.
}
  TXRTLStreamProcessorState = (
// Stream processor is not initialized
    spsUnknown,
// Stream closed
    spsClosed,
// Stream processor is initialized
    spsRunning
  );

{ Abstract stream processor.
}
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
//  Process another chunk of data.
//  Advance input buffer pointer @link(InBuffer) by the number
//  of bytes processed, decrease @link(InAvail) by the same amount.
//  Advance output buffer pointer @link(OutBuffer) by the number
//  of bytes outputted, decrease @link(OutAvail) by the same amount.
//  @param(InAvail length of data available in input buffer)
//  @param(OutBuffer pointer to an output buffer)
//  @param(OutAvail length of output buffer available)
//  @param(Operation requested operation)
//  @returns(@true if output buffer is too small to include all data available
//           (processor buffered excessive output data),
//           @false if all data available has been put into output buffer.)
    function   Update(var InBuffer: PByteArray;
                      var InAvail: Integer;
                      var OutBuffer: PByteArray;
                      var OutAvail: Integer;
                      const Operation: TXRTLStreamProcessorOperation): Boolean;
//  Returns a flag which denotes if this stream processor supports flush operation.
//  If flush operation is not supported then calls to @link(TXRTLOutputStream.Flush)
//  will have no effect on stream processor, i.e. stream processor's buffers will not be
//  flushed. 
//  @returns(@true if @link(spoFlush) is supported.)
    function   IsFlushSupported: Boolean; virtual; abstract;
//  Reset processor into non-initialized state.
    procedure  Reset;
    class function GetDisplayName: string; virtual;
  end;

{ TXRTLConvertingStreamProcessor conversion mode.
  Exact meaning of @link(cspmDirect) and @link(cspmReverse) depends on implementation
  but general rule is that @link(cspmDirect) denotes conversion of application
  data into some other form. Compressing and Enciphering are direct conversions
  while Decompressing and Deciphering are reverse conversions under that rule.
}
  TXRTLConvertingStreamProcessorMode = (
// Conversion mode is not initialized
    cspmUnknown,
// Direct conversion
    cspmDirect,
// Reverse conversion
    cspmReverse
  );

{ Stream processor which provides bidirectional stream data conversion support.
  Enciphering/Deciphering, Compressing/Decompressing, etc. stream processors belong here.
}
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

{ Compressing/Decompressing stream processor.
}
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

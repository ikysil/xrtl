{ 
  @author(Illya Kysil <ikysil at users.berlios.de>)
}
unit xrtl_io_ProcessingStream;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Math,
  xrtl_util_CPUUtils,
  xrtl_io_Exception, xrtl_io_ResourceStrings,
  xrtl_io_Stream,
  xrtl_io_StreamProcessor;

type
  TXRTLProcessingInputStream = class(TXRTLFilterInputStream)
  private
    FOwnProcessor: Boolean;
    FProcessor: TXRTLStreamProcessor;
    FInBuffer: PByteArray;
    FInBufferLength: Integer;
    FInBufferHead: Integer;
    FInBuffered: Integer;
    FOutBuffer: PByteArray;
    FOutBufferLength: Integer;
    FOutBufferHead: Integer;
    FOutBuffered: Integer;
  protected
    procedure  CheckProcessor; virtual;
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const ACoreStream: TXRTLInputStream;
                       const AProcessor: TXRTLStreamProcessor;
                       AOwnCoreStream: Boolean = True;
                       AOwnProcessor: Boolean = True);
    destructor Destroy; override;
    property   Processor: TXRTLStreamProcessor read FProcessor;
    property   OwnProcessor: Boolean read FOwnProcessor write FOwnProcessor;
    function   BytesAvailable: Int64; override;
    function   MarkPosition: TXRTLMarkData; override;
    procedure  RestorePosition(const MarkData: TXRTLMarkData); override;
    function   Skip(const Count: Int64): Int64; override;
  end;

  TXRTLProcessingOutputStream = class(TXRTLFilterOutputStream)
  private
    FOwnProcessor: Boolean;
    FProcessor: TXRTLStreamProcessor;
    FInBuffer: PByteArray;
    FInBufferLength: Integer;
    FInBufferHead: Integer;
    FInBuffered: Integer;
  protected
    procedure  CheckProcessor; virtual;
    procedure  DoClose; override;
    procedure  UpdateAndWrite(const Buffer: PByteArray; const Count: Integer;
                              const Operation: TXRTLStreamProcessorOperation);
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
  public
    constructor Create(const ACoreStream: TXRTLOutputStream;
                       const AProcessor: TXRTLStreamProcessor;
                       AOwnCoreStream: Boolean = True;
                       AOwnProcessor: Boolean = True);
    destructor Destroy; override;
    property   Processor: TXRTLStreamProcessor read FProcessor;
    property   OwnProcessor: Boolean read FOwnProcessor write FOwnProcessor;
    procedure  Flush; override;
  end;

implementation

uses
  xrtl_util_MemoryUtils, xrtl_util_MemoryManager;

{ TXRTLProcessingInputStream }

constructor TXRTLProcessingInputStream.Create(const ACoreStream: TXRTLInputStream;
  const AProcessor: TXRTLStreamProcessor; AOwnCoreStream: Boolean = True;
  AOwnProcessor: Boolean = True);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FProcessor:= AProcessor;
  FOwnProcessor:= AOwnProcessor;
  FInBuffer:= nil;
  FInBufferLength:= 0;
  FInBufferHead:= 0;
  FInBuffered:= 0;
  FOutBuffer:= nil;
  FOutBufferLength:= 0;
  FOutBufferHead:= 0;
  FOutBuffered:= 0;
end;

destructor TXRTLProcessingInputStream.Destroy;
begin
  XRTLFreeMemory(Pointer(FOutBuffer));
  XRTLFreeMemory(Pointer(FInBuffer));
  if FOwnProcessor then
    FreeAndNil(FProcessor);
  inherited;
end;

procedure TXRTLProcessingInputStream.CheckProcessor;
begin
  if not Assigned(Processor) then
    raise EXRTLIOException.Create('Stream processor is not assigned');
  Processor.CheckState;
end;

function TXRTLProcessingInputStream.BytesAvailable: Int64;
begin
  CheckProcessor;
  Result:= FOutBuffered + Processor.GetOutputSize(inherited BytesAvailable + FInBuffered, spoRun);
end;

function TXRTLProcessingInputStream.MarkPosition: TXRTLMarkData;
begin
  raise EXRTLMarkException.CreateFmt(SXRTLMarkExceptionFmt, [ClassName]);
end;

procedure TXRTLProcessingInputStream.RestorePosition(const MarkData: TXRTLMarkData);
begin
  raise EXRTLRestoreException.CreateFmt(SXRTLRestoreExceptionFmt, [ClassName]);
end;

function TXRTLProcessingInputStream.Skip(const Count: Int64): Int64;
begin
  Result:= _Skip(Count);
end;

function TXRTLProcessingInputStream._ReadBuffer(var Buffer; const Count: Integer): Integer;

  procedure FetchOutBufferedData(var OutBuffer: PByteArray; var Count: Integer);
  var
    ResSize: Integer;
  begin
    if (FOutBuffered > 0) and Assigned(FOutBuffer) then
    begin
      ResSize:= IfThen(FOutBuffered < Count, FOutBuffered, Count);
      Inc(Result, ResSize);
      XRTLMoveMemory(XRTLPointerAdd(FOutBuffer, FOutBufferHead), OutBuffer, ResSize);
      OutBuffer:= XRTLPointerAdd(OutBuffer, ResSize);
      Inc(FOutBufferHead, ResSize);
      Dec(FOutBuffered, ResSize);
      Dec(Count, ResSize);
      if FOutBuffered = 0 then
        FOutBufferHead:= 0;
    end;
  end;

  function FetchInBufferedData(const OutBuffer: PByteArray; const Count: Integer): Integer;
  var
    ResSize: Integer;
  begin
    Result:= 0;
    if (FInBuffered > 0) and Assigned(FInBuffer) then
    begin
      ResSize:= IfThen(FInBuffered < Count, FInBuffered, Count);
      XRTLMoveMemory(XRTLPointerAdd(FInBuffer, FInBufferHead), OutBuffer, ResSize);
      Inc(Result, ResSize);
      Inc(FInBufferHead, ResSize);
      Dec(FInBuffered, ResSize);
      if FInBuffered = 0 then
        FInBufferHead:= 0;
    end;
  end;

  procedure StoreInBufferedData(const InBuffer: PByteArray; const Count: Integer);
  var
    NewInBuffered: Integer;
    NewInBuffer: PByteArray;
  begin
    if Count > 0 then
    begin
      NewInBuffer:= nil;
      if Count + FInBuffered > FInBufferLength then
      begin
        XRTLGetMemory(Pointer(NewInBuffer), Count + FInBuffered, [gmoZeroMemory]);
        FInBufferLength:= Count + FInBuffered;
      end
      else
        NewInBuffer:= FInBuffer;
      NewInBuffered:= Count;
      XRTLMoveMemory(InBuffer, NewInBuffer, NewInBuffered);
      if Assigned(FInBuffer) then
      begin
        XRTLMoveMemory(XRTLPointerAdd(FInBuffer, FInBufferHead),
                       XRTLPointerAdd(NewInBuffer, NewInBuffered), FInBuffered);
        if NewInBuffer <> FInBuffer then
          XRTLFreeMemory(Pointer(FInBuffer));
      end;
      FInBuffer:= NewInBuffer;
      FInBufferHead:= 0;
      FInBuffered:=   Count + FInBuffered;
    end;
  end;

  procedure StoreOutBufferedData(const OutBuffer: PByteArray; const NeededCount: Integer; var Count: Integer);
  begin
    if Count > NeededCount then
    begin
      FOutBuffered:= Count - NeededCount;
      FOutBufferHead:= 0;
      if FOutBuffered > FOutBufferLength then
      begin
        XRTLGetMemory(Pointer(FOutBuffer), FOutBuffered, [gmoZeroMemory]);
        FOutBufferLength:= FOutBuffered;
      end;
      XRTLMoveMemory(XRTLPointerAdd(OutBuffer, NeededCount), FOutBuffer, FOutBuffered);
      Count:= NeededCount;
    end;
  end;

var
  LCount, InputSize, OutputSize, UpdateSize, ResSize, RCount, LInAvail: Integer;
  LBuffer, LInBuffer, LUpdateInBuffer, LOutBuffer, LUpdateOutBuffer: PByteArray;
  LOperation: TXRTLStreamProcessorOperation;
  HasMoreData: Boolean;
begin
  CheckProcessor;
  Result:= 0;
  LCount:= Count;
  if LCount <= 0 then
    Exit;
  LBuffer:= @Buffer;
  LInBuffer:= nil;
  LOutBuffer:= nil;
  OutputSize:= 0;
  RCount:= 0;
  HasMoreData:= True;
  try
    repeat
// check for buffered processor output data
      FetchOutBufferedData(LBuffer, LCount);
      if LCount = 0 then
        Break;
      InputSize:= Processor.GetInputSize(LCount, spoRun);
      XRTLGetMemory(Pointer(LInBuffer), InputSize);
// check for buffered processor input data
      LInAvail:= FetchInBufferedData(LInBuffer, InputSize);
      if LInAvail < InputSize then
      begin
        RCount:= inherited _ReadBuffer(XRTLPointerAdd(LInBuffer, LInAvail)^, InputSize - LInAvail);
        if RCount > 0 then
          Inc(LInAvail, RCount);
      end;
      if LInAvail < InputSize then
        LOperation:= spoFinish
      else
        LOperation:= spoRun;
      Inc(OutputSize, Processor.GetOutputSize(LInAvail, LOperation));
      if LInAvail = 0 then
        Break;
      XRTLGetMemory(Pointer(LOutBuffer), OutputSize);
      LUpdateInBuffer:= LInBuffer;
      UpdateSize:= OutputSize;
      LUpdateOutBuffer:= LOutBuffer;
      HasMoreData:= Processor.Update(LUpdateInBuffer, LInAvail, LUpdateOutBuffer, UpdateSize, LOperation);
// store unprocessed input data
      StoreInBufferedData(LUpdateInBuffer, LInAvail);
      ResSize:= OutputSize - UpdateSize;
// store excessive output data
      StoreOutBufferedData(LOutBuffer, LCount, ResSize);
      XRTLMoveMemory(LOutBuffer, LBuffer, ResSize);
      LBuffer:= XRTLPointerAdd(LBuffer, ResSize);
      Inc(Result, ResSize);
      Dec(LCount, ResSize);
    until (RCount < 0) or (LCount = 0) or not HasMoreData;
    if Result = 0 then
      Result:= XRTLEndOfStreamValue;
  finally
    XRTLFreeMemory(Pointer(LOutBuffer));
    XRTLFreeMemory(Pointer(LInBuffer));
  end;
end;

{ TXRTLProcessingOutputStream }

constructor TXRTLProcessingOutputStream.Create(const ACoreStream: TXRTLOutputStream;
  const AProcessor: TXRTLStreamProcessor; AOwnCoreStream: Boolean = True;
  AOwnProcessor: Boolean = True);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FProcessor:= AProcessor;
  FOwnProcessor:= AOwnProcessor;
  FInBuffer:= nil;
  FInBufferLength:= 0;
  FInBufferHead:= 0;
  FInBuffered:= 0;
end;

destructor TXRTLProcessingOutputStream.Destroy;
begin
  XRTLFreeMemory(Pointer(FInBuffer));
  if FOwnProcessor then
    FreeAndNil(FProcessor);
  inherited;
end;

procedure TXRTLProcessingOutputStream.CheckProcessor;
begin
  if not Assigned(Processor) then
    raise EXRTLIOException.Create('Stream processor is not assigned');
  Processor.CheckState;
end;

procedure TXRTLProcessingOutputStream.UpdateAndWrite(const Buffer: PByteArray;
  const Count: Integer; const Operation: TXRTLStreamProcessorOperation);

  function FetchInBufferedData(const OutBuffer: PByteArray; const Count: Integer): Integer;
  var
    ResSize: Integer;
  begin
    Result:= 0;
    if (FInBuffered > 0) and Assigned(FInBuffer) then
    begin
      ResSize:= IfThen(FInBuffered < Count, FInBuffered, Count);
      XRTLMoveMemory(XRTLPointerAdd(FInBuffer, FInBufferHead), OutBuffer, ResSize);
      Inc(Result, ResSize);
      Inc(FInBufferHead, ResSize);
      Dec(FInBuffered, ResSize);
      if FInBuffered = 0 then
        FInBufferHead:= 0;
    end;
  end;

  procedure StoreInBufferedData(const InBuffer: PByteArray; const Count: Integer);
  var
    NewInBuffered: Integer;
    NewInBuffer: PByteArray;
  begin
    if Count > 0 then
    begin
      if Count + FInBuffered > FInBufferLength then
      begin
        NewInBuffer:= XRTLGetMemory(Count + FInBuffered);
        FInBufferLength:= Count + FInBuffered;
      end
      else
        NewInBuffer:= FInBuffer;
      NewInBuffered:= Count;
      XRTLMoveMemory(InBuffer, NewInBuffer, NewInBuffered);
      if Assigned(FInBuffer) then
      begin
        XRTLMoveMemory(XRTLPointerAdd(FInBuffer, FInBufferHead),
                       XRTLPointerAdd(NewInBuffer, NewInBuffered), FInBuffered);
        if NewInBuffer <> FInBuffer then
          XRTLFreeMemory(Pointer(FInBuffer));
      end;
      FInBuffer:= NewInBuffer;
      FInBufferHead:= 0;
      FInBuffered:= Count + FInBuffered;
    end;
  end;

var
  InputSize, OutputSize: Integer;
  InputBuffer, OutputBuffer: PByteArray;
  LInAvail, LOutAvail: Integer;
  LInBuffer, LOutBuffer: PByteArray;
  HasMoreData: Boolean;
begin
  CheckProcessor;
  if Count < 0 then
    Exit;
  OutputBuffer:= nil;
  InputBuffer:= nil;
  try
    OutputSize:= Processor.GetOutputSize(Count + FInBuffered, Operation);
    InputSize:= Processor.GetInputSize(OutputSize, Operation);
    XRTLGetMemory(Pointer(OutputBuffer), OutputSize);
    XRTLGetMemory(Pointer(InputBuffer), InputSize);
    LInBuffer:= InputBuffer;
    LInAvail:= FetchInBufferedData(LInBuffer, InputSize);
    XRTLMoveMemory(Buffer, XRTLPointerAdd(LInBuffer, LInAvail), Count);
    Inc(LInAvail, Count);
{
    InputSize:= Count + FInBuffered;
    OutputSize:= Processor.GetOutputSize(InputSize, Operation);
    XRTLGetMemory(Pointer(OutputBuffer), OutputSize);
    if FInBuffered > 0 then
    begin
      XRTLGetMemory(Pointer(InputBuffer), InputSize);
      LInBuffer:= InputBuffer;
      LInAvail:= FetchInBufferedData(LInBuffer, InputSize);
      XRTLMoveMemory(Buffer, XRTLPointerAdd(LInBuffer, LInAvail), Count);
      Inc(LInAvail, Count);
    end
    else
    begin
      LInBuffer:= Buffer;
      LInAvail:= Count;
    end;
}
    repeat
      LOutBuffer:= OutputBuffer;
      LOutAvail:= OutputSize;
      HasMoreData:= Processor.Update(LInBuffer, LInAvail, LOutBuffer, LOutAvail, Operation);
      if OutputSize - LOutAvail > 0 then
      begin
        inherited _WriteBuffer(OutputBuffer^, OutputSize - LOutAvail);
      end;
    until (LOutAvail = OutputSize) or ((LInAvail = 0) and (Operation = spoRun)) or
          not HasMoreData;
    StoreInBufferedData(LInBuffer, LInAvail);
  finally
    XRTLFreeMemory(Pointer(OutputBuffer));
    XRTLFreeMemory(Pointer(InputBuffer));
  end;
end;

procedure TXRTLProcessingOutputStream.DoClose;
begin
  Flush;
  UpdateAndWrite(nil, 0, spoFinish);
  inherited;
end;

procedure TXRTLProcessingOutputStream.Flush;
begin
  CheckProcessor;
  if FProcessor.IsFlushSupported then
    UpdateAndWrite(nil, 0, spoFlush);
  inherited;
end;

procedure TXRTLProcessingOutputStream._WriteBuffer(const Buffer; const Count: Integer);
begin
  UpdateAndWrite(@Buffer, Count, spoRun);
end;

end.

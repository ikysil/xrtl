unit xrtl_io_bzlib_StreamProcessor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_io_StreamProcessor,
  xrtl_io_bzlib_bzlib;

type
  TXRTLbzlibStreamProcessor = class(TXRTLCompressingStreamProcessor)
  private
    FBZRec: TBZStreamRec;
  protected
    function   engineInOutRatio: Double; override;
    function   engineOutInRatio: Double; override;
    function   engineUpdateCompress(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateDecompress(var InBuffer: PByteArray;
                                      var InAvail: Integer;
                                      var OutBuffer: PByteArray;
                                      var OutAvail: Integer;
                                      const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  public
    constructor Create;
    procedure  InitCompress(ABlockSize100k: TbzlibBlockSize100k = bs9);
    procedure  InitDecompress;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   IsFlushSupported: Boolean; override;
  end;

implementation

{ TXRTLbzlibStreamProcessor }

constructor TXRTLbzlibStreamProcessor.Create;
begin
  inherited;
end;

procedure TXRTLbzlibStreamProcessor.InitCompress(ABlockSize100k: TbzlibBlockSize100k = bs9);
const
  BlockSizes: array[TbzlibBlockSize100k] of ShortInt = (1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  SetState(spsUnknown);
  FillChar(FBZRec, SizeOf(FBZRec), 0);
  FBZRec.bzalloc:= bzip2AllocMem;
  FBZRec.bzfree:= bzip2FreeMem;
  bzlibCheck(bzCompressInit(FBZRec, BlockSizes[ABlockSize100k], 0, 0));
  SetMode(cspmDirect);
end;

procedure TXRTLbzlibStreamProcessor.InitDecompress;
begin
  SetState(spsUnknown);
  FillChar(FBZRec, SizeOf(FBZRec), 0);
  FBZRec.bzalloc:= bzip2AllocMem;
  FBZRec.bzfree:= bzip2FreeMem;
  bzlibCheck(bzDecompressInit(FBZRec, 0, 0));
  SetMode(cspmReverse);
end;

function TXRTLbzlibStreamProcessor.engineInOutRatio: Double;
begin
  if FBZRec.total_out = 0 then
    Result:= 1.0
  else
    Result:= FBZRec.total_in / FBZRec.total_out;
end;

function TXRTLbzlibStreamProcessor.engineOutInRatio: Double;
begin
  if FBZRec.total_in = 0 then
    Result:= 1.0
  else
    Result:= FBZRec.total_out / FBZRec.total_in;
end;

function TXRTLbzlibStreamProcessor.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= inherited GetInputSize(OutputSize, Operation);
  Inc(Result, 12);
end;

function TXRTLbzlibStreamProcessor.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= inherited GetOutputSize(InputSize, Operation);
  Inc(Result, 12);
end;

function TXRTLbzlibStreamProcessor.engineUpdateCompress(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  ZMode: Integer;
  ZState: Integer;
begin
  ZMode:= BZ_RUN;
  case Operation of
    spoRun:    ZMode:= BZ_RUN;
    spoFlush:  ZMode:= BZ_FLUSH;
    spoFinish: ZMode:= BZ_FINISH;
  else
    XRTLInvalidOperation(ClassName, 'engineUpdateCompress', 'Unknown operation');
  end;
  FBZRec.next_in:= PChar(InBuffer);
  FBZRec.avail_in:= InAvail;
  FBZRec.next_out:= PChar(OutBuffer);
  FBZRec.avail_out:= OutAvail;
  ZState:= bzCompress(FBZRec, ZMode);
  if bzlibCheck(ZState) = BZ_STREAM_END then
  begin
    Result:= False;
    SetState(spsClosed);
    bzlibCheck(bzCompressEnd(FBZRec));
  end
  else
    Result:= True;
  InBuffer:= PByteArray(FBZRec.next_in);
  InAvail:= FBZRec.avail_in;
  OutBuffer:= PByteArray(FBZRec.next_out);
  OutAvail:= FBZRec.avail_out;
end;

function TXRTLbzlibStreamProcessor.engineUpdateDecompress(
  var InBuffer: PByteArray; var InAvail: Integer;
  var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  ZState: Integer;
begin
  FBZRec.next_in:= PChar(InBuffer);
  FBZRec.avail_in:= InAvail;
  FBZRec.next_out:= PChar(OutBuffer);
  FBZRec.avail_out:= OutAvail;
  ZState:= bzDecompress(FBZRec);
  if bzlibCheck(ZState) = BZ_STREAM_END then
  begin
    Result:= False;
    SetState(spsClosed);
    bzlibCheck(bzDecompressEnd(FBZRec));
  end
  else
    Result:= True;
  InBuffer:= PByteArray(FBZRec.next_in);
  InAvail:= FBZRec.avail_in;
  OutBuffer:= PByteArray(FBZRec.next_out);
  OutAvail:= FBZRec.avail_out;
end;

function TXRTLbzlibStreamProcessor.IsFlushSupported: Boolean;
begin
  Result:= True;
end;

end.

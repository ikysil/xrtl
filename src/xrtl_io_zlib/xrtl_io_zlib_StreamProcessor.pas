{ 
  @author(Illya Kysil <ikysil at users.berlios.de>)
}
unit xrtl_io_zlib_StreamProcessor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_io_StreamProcessor,
  xrtl_io_zlib_zlib;

const
  XRTLzlibNoCompression      = 0;
  XRTLzlibDefaultCompression = 6;
  XRTLzlibBestCompression    = 9;

type
  TXRTLzlibCompressionLevel = XRTLzlibNoCompression .. XRTLzlibBestCompression;

const
  XRTLzlibDefaultCompressionLevel = XRTLzlibBestCompression;

type
  TXRTLzlibStreamProcessor = class(TXRTLCompressingStreamProcessor)
  private
    FZRec: TZStreamRec;
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
    procedure  InitCompress(ACompressionLevel: TXRTLzlibCompressionLevel = XRTLzlibDefaultCompressionLevel);
    procedure  InitDecompress;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   IsFlushSupported: Boolean; override;
  end;

implementation

{ TXRTLzlibStreamProcessor }

constructor TXRTLzlibStreamProcessor.Create;
begin
  inherited;
end;

procedure TXRTLzlibStreamProcessor.InitCompress(ACompressionLevel: TXRTLzlibCompressionLevel = XRTLzlibDefaultCompressionLevel);
begin
  SetState(spsUnknown);
  FillChar(FZRec, SizeOf(FZRec), 0);
  zlibCheck(deflateInit_(FZRec, ACompressionLevel, zlib_version, SizeOf(FZRec)));
  SetMode(cspmDirect);
end;

procedure TXRTLzlibStreamProcessor.InitDecompress;
begin
  SetState(spsUnknown);
  FillChar(FZRec, SizeOf(FZRec), 0);
  zlibCheck(inflateInit_(FZRec, zlib_version, SizeOf(FZRec)));
  SetMode(cspmReverse);
end;

function TXRTLzlibStreamProcessor.engineInOutRatio: Double;
begin
  if FZRec.total_out = 0 then
    Result:= 1.0
  else
    Result:= FZRec.total_in / FZRec.total_out;
end;

function TXRTLzlibStreamProcessor.engineOutInRatio: Double;
begin
  if FZRec.total_in = 0 then
    Result:= 1.0
  else
    Result:= FZRec.total_out / FZRec.total_in;
end;

function TXRTLzlibStreamProcessor.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= inherited GetInputSize(OutputSize, Operation);
  Inc(Result, 12);
end;

function TXRTLzlibStreamProcessor.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= inherited GetOutputSize(InputSize, Operation);
  Inc(Result, 12);
end;

function TXRTLzlibStreamProcessor.engineUpdateCompress(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  ZMode: Integer;
  ZState: Integer;
begin
  ZMode:= Z_NO_FLUSH;
  case Operation of
    spoRun:    ZMode:= Z_NO_FLUSH;
    spoFlush:  ZMode:= Z_FULL_FLUSH;
    spoFinish: ZMode:= Z_FINISH;
  else
    XRTLInvalidOperation(ClassName, 'engineUpdateCompress', 'Unknown operation');
  end;
  FZRec.next_in:= PChar(InBuffer);
  FZRec.avail_in:= InAvail;
  FZRec.next_out:= PChar(OutBuffer);
  FZRec.avail_out:= OutAvail;
  ZState:= deflate(FZRec, ZMode);
  if zlibCheck(ZState) = Z_STREAM_END then
  begin
    Result:= False;
    SetState(spsClosed);
    zlibCheck(deflateEnd(FZRec));
  end
  else
    Result:= True;
  InBuffer:= PByteArray(FZRec.next_in);
  InAvail:= FZRec.avail_in;
  OutBuffer:= PByteArray(FZRec.next_out);
  OutAvail:= FZRec.avail_out;
end;

function TXRTLzlibStreamProcessor.engineUpdateDecompress(
  var InBuffer: PByteArray; var InAvail: Integer;
  var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  ZMode: Integer;
  ZState: Integer;
begin
  ZMode:= Z_NO_FLUSH;
  case Operation of
    spoRun:    ZMode:= Z_NO_FLUSH;
    spoFlush:  ZMode:= Z_FULL_FLUSH;
    spoFinish: ZMode:= Z_FINISH;
  else
    XRTLInvalidOperation(ClassName, 'engineUpdateDecompress', 'Unknown operation');
  end;
  FZRec.next_in:= PChar(InBuffer);
  FZRec.avail_in:= InAvail;
  FZRec.next_out:= PChar(OutBuffer);
  FZRec.avail_out:= OutAvail;
  ZState:= inflate(FZRec, ZMode);
  if zlibCheck(ZState) = Z_STREAM_END then
  begin
    Result:= False;
    SetState(spsClosed);
    zlibCheck(inflateEnd(FZRec));
  end
  else
    Result:= True;
  InBuffer:= PByteArray(FZRec.next_in);
  InAvail:= FZRec.avail_in;
  OutBuffer:= PByteArray(FZRec.next_out);
  OutAvail:= FZRec.avail_out;
end;

function TXRTLzlibStreamProcessor.IsFlushSupported: Boolean;
begin
  Result:= False;
end;

end.

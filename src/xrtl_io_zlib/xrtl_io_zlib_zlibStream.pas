unit xrtl_io_zlib_zlibStream;

{$INCLUDE xrtl.inc}

interface

uses
  Math,
  xrtl_io_Stream, xrtl_io_zlib_zlib;

const
  XRTLzlibDefaultBufferSize  = 32768;
  XRTLzlibNoCompression      = 0;
  XRTLzlibDefaultCompression = 6;
  XRTLzlibBestCompression    = 9;

type
  TXRTLzlibCompressionLevel = XRTLzlibNoCompression .. XRTLzlibBestCompression;

const
  XRTLzlibDefaultCompressionLevel = XRTLzlibBestCompression;

type
  TXRTLzlibInputStream = class(TXRTLFilterInputStream)
  private
    FZRec: TZStreamRec;
    FBufferSize: Integer;
    FInputBuffer: PChar;
    FOutputBuffer: PChar;
    FInPrivateBuffer: Integer;
    FPrivateHead: PChar;
    FPrivateBuffer: PChar;
    procedure  Reset;
  protected
    procedure  DoClose; override;
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const ACoreStream: TXRTLInputStream; AOwnCoreStream: Boolean = True;
                       ABufferSize: Integer = XRTLzlibDefaultBufferSize);
    destructor Destroy; override;
    function   BytesAvailable: Int64; override;
    function   MarkPosition: TXRTLMarkData; override;
    procedure  RestorePosition(const MarkData: TXRTLMarkData); override;
    function   Skip(const Count: Int64): Int64; override;
  end;

  TXRTLzlibOutputStream = class(TXRTLFilterOutputStream)
  private
    FZRec: TZStreamRec;
    FBufferSize: Integer;
    FInputBuffer: PChar;
    FOutputBuffer: PChar;
    procedure  Reset;
    function   GetCompressionRate: Double;
    procedure  _Flush;
  protected
    procedure  DoClose; override;
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
  public
    constructor Create(const ACoreStream: TXRTLOutputStream; AOwnCoreStream: Boolean = True;
                       ABufferSize: Integer = XRTLzlibDefaultBufferSize;
                       ACompressionLevel: TXRTLzlibCompressionLevel = XRTLzlibDefaultCompressionLevel);
    destructor Destroy; override;
    procedure  Flush; override;
    property   CompressionRate: Double read GetCompressionRate;
  end;

implementation

uses
  xrtl_util_MemoryUtils;

{ TXRTLzlibInputStream }

constructor TXRTLzlibInputStream.Create(const ACoreStream: TXRTLInputStream;
                                        AOwnCoreStream: Boolean = True;
                                        ABufferSize: Integer = XRTLzlibDefaultBufferSize);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FillChar(FZRec, SizeOf(FZRec), 0);
  FInputBuffer:= nil;
  FOutputBuffer:= nil;
  FBufferSize:= ABufferSize;
  GetMem(FInputBuffer, FBufferSize);
  FillChar(FInputBuffer^, FBufferSize, 0);
  GetMem(FOutputBuffer, FBufferSize);
  FillChar(FOutputBuffer^, FBufferSize, 0);
  Reset;
  GetMem(FPrivateBuffer, FBufferSize);
  FPrivateHead:= FPrivateBuffer;
  FInPrivateBuffer:= 0;
  zlibCheck(inflateInit_(FZRec, zlib_version, SizeOf(FZRec)));
end;

destructor TXRTLzlibInputStream.Destroy;
begin
  if Assigned(FPrivateBuffer) then
    FreeMem(FPrivateBuffer);
  if Assigned(FOutputBuffer) then
    FreeMem(FOutputBuffer);
  if Assigned(FInputBuffer) then
    FreeMem(FInputBuffer);
  inherited;
end;

procedure TXRTLzlibInputStream.Reset;
begin
  FillChar(FZRec, SizeOf(FZRec), 0);
  FZRec.next_in:= FInputBuffer;
  FZRec.next_out:= FOutputBuffer;
  FZRec.avail_out:= FBufferSize;
end;

function TXRTLzlibInputStream.BytesAvailable: Int64;
begin
  Result:= FInPrivateBuffer + CoreStream.BytesAvailable;
end;

procedure TXRTLzlibInputStream.DoClose;
begin
  zlibCheck(inflateEnd(FZRec));
  inherited;
end;

function TXRTLzlibInputStream.MarkPosition: TXRTLMarkData;
begin
  Result:= inherited MarkPosition;
end;

procedure TXRTLzlibInputStream.RestorePosition(const MarkData: TXRTLMarkData);
begin
  inherited;
end;

function TXRTLzlibInputStream._ReadBuffer(var Buffer; const Count: Integer): Integer;
var
  P: PChar;
  ToCopy: Integer;
  ZResult: Integer;
  LCount: Integer;
begin
  P:= @Buffer;
  Result:= 0;
  ZResult:= Z_OK;
  LCount:= Count;
// Loop while we have more data to process
  while (LCount > 0) and
        ((ZResult = Z_OK) or ((ZResult = Z_STREAM_END) and (FInPrivateBuffer > 0))) do
  begin
    if (FInPrivateBuffer > 0) then
    begin
      ToCopy:= Min(LCount, FInPrivateBuffer);
      XRTLMoveMemory(FPrivateHead, P, ToCopy);
      Inc(FPrivateHead, ToCopy);
      Dec(FInPrivateBuffer, ToCopy);
      Inc(P, ToCopy);
      Dec(LCount, ToCopy);
      Inc(Result, ToCopy);
    end
    else
    begin
      // Check if the stream can accept more input for compression
      if (FZRec.avail_in = 0) then
      begin
        FZRec.next_in:= FInputBuffer;
        FZRec.avail_in:= CoreStream.ReadBuffer(FZRec.next_in^, FBufferSize);
      end;
      // Now call the decompression routine
      if (FZRec.avail_in < 0) then
        ZResult:= inflate(FZRec, Z_FINISH)
      else
        ZResult:= inflate(FZRec, Z_NO_FLUSH);
      zlibCheck(ZResult);
      // Check if the output buffer is full
      if (FZRec.avail_out = 0) or (ZResult = Z_STREAM_END) then
      begin
        // Calculate how much to copy
        ToCopy:= (FBufferSize - FZRec.avail_out);
        // Copy to local buffer
        XRTLMoveMemory(FOutputBuffer, FPrivateBuffer, ToCopy);
        FPrivateHead:= FPrivateBuffer;
        FInPrivateBuffer:= ToCopy;
        // Reset zlib data
        FZRec.avail_out:= FBufferSize;
        FZRec.next_out:= FOutputBuffer;
      end;
    end;
  end;
  if (ZResult = Z_STREAM_END) and (Result = 0) then
    Result:= XRTLEndOfStreamValue;
end;

function TXRTLzlibInputStream.Skip(const Count: Int64): Int64;
begin
  Result:= _Skip(Count);
end;

{ TXRTLzlibOutputStream }

constructor TXRTLzlibOutputStream.Create(const ACoreStream: TXRTLOutputStream;
                                         AOwnCoreStream: Boolean = True;
                                         ABufferSize: Integer = XRTLzlibDefaultBufferSize;
                                         ACompressionLevel: TXRTLzlibCompressionLevel = XRTLzlibDefaultCompressionLevel);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FillChar(FZRec, SizeOf(FZRec), 0);
  FInputBuffer:= nil;
  FOutputBuffer:= nil;
  FBufferSize:= ABufferSize;
  GetMem(FInputBuffer, FBufferSize);
  FillChar(FInputBuffer^, FBufferSize, 0);
  GetMem(FOutputBuffer, FBufferSize);
  FillChar(FOutputBuffer^, FBufferSize, 0);
  Reset;
  zlibCheck(deflateInit_(FZRec, ACompressionLevel, zlib_version, SizeOf(FZRec)));
end;

destructor TXRTLzlibOutputStream.Destroy;
begin
  if Assigned(FOutputBuffer) then FreeMem(FOutputBuffer);
  if Assigned(FInputBuffer) then FreeMem(FInputBuffer);
  inherited;
end;

procedure TXRTLzlibOutputStream.Reset;
begin
  FillChar(FZRec, SizeOf(FZRec), 0);
  FZRec.next_in:= FInputBuffer;
  FZRec.next_out:= FOutputBuffer;
  FZRec.avail_out:= FBufferSize;
end;

function TXRTLzlibOutputStream.GetCompressionRate: Double;
begin
  if FZRec.total_in = 0 then
    Result:= 0
  else
    Result:= (1.0 - (FZRec.total_out / FZRec.total_in)) * 100.0;
end;

procedure TXRTLzlibOutputStream.DoClose;
begin
  Flush;
  FZRec.next_in:= nil;
  FZRec.avail_in:= 0;
  while (zlibCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END) and (FZRec.avail_out < FBufferSize) do
  begin
    _Flush;
  end;
  _Flush;
  zlibCheck(deflateEnd(FZRec));
  inherited;
end;

procedure TXRTLzlibOutputStream._Flush;
begin
  if FZRec.avail_out < FBufferSize then
  begin
    CoreStream.WriteBuffer(FOutputBuffer^, FBufferSize - FZRec.avail_out);
    FZRec.next_out:= FOutputBuffer;
    FZRec.avail_out:= FBufferSize;
  end;
end;

procedure TXRTLzlibOutputStream.Flush;
begin
  while (zlibCheck(deflate(FZRec, Z_FULL_FLUSH)) = Z_OK) and (FZRec.avail_in > 0) do
    _Flush;
  _Flush;
  inherited;
end;

procedure TXRTLzlibOutputStream._WriteBuffer(const Buffer; const Count: Integer);
begin
  FZRec.next_in:= @Buffer;
  FZRec.avail_in:= Count;
  while (FZRec.avail_in > 0) do
  begin
    zlibCheck(deflate(FZRec, Z_NO_FLUSH));
    if FZRec.avail_out = 0 then
    begin
      CoreStream.WriteBuffer(FOutputBuffer^, FBufferSize - FZRec.avail_out);
      FZRec.next_out:= FOutputBuffer;
      FZRec.avail_out:= FBufferSize;
    end;
  end;
end;

end.


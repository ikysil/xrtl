unit xrtl_io_bzlib_bzlibStream;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_io_Stream,
  xrtl_io_bzlib_bzlib;

type
  TXRTLbzlibInputStream = class(TXRTLFilterInputStream)
  private
    FBZRec: TBZStreamRec;
    FBuffer: array[Word] of Char;
  protected
    procedure  DoClose; override;
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const ACoreStream: TXRTLInputStream; AOwnCoreStream: Boolean = True);
    function   BytesAvailable: Int64; override;
    function   MarkPosition: TXRTLMarkData; override;
    procedure  RestorePosition(const MarkData: TXRTLMarkData); override;
    function   Skip(const Count: Int64): Int64; override;
  end;

  TXRTLbzlibOutputStream = class(TXRTLFilterOutputStream)
  private
    FBZRec: TBZStreamRec;
    FBuffer: array[Word] of Char;
    function   GetCompressionRate: Double;
    procedure  _Flush;
  protected
    procedure  DoClose; override;
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
  public
    constructor Create(const ACoreStream: TXRTLOutputStream; AOwnCoreStream: Boolean = True;
                       ABlockSize100k: TbzlibBlockSize100k = bs9);
    procedure  Flush; override;
    property   CompressionRate: Double read GetCompressionRate;
  end;

implementation

{ TXRTLbzlibInputStream }

constructor TXRTLbzlibInputStream.Create(const ACoreStream: TXRTLInputStream;
                                       AOwnCoreStream: Boolean = True);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FillChar(FBZRec, SizeOf(FBZRec), 0);
  FBZRec.bzalloc:= bzip2AllocMem;
  FBZRec.bzfree:= bzip2FreeMem;
  FBZRec.next_in:= FBuffer;
  FBZRec.avail_in:= 0;
  bzlibCheck(bzDecompressInit(FBZRec, 0, 0));
end;

function TXRTLbzlibInputStream.BytesAvailable: Int64;
begin
  Result:= CoreStream.BytesAvailable;
end;

procedure TXRTLbzlibInputStream.DoClose;
begin
  bzlibCheck(bzDecompressEnd(FBZRec));
  inherited;
end;

function TXRTLbzlibInputStream.MarkPosition: TXRTLMarkData;
begin
  Result:= inherited MarkPosition;
end;

procedure TXRTLbzlibInputStream.RestorePosition(const MarkData: TXRTLMarkData);
begin
  inherited;
end;

function TXRTLbzlibInputStream._ReadBuffer(var Buffer; const Count: Integer): Integer;
var
  BZResult: Integer;
begin
  FBZRec.next_out:= @Buffer;
  FBZRec.avail_out:= Count;
  BZResult:= BZ_OK;
  while (FBZRec.avail_out > 0) and (BZResult = BZ_OK) do
  begin
    if FBZRec.avail_in = 0 then
    begin
      FBZRec.avail_in:= CoreStream.ReadBuffer(FBuffer, SizeOf(FBuffer));
      FBZRec.next_in:= FBuffer;
    end;
    if FBZRec.avail_in > 0 then
      BZResult:= bzlibCheck(bzDecompress(FBZRec))
    else
      BZResult:= BZ_STREAM_END;
  end;
  Result:= Count - FBZRec.avail_out;
  if (BZResult = BZ_STREAM_END) and (Result = 0) then
    Result:= XRTLEndOfStreamValue;
end;

function TXRTLbzlibInputStream.Skip(const Count: Int64): Int64;
begin
  Result:= _Skip(Count);
end;

{ TXRTLbzlibOutputStream }

constructor TXRTLbzlibOutputStream.Create(const ACoreStream: TXRTLOutputStream;
                                          AOwnCoreStream: Boolean = True;
                                          ABlockSize100k: TbzlibBlockSize100k = bs9);
const
  BlockSizes: array[TbzlibBlockSize100k] of ShortInt = (1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FBZRec.bzalloc:= bzip2AllocMem;
  FBZRec.bzfree:= bzip2FreeMem;
  FBZRec.next_out:= FBuffer;
  FBZRec.avail_out:= sizeof(FBuffer);
  bzlibCheck(bzCompressInit(FBZRec, BlockSizes[ABlockSize100k], 0, 0));
end;

function TXRTLbzlibOutputStream.GetCompressionRate: Double;
begin
  if FBZRec.total_in = 0 then
    Result:= 0
  else
    Result:= (1.0 - (FBZRec.total_out / FBZRec.total_in)) * 100.0;
end;

procedure TXRTLbzlibOutputStream.DoClose;
begin
  Flush;
  FBZRec.next_in:= nil;
  FBZRec.avail_in:= 0;
  try
    while (bzlibCheck(bzCompress(FBZRec, BZ_FINISH)) <> BZ_STREAM_END)
      and (FBZRec.avail_out = 0) do
    begin
      CoreStream.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FBZRec.next_out:= FBuffer;
      FBZRec.avail_out:= sizeof(FBuffer);
    end;
    if FBZRec.avail_out < sizeof(FBuffer) then
      CoreStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FBZRec.avail_out);
  finally
    bzCompressEnd(FBZRec);
  end;
  inherited;
end;

procedure TXRTLbzlibOutputStream._Flush;
begin
  if FBZRec.avail_out < SizeOf(FBuffer) then
  begin
    CoreStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FBZRec.avail_out);
    FBZRec.next_out:= FBuffer;
    FBZRec.avail_out:= SizeOf(FBuffer);
  end;
end;

procedure TXRTLbzlibOutputStream.Flush;
begin
  while bzlibCheck(bzCompress(FBZRec, BZ_FLUSH)) <> BZ_RUN_OK do
    _Flush;
  _Flush;
  inherited;
end;

procedure TXRTLbzlibOutputStream._WriteBuffer(const Buffer; const Count: Integer);
begin
  FBZRec.next_in:= @Buffer;
  FBZRec.avail_in:= Count;
  while FBZRec.avail_in > 0 do
  begin
    bzlibCheck(bzCompress(FBZRec, BZ_RUN));
    if FBZRec.avail_out = 0 then
    begin
      CoreStream.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FBZRec.next_out:= FBuffer;
      FBZRec.avail_out:= SizeOf(FBuffer);
    end;
  end;
end;

end.


unit xrtl_io_StreamAdapter;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Classes,
  xrtl_io_Exception, xrtl_io_ResourceStrings, xrtl_io_Stream;

type
  TXRTLStreamToInputStreamAdapter = class(TXRTLInputStream)
  private
    FOwnCoreStream: Boolean;
    FCoreStream: TStream;
  protected
    procedure  DoClose; override;
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const ACoreStream: TStream; AOwnCoreStream: Boolean = True);
    destructor Destroy; override;
    property   CoreStream: TStream read FCoreStream;
    property   OwnCoreStream: Boolean read FOwnCoreStream write FOwnCoreStream;
    function   BytesAvailable: Int64; override;
  end;

  TXRTLStreamToOutputStreamAdapter = class(TXRTLOutputStream)
  private
    FOwnCoreStream: Boolean;
    FCoreStream: TStream;
  protected
    procedure  DoClose; override;
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
  public
    constructor Create(const ACoreStream: TStream; AOwnCoreStream: Boolean = True);
    destructor Destroy; override;
    property   CoreStream: TStream read FCoreStream;
    property   OwnCoreStream: Boolean read FOwnCoreStream write FOwnCoreStream;
  end;

  TXRTLInputStreamToStreamAdapter = class(TStream)
  private
    FOwnCoreStream: Boolean;
    FCoreStream: TXRTLInputStream;
  public
    constructor Create(const ACoreStream: TXRTLInputStream; AOwnCoreStream: Boolean = True);
    destructor Destroy; override;
    property   CoreStream: TXRTLInputStream read FCoreStream;
    property   OwnCoreStream: Boolean read FOwnCoreStream write FOwnCoreStream;
    function   Read(var Buffer; Count: Integer): Integer; override;
    function   Write(const Buffer; Count: Integer): Integer; override;
    function   Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TXRTLOutputStreamToStreamAdapter = class(TStream)
  private
    FOwnCoreStream: Boolean;
    FCoreStream: TXRTLOutputStream;
  public
    constructor Create(const ACoreStream: TXRTLOutputStream; AOwnCoreStream: Boolean = True);
    destructor Destroy; override;
    property   CoreStream: TXRTLOutputStream read FCoreStream;
    property   OwnCoreStream: Boolean read FOwnCoreStream write FOwnCoreStream;
    function   Read(var Buffer; Count: Integer): Integer; override;
    function   Write(const Buffer; Count: Integer): Integer; override;
    function   Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

{ TXRTLStreamToInputStreamAdapter }

constructor TXRTLStreamToInputStreamAdapter.Create(const ACoreStream: TStream; AOwnCoreStream: Boolean = True);
begin
  inherited Create;
  FCoreStream:= ACoreStream;
  FOwnCoreStream:= AOwnCoreStream;
end;

destructor TXRTLStreamToInputStreamAdapter.Destroy;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

function TXRTLStreamToInputStreamAdapter.BytesAvailable: Int64;
begin
  Result:= FCoreStream.Size - FCoreStream.Position;
end;

procedure TXRTLStreamToInputStreamAdapter.DoClose;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

function TXRTLStreamToInputStreamAdapter._ReadBuffer(var Buffer; const Count: Integer): Integer;
begin
  Result:= FCoreStream.Read(Buffer, Count);
  if (Result = 0) and (Count > 0) then
    Result:= XRTLEndOfStreamValue;
end;

{ TXRTLStreamToOutputStreamAdapter }

constructor TXRTLStreamToOutputStreamAdapter.Create(const ACoreStream: TStream; AOwnCoreStream: Boolean = True);
begin
  inherited Create;
  FCoreStream:= ACoreStream;
  FOwnCoreStream:= AOwnCoreStream;
end;

destructor TXRTLStreamToOutputStreamAdapter.Destroy;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

procedure TXRTLStreamToOutputStreamAdapter.DoClose;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

procedure TXRTLStreamToOutputStreamAdapter._WriteBuffer(const Buffer; const Count: Integer);
begin
  FCoreStream.WriteBuffer(Buffer, Count);
end;

{ TXRTLInputStreamToStreamAdapter }

constructor TXRTLInputStreamToStreamAdapter.Create(const ACoreStream: TXRTLInputStream; AOwnCoreStream: Boolean = True);
begin
  inherited Create;
  FCoreStream:= ACoreStream;
  FOwnCoreStream:= AOwnCoreStream;
end;

destructor TXRTLInputStreamToStreamAdapter.Destroy;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

function TXRTLInputStreamToStreamAdapter.Read(var Buffer; Count: Integer): Integer;
begin
  Result:= FCoreStream.ReadBuffer(Buffer, Count);
  if Result < 0 then
    Result:= 0;
end;

function TXRTLInputStreamToStreamAdapter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise EXRTLSeekException.CreateFmt(SXRTLSeekExceptionFmt, [ClassName]);
end;

function TXRTLInputStreamToStreamAdapter.Write(const Buffer; Count: Integer): Integer;
begin
  raise EXRTLWriteException.CreateFmt(SXRTLWriteExceptionFmt, [ClassName]);
end;

{ TXRTLOutputStreamToStreamAdapter }

constructor TXRTLOutputStreamToStreamAdapter.Create(const ACoreStream: TXRTLOutputStream; AOwnCoreStream: Boolean = True);
begin
  inherited Create;
  FCoreStream:= ACoreStream;
  FOwnCoreStream:= AOwnCoreStream;
end;

destructor TXRTLOutputStreamToStreamAdapter.Destroy;
begin
  if FOwnCoreStream then
    FreeAndNil(FCoreStream);
  inherited;
end;

function TXRTLOutputStreamToStreamAdapter.Read(var Buffer; Count: Integer): Integer;
begin
  raise EXRTLReadException.CreateFmt(SXRTLReadExceptionFmt, [ClassName]);
end;

function TXRTLOutputStreamToStreamAdapter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise EXRTLSeekException.CreateFmt(SXRTLSeekExceptionFmt, [ClassName]);
end;

function TXRTLOutputStreamToStreamAdapter.Write(const Buffer; Count: Integer): Integer;
begin
  FCoreStream.WriteBuffer(Buffer, Count);
  Result:= Count;
end;

end.

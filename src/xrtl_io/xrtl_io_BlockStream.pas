unit xrtl_io_BlockStream;

{$INCLUDE xrtl.inc}

interface

uses
  Math,
  xrtl_util_CPUUtils,
  xrtl_io_Stream;

const
  bsfNoFlags    = $00;
  bsfEOS        = $01;
  bsfData       = $02;
  bsfSync       = $04;
  bsfLengthMask = $C0;

const
  XRTLDefaultBlockSize = 4096;

type
  TXRTLBlockInputStream = class(TXRTLFilterInputStream)
  private
    FBlockFlags: Byte;
    FBlockSize: Integer;
    procedure  ReadBlockHeader;
    function   SkipTo(const Flags: Byte): Int64;
  protected
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const ACoreStream: TXRTLInputStream;
                       AOwnCoreStream: Boolean = True);
    destructor Destroy; override;
    function   BytesAvailable: Int64; override;
    function   MarkPosition: TXRTLMarkData; override;
    procedure  RestorePosition(const MarkData: TXRTLMarkData); override;
    function   Skip(const Count: Int64): Int64; override;
    function   SkipToSync: Int64;
    function   SkipToEndOfStream: Int64;
  end;

  TXRTLBlockOutputStream = class(TXRTLFilterOutputStream)
  private
    FBlockData: Pointer;
    FBlockSize: Integer;
    FWritePointer: Integer;
    procedure  SetBlockSize(const Value: Integer);
    function   BytesInBlock: Integer;
  protected
    procedure  DoClose; override;
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
    procedure  DoFlush(const Flags: Byte);
  public
    constructor Create(const ACoreStream: TXRTLOutputStream;
                       AOwnCoreStream: Boolean = True;
                       const ABlockSize: Integer = XRTLDefaultBlockSize);
    destructor Destroy; override;
    property   BlockData: Pointer read FBlockData;
    property   BlockSize: Integer read FBlockSize write SetBlockSize;
    procedure  Flush; override;
    procedure  Synchronize;
  end;

implementation

uses
  xrtl_util_MemoryUtils;

type
  TXRTLBlockInputStreamMarkData = class(TXRTLMarkData)
  public
    BlockFlags: Byte;
    BlockSize: Integer;
  end;

{ TXRTLBlockInputStream }

constructor TXRTLBlockInputStream.Create(const ACoreStream: TXRTLInputStream;
  AOwnCoreStream: Boolean = True);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FBlockFlags:= bsfNoFlags;
  FBlockSize:= 0;
end;

destructor TXRTLBlockInputStream.Destroy;
begin
  inherited;
end;

function TXRTLBlockInputStream.BytesAvailable: Int64;
begin
  Result:= FBlockSize;
end;

function TXRTLBlockInputStream.MarkPosition: TXRTLMarkData;
var
  MD: TXRTLBlockInputStreamMarkData;
begin
  MD:= TXRTLBlockInputStreamMarkData.Create(Self, inherited MarkPosition);
  MD.BlockFlags:= FBlockFlags;
  MD.BlockSize:= FBlockSize;
  Result:= MD;
end;

procedure TXRTLBlockInputStream.RestorePosition(const MarkData: TXRTLMarkData);
var
  MD: TXRTLBlockInputStreamMarkData;
begin
  MarkData.CheckOwner(Self);
  inherited RestorePosition(MarkData.Next);
  MD:= MarkData as TXRTLBlockInputStreamMarkData;
  FBlockFlags:= MD.BlockFlags;
  FBlockSize:= MD.BlockSize;
end;

procedure TXRTLBlockInputStream.ReadBlockHeader;
var
  LLength: Integer;
begin
  FBlockSize:= 0;
  if (FBlockFlags and bsfEOS) = bsfEOS then
    Exit;
  ReadBufferFully(FBlockFlags, SizeOf(FBlockFlags));
  if (FBlockFlags and bsfData) = bsfData then
  begin
    LLength:= ((FBlockFlags and bsfLengthMask) shr 6) + 1;
    ReadBufferFully(FBlockSize, LLength);
  end;
end;

function TXRTLBlockInputStream._ReadBuffer(var Buffer; const Count: Integer): Integer;
var
  Step, RResult: Integer;
  InBuffer: Pointer;
begin
  Result:= 0;
  if Count = 0 then
    Exit;
  InBuffer:= @Buffer;
  while Result <> XRTLEndOfStreamValue do
  begin
    while FBlockSize <= 0 do
    begin
      if ((FBlockFlags and bsfEOS) = bsfEOS) and (FBlockSize = 0) then
      begin
        if Result <= 0 then
          Result:= XRTLEndOfStreamValue;
        Break;
      end;
      ReadBlockHeader;
    end;
    if FBlockSize > 0 then
    begin
      Step:= Min(Count - Result, FBlockSize);
      RResult:= inherited _ReadBuffer(InBuffer^, Step);
      if RResult > 0 then
      begin
        Inc(Result, RResult);
        InBuffer:= XRTLPointerAdd(InBuffer, RResult);
        if Result = Count then
          Break;
      end;
    end;
  end;
end;

function TXRTLBlockInputStream.Skip(const Count: Int64): Int64;
begin
  Result:= inherited _Skip(Count);
end;

function TXRTLBlockInputStream.SkipTo(const Flags: Byte): Int64;
var
  IsFlags: Boolean;
begin
  Result:= 0;
  repeat
    IsFlags:= (FBlockFlags and Flags) <> 0;
    Result:= Result + Skip(Max(1, FBlockSize));
  until IsFlags;
end;

function TXRTLBlockInputStream.SkipToSync: Int64;
begin
  Result:= SkipTo(bsfSync or bsfEOS);
end;

function TXRTLBlockInputStream.SkipToEndOfStream: Int64;
begin
  Result:= SkipTo(bsfEOS);
end;

{ TXRTLBlockOutputStream }

constructor TXRTLBlockOutputStream.Create(const ACoreStream: TXRTLOutputStream;
  AOwnCoreStream: Boolean = True; const ABlockSize: Integer = XRTLDefaultBlockSize);
begin
  inherited Create(ACoreStream, AOwnCoreStream);
  FBlockData:= nil;
  FWritePointer:= 0;
  BlockSize:= ABlockSize;
end;

destructor TXRTLBlockOutputStream.Destroy;
begin
  XRTLFreeMemory(FBlockData);
  inherited;
end;

function TXRTLBlockOutputStream.BytesInBlock: Integer;
begin
  Result:= FWritePointer;
end;

procedure TXRTLBlockOutputStream.SetBlockSize(const Value: Integer);
var
  ABlock: Pointer;
  ABlockSize: Integer;
begin
// check if new size is smaller than current data present in buffer
  if Value < BytesInBlock then
    Flush;
  ABlockSize:= Max(Value, 16);
  ABlock:= XRTLGetMemory(ABlockSize);
  if FBlockData <> nil then
  begin
    XRTLMoveMemory(FBlockData, ABlock, BytesInBlock);
    XRTLFreeMemory(FBlockData);
  end;
  FBlockData:= ABlock;
  FBlockSize:= ABlockSize;
end;

procedure TXRTLBlockOutputStream.Flush;
begin
  DoFlush(bsfNoFlags);
end;

procedure TXRTLBlockOutputStream._WriteBuffer(const Buffer; const Count: Integer);
var
  RCount, Step: Integer;
  InBuffer: Pointer;
begin
  RCount:= Count;
  InBuffer:= @Buffer;
  while RCount > 0 do
  begin
    Step:= Min(RCount, BlockSize - BytesInBlock);
    XRTLMoveMemory(InBuffer, XRTLPointerAdd(FBlockData, FWritePointer), Step);
    Inc(FWritePointer, Step);
    Dec(RCount, Step);
    InBuffer:= XRTLPointerAdd(InBuffer, Step);
    if BytesInBlock = BlockSize then
      Flush;
  end;
end;

procedure TXRTLBlockOutputStream.DoClose;
begin
  DoFlush(bsfEOS or bsfSync);
  inherited;
end;

procedure TXRTLBlockOutputStream.DoFlush(const Flags: Byte);

  function GetLengthSize(const Value: Integer): Byte;
  begin
    Result:= 1;
    if Value < $100 then
      Exit;
    Result:= 2;
    if Value < $10000 then
      Exit;
    Result:= 3;
    if Value < $1000000 then
      Exit;
    Result:= 4;
  end;

var
  LFlags, LSize, LBytesInBlock: Byte;
begin
  LFlags:= Flags;
  LBytesInBlock:= BytesInBlock;
  LSize:= GetLengthSize(LBytesInBlock);
  if LBytesInBlock > 0 then
    LFlags:= LFlags or bsfData or (((LSize - 1) and $03) shl 6);
  inherited _WriteBuffer(LFlags, SizeOf(LFlags));
  inherited _WriteBuffer(LBytesInBlock, LSize);
  if LBytesInBlock > 0 then
  begin
    inherited _WriteBuffer(FBlockData^, FWritePointer);
    FWritePointer:= 0;
  end;
end;

procedure TXRTLBlockOutputStream.Synchronize;
begin
  DoFlush(bsfSync);
end;

end.

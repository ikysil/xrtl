unit xrtl_crypt_MessageDigest;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_io_StreamProcessor;

type
  EXRTLMessageDigestException = class(EXRTLException);

  TXRTLMessageDigest = class(TXRTLStreamProcessor)
  private
  protected
    FBytes: PByteArray;
    FSize: Integer;
    function   GetBytes(Index: Integer): Byte;
    function   GetAsString: string;
    function   engineUpdate(var InBuffer: PByteArray;
                            var InAvail: Integer;
                            var OutBuffer: PByteArray;
                            var OutAvail: Integer;
                            const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    procedure  engineUpdateDigest(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation); virtual; abstract;
    procedure  engineInit; virtual; abstract;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function   IsFlushSupported: Boolean; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    property   Size: Integer read FSize;
    property   Bytes[Index: Integer]: Byte read GetBytes;
    property   AsString: string read GetAsString;
    procedure  Init;
  end;

function XRTLCompareMessageDigest(MD: TXRTLMessageDigest; Data: Pointer; Length: Integer): Boolean;

implementation

uses
  Windows, Math,
  xrtl_util_CPUUtils, xrtl_util_MemoryUtils, xrtl_util_MemoryManager,
  xrtl_io_ResourceStrings;

function XRTLCompareMessageDigest(MD: TXRTLMessageDigest; Data: Pointer; Length: Integer): Boolean;
begin
  Result:= CompareMem(MD.FBytes, Data, Min(Length, MD.Size));
end;

{ TXRTLMessageDigest }

constructor TXRTLMessageDigest.Create(ASize: Integer);
begin
  inherited Create;
  FBytes:= XRTLGetMemory(ASize, [gmoZeroMemory]);
  FSize:= ASize;
end;

destructor TXRTLMessageDigest.Destroy;
begin
  XRTLFreeMemory(Pointer(FBytes));
  inherited;
end;

function TXRTLMessageDigest.GetAsString: string;
var
  I: Integer;
  HS: string;
  CResult: PChar;
begin
  SetLength(Result, FSize * 2);
  CResult:= @Result[1];
  for I:= 0 to FSize - 1 do
  begin
    HS:= IntToHex(FBytes[I], 2);
    CResult[I * 2    ]:= HS[1];
    CResult[I * 2 + 1]:= HS[2];
  end;
end;

function TXRTLMessageDigest.GetBytes(Index: Integer): Byte;
begin
  if (Index < 0) or (Index >= FSize) then
    raise ERangeError.CreateFmt(SXRTLGetByteRange, [ClassName, 0, Size - 1])
  else
    Result:= FBytes[Index];
end;

function TXRTLMessageDigest.IsFlushSupported: Boolean;
begin
  Result:= True;
end;

function TXRTLMessageDigest.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= InputSize;
end;

function TXRTLMessageDigest.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= OutputSize;
end;

function TXRTLMessageDigest.engineUpdate(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LInAvail, LInAvailUpdated: Integer;
begin
  LInAvail:= Min(InAvail, OutAvail);
  LInAvailUpdated:= LInAvail;
  engineUpdateDigest(InBuffer, LInAvailUpdated, Operation);
  OutBuffer:= XRTLPointerAdd(InBuffer, LInAvail - LInAvailUpdated);
  OutAvail:= OutAvail - (LInAvail - LInAvailUpdated);
  Result:= Operation <> spoFinish; // can process data if not finish
  if Operation = spoFinish then
    SetState(spsClosed);
end;

procedure TXRTLMessageDigest.Init;
begin
  ZeroMemory(FBytes, FSize);
  engineInit;
  SetState(spsRunning);
end;

end.

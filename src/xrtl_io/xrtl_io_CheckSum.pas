unit xrtl_io_CheckSum;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Compat,
  xrtl_io_StreamProcessor;

type
  TXRTLCheckSum = class(TXRTLStreamProcessor)
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
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    procedure  engineReset; override;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function   IsFlushSupported: Boolean; override;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    property   Size: Integer read FSize;
    property   Bytes[Index: Integer]: Byte read GetBytes;
    property   AsString: string read GetAsString;
    procedure  CheckState; override;
  end;

  TXRTLCustomCheckSum8Bit = class(TXRTLCheckSum)
  private
  protected
    function   GetSum: Byte;
  public
    constructor Create;
    property   Sum: Byte read GetSum;
  end;

  TXRTLCustomCheckSum16Bit = class(TXRTLCheckSum)
  private
  protected
    function   GetSum: Word;
  public
    constructor Create;
    property   Sum: Word read GetSum;
  end;

  TXRTLCustomCheckSum32Bit = class(TXRTLCheckSum)
  private
  protected
    function   GetSum: Cardinal;
  public
    constructor Create;
    property   Sum: Cardinal read GetSum;
  end;

  TXRTLCheckSum8Bit = class(TXRTLCustomCheckSum8Bit)
  protected
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  end;
  
  TXRTLCheckSum16Bit = class(TXRTLCustomCheckSum16Bit)
  protected
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  end;

  TXRTLCheckSum32Bit = class(TXRTLCustomCheckSum32Bit)
  protected
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  end;

implementation

uses
  Windows, Math,
  xrtl_util_CPUUtils, xrtl_util_MemoryUtils, xrtl_util_MemoryManager,
  xrtl_io_ResourceStrings;

{ TXRTLCheckSum }

constructor TXRTLCheckSum.Create(ASize: Integer);
begin
  inherited Create;
  FBytes:= XRTLGetMemory(ASize, [gmoZeroMemory]);
  FSize:= ASize;
  Reset;
end;

destructor TXRTLCheckSum.Destroy;
begin
  XRTLFreeMemory(Pointer(FBytes));
  inherited;
end;

procedure TXRTLCheckSum.CheckState;
begin
// override default behaviour to DO NOT CHECK state
// in order to enable processing in spsUnknown state
end;

function TXRTLCheckSum.GetAsString: string;
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

function TXRTLCheckSum.GetBytes(Index: Integer): Byte;
begin
  if (Index < 0) or (Index >= FSize) then
    raise ERangeError.CreateFmt(SXRTLGetByteRange, [ClassName, 0, Size - 1])
  else
    Result:= FBytes[Index];
end;

function TXRTLCheckSum.IsFlushSupported: Boolean;
begin
  Result:= True;
end;

function TXRTLCheckSum.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= InputSize;
end;

function TXRTLCheckSum.GetInputSize(OutputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
begin
  Result:= OutputSize;
end;

procedure TXRTLCheckSum.engineReset;
begin
  inherited;
  ZeroMemory(FBytes, FSize);
end;

function TXRTLCheckSum.engineUpdate(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LInAvail, LInAvailUpdated: Integer;
begin
  LInAvail:= Min(InAvail, OutAvail);
  LInAvailUpdated:= LInAvail;
  Result:= engineUpdateSum(InBuffer, LInAvailUpdated, Operation);
  OutBuffer:= XRTLPointerAdd(InBuffer, LInAvail - LInAvailUpdated);
  OutAvail:= OutAvail - (LInAvail - LInAvailUpdated);
  if Operation = spoFinish then
    SetState(spsClosed);
end;

{ TXRTLCustomCheckSum8Bit }

constructor TXRTLCustomCheckSum8Bit.Create;
begin
  inherited Create(SizeOf(Byte));
end;

function TXRTLCustomCheckSum8Bit.GetSum: Byte;
begin
  Result:= PByte(FBytes)^;
end;

{ TXRTLCustomCheckSum16Bit }

constructor TXRTLCustomCheckSum16Bit.Create;
begin
  inherited Create(SizeOf(Word));
end;

function TXRTLCustomCheckSum16Bit.GetSum: Word;
begin
  Result:= PWord(FBytes)^;
end;

{ TXRTLCustomCheckSum32Bit }

constructor TXRTLCustomCheckSum32Bit.Create;
begin
  inherited Create(SizeOf(Cardinal));
end;

function TXRTLCustomCheckSum32Bit.GetSum: Cardinal;
begin
  Result:= PCardinal(FBytes)^;
end;

{ TXRTLCheckSum8Bit }

function TXRTLCheckSum8Bit.engineUpdateSum(var InBuffer: PByteArray;
  var InAvail: Integer; const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  FSum: PByte;
begin
  Result:= True;
  while InAvail > 0 do
  begin
{$R-,Q-}
    FSum:= @FBytes;
    FSum^:= FSum^ + InBuffer[0];
{$R+,Q+}
    InBuffer:= XRTLPointerAdd(InBuffer, SizeOf(Byte));
    Dec(InAvail);
  end;
end;

{ TXRTLCheckSum16Bit }

function TXRTLCheckSum16Bit.engineUpdateSum(var InBuffer: PByteArray;
  var InAvail: Integer; const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  FSum: PWord;
begin
  Result:= True;
  while InAvail > 0 do
  begin
{$R-,Q-}
    FSum:= @FBytes;
    FSum^:= FSum^ + InBuffer[0];
{$R+,Q+}
    InBuffer:= XRTLPointerAdd(InBuffer, SizeOf(Byte));
    Dec(InAvail);
  end;
end;

{ TXRTLCheckSum32Bit }

function TXRTLCheckSum32Bit.engineUpdateSum(var InBuffer: PByteArray;
  var InAvail: Integer; const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  FSum: PCardinal;
begin
  Result:= True;
  while InAvail > 0 do
  begin
{$R-,Q-}
    FSum:= @FBytes;
    FSum^:= FSum^ + InBuffer[0];
{$R+,Q+}
    InBuffer:= XRTLPointerAdd(InBuffer, SizeOf(Byte));
    Dec(InAvail);
  end;
end;

end.

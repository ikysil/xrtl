unit xrtl_crypt_Cipher;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_io_StreamProcessor,
  xrtl_crypt_CipherKey;

type
  EXRTLCipherException = class(EXRTLException);

  TXRTLCipher = class(TXRTLConvertingStreamProcessor)
  private
    FKey: PByteArray;
    FKeySize: Integer;
  protected
    procedure  engineReset; override;
    procedure  engineSetKey; virtual;
    function   engineUpdate(var InBuffer: PByteArray;
                            var InAvail: Integer;
                            var OutBuffer: PByteArray;
                            var OutAvail: Integer;
                            const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateEncipher(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    function   engineUpdateDecipher(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    procedure  engineInitEncipher; virtual; abstract;
    procedure  engineInitDecipher; virtual; abstract;
// size of key in BYTES
    function   engineGetMaximumKeySize: Integer; virtual; abstract;
    function   engineGetMinimumKeySize: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  InitEncipher;
    procedure  InitDecipher;
    procedure  SetKey(AKey: Pointer; AKeySize: Integer); overload;
    procedure  SetKey(const AKey: WideString); overload;
    procedure  SetKey(const AKey: TXRTLCipherKey); overload;
    property   Key: PByteArray read FKey;
    property   KeySize: Integer read FKeySize;
    function   GetMaximumKeySize: Integer;
    function   GetMinimumKeySize: Integer;
  end;

  TXRTLCipherClass = class of TXRTLCipher;

implementation

uses
  xrtl_util_MemoryManager, xrtl_util_MemoryUtils,
  xrtl_crypt_ResourceStrings;

{ TXRTLCipher }

constructor TXRTLCipher.Create;
begin
  inherited;             
  FKey:= nil;
  SetKey(nil, GetMinimumKeySize);
end;

destructor TXRTLCipher.Destroy;
begin
  XRTLFreeMemory(Pointer(FKey));
  inherited;
end;

procedure TXRTLCipher.engineReset;
begin
  inherited;
  FillChar(FKey^, GetMaximumKeySize, $FF);
  SetKey(nil, 0);
end;

procedure TXRTLCipher.engineSetKey;
begin
end;

function TXRTLCipher.GetMaximumKeySize: Integer;
begin
  Result:= engineGetMaximumKeySize;
end;

function TXRTLCipher.GetMinimumKeySize: Integer;
begin
  Result:= engineGetMinimumKeySize;
end;

function TXRTLCipher.engineGetMinimumKeySize: Integer;
begin
  Result:= 0;
end;

procedure TXRTLCipher.SetKey(AKey: Pointer; AKeySize: Integer);
begin
  if Assigned(AKey) and ((AKeySize > GetMaximumKeySize) or (AKeySize < GetMinimumKeySize)) then
    raise EXRTLCipherException.CreateFmt(SInvalidKeySize, [AKeySize, GetMinimumKeySize, GetMaximumKeySize]);
  XRTLGetMemory(Pointer(FKey), GetMaximumKeySize, [gmoZeroMemory]);
  FKeySize:= AKeySize;
  if Assigned(AKey) then
  begin
    XRTLMoveMemory(AKey, FKey, AKeySize);
  end;
  engineSetKey;
end;

procedure TXRTLCipher.SetKey(const AKey: WideString);
begin
  SetKey(@AKey[1], Length(AKey));
end;

procedure TXRTLCipher.SetKey(const AKey: TXRTLCipherKey);
begin
  SetKey(AKey.KeyData, AKey.KeySize);
end;

function TXRTLCipher.engineUpdate(var InBuffer: PByteArray; var InAvail: Integer;
  var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  case Mode of
    cspmDirect:  Result:= engineUpdateEncipher(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
    cspmReverse: Result:= engineUpdateDecipher(InBuffer, InAvail, OutBuffer, OutAvail, Operation);
  else
    raise EXRTLInvalidStreamProcessorState.CreateFmt('Invalid %s mode', [ClassName]);
  end;
  if Operation = spoFinish then
    SetState(spsClosed);
end;

procedure TXRTLCipher.InitEncipher;
begin
  SetState(spsUnknown);
  engineInitEncipher;
  SetMode(cspmDirect);
end;

procedure TXRTLCipher.InitDecipher;
begin
  SetState(spsUnknown);
  engineInitDecipher;
  SetMode(cspmReverse);
end;

end.

{ Abstract block ciphers, modes, and paddings.
  @author(Illya Kysil <ikysil at users.berlios.de>)
}
unit xrtl_crypt_BlockCipher;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception,
  xrtl_util_CPUUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_Cipher;

type
  EXRTLBlockCipherException        = class(EXRTLCipherException);
  EXRTLBlockCipherPaddingException = class(EXRTLBlockCipherException);
  EXRTLBlockCipherModeException    = class(EXRTLBlockCipherException);

  TXRTLBlockCipher = class;

  TXRTLBlockCipherModeOption  = (bcmoPaddingRequired);
  TXRTLBlockCipherModeOptions = set of TXRTLBlockCipherModeOption;

  TXRTLBlockCipherMode = class
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); virtual;
    procedure  Reset(const ACipher: TXRTLBlockCipher); virtual; abstract;
    function   GetOptions: TXRTLBlockCipherModeOptions; virtual;
    class function GetDisplayName: string; virtual;
  end;

  TXRTLBlockCipherEngine = class
  public
    function   engineUpdateEncipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
    function   engineUpdateDecipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; virtual; abstract;
  end;

  TXRTLBlockCipherPadding = class
  public
    procedure  enginePadBlock(const ACipher: TXRTLBlockCipher;
                              const Buffer: PByteArray;
                              var   Avail: Integer); virtual; abstract;
    procedure  engineUnPadBlock(const ACipher: TXRTLBlockCipher;
                                const Buffer: PByteArray;
                                var   Avail: Integer); virtual; abstract;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); virtual; abstract;
    function   GetMaximumPadSize(const ACipher: TXRTLBlockCipher): Integer; virtual; abstract;
    procedure  Reset(const ACipher: TXRTLBlockCipher); virtual; abstract;
    class function GetDisplayName: string; virtual;
  end;

  TXRTLBlockCipher = class(TXRTLCipher)
  private
    FOwnCipherMode: Boolean;
    FCipherMode: TXRTLBlockCipherMode;
    FOwnCipherPadding: Boolean;
    FCipherPadding: TXRTLBlockCipherPadding;
    FCipherEngine: TXRTLBlockCipherEngine;
    FDefaultCipherEngine: TXRTLBlockCipherEngine;
  protected
    procedure  engineInitEncipher; override;
    procedure  engineInitDecipher; override;
    procedure  engineReset; override;
    function   engineUpdateEncipher(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateDecipher(var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
// size of cipher block in BYTES
    function   engineGetBlockSize: Integer; virtual;
// size of plain text block in BYTES
// plain text block = cipher text block = cipher block size by default
    function   engineGetPlainTextBlockSize: Integer; virtual;
// size of cipher text block in BYTES
// plain text block = cipher text block = cipher block size by default
    function   engineGetCipherTextBlockSize: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property   CipherMode: TXRTLBlockCipherMode read FCipherMode;
    property   OwnCipherMode: Boolean read FOwnCipherMode write FOwnCipherMode;
    procedure  SetCipherMode(const Value: TXRTLBlockCipherMode; AOwnCipherMode: Boolean = True);
    property   CipherPadding: TXRTLBlockCipherPadding read FCipherPadding;
    property   OwnCipherPadding: Boolean read FOwnCipherPadding write FOwnCipherPadding;
    procedure  SetCipherPadding(const Value: TXRTLBlockCipherPadding; AOwnCipherPadding: Boolean = True);
    procedure  SetCipherEngine(const Value: TXRTLBlockCipherEngine);
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); virtual; abstract;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); virtual; abstract;
    function   GetInputBlockSize: Integer;
    function   GetOutputBlockSize: Integer;
    function   GetOutputSize(InputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   GetInputSize(OutputSize: Integer; const Operation: TXRTLStreamProcessorOperation): Integer; override;
    function   IsFlushSupported: Boolean; override;
    function   GetPlainTextBlockSize: Integer;
    function   GetCipherTextBlockSize: Integer;
    function   GetBlockSize: Integer;
  end;

implementation

uses
  xrtl_crypt_ResourceStrings, xrtl_crypt_BlockCipherEngine;

{ TXRTLBlockCipherMode }

procedure TXRTLBlockCipherMode.Initialize(const ACipher: TXRTLBlockCipher);
begin
  ACipher.SetCipherEngine(nil);
end;

class function TXRTLBlockCipherMode.GetDisplayName: string;
begin
  Result:= ClassName;
end;

function TXRTLBlockCipherMode.GetOptions: TXRTLBlockCipherModeOptions;
begin
  Result:= [bcmoPaddingRequired];
end;

{ TXRTLBlockCipherPadding }

class function TXRTLBlockCipherPadding.GetDisplayName: string;
begin
  Result:= ClassName;
end;

{ TXRTLBlockCipher }

constructor TXRTLBlockCipher.Create;
begin
  inherited Create;
  FCipherMode:= nil;
  FOwnCipherMode:= False;
  FCipherPadding:= nil;
  FOwnCipherPadding:= False;
  FDefaultCipherEngine:= TXRTLDefaultBlockCipherEngine.Create;
  SetCipherEngine(nil);
end;

destructor TXRTLBlockCipher.Destroy;
begin
  FreeAndNil(FDefaultCipherEngine);
  if FOwnCipherMode then
    FreeAndNil(FCipherMode);
  if FOwnCipherPadding then
    FreeAndNil(FCipherPadding);
  inherited;
end;

function TXRTLBlockCipher.IsFlushSupported: Boolean;
begin
  Result:= True;
end;

procedure TXRTLBlockCipher.SetCipherEngine(const Value: TXRTLBlockCipherEngine);
begin
  if Assigned(Value) then
    FCipherEngine:= Value
  else
    FCipherEngine:= FDefaultCipherEngine;
end;

procedure TXRTLBlockCipher.SetCipherMode(const Value: TXRTLBlockCipherMode; AOwnCipherMode: Boolean = True);
begin
  if Value <> FCipherMode then
  begin
    Reset;
    if FOwnCipherMode then
      FreeAndNil(FCipherMode);
    FCipherMode:= Value;
  end;
  FOwnCipherMode:= AOwnCipherMode;
end;

procedure TXRTLBlockCipher.SetCipherPadding(const Value: TXRTLBlockCipherPadding; AOwnCipherPadding: Boolean = True);
begin
  if Value <> FCipherPadding then
  begin
    Reset;
    if FOwnCipherPadding then
      FreeAndNil(FCipherPadding);
    FCipherPadding:= Value;
  end;
  FOwnCipherPadding:= AOwnCipherPadding;
end;

function TXRTLBlockCipher.engineGetBlockSize: Integer;
begin
  Result:= -1;
  XRTLNotImplemented;
end;

function TXRTLBlockCipher.engineGetCipherTextBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

function TXRTLBlockCipher.engineGetPlainTextBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

function TXRTLBlockCipher.GetInputBlockSize: Integer;
begin
  CheckState;
  Result:= -1;
  case Mode of
    cspmDirect:  Result:= engineGetPlainTextBlockSize;
    cspmReverse: Result:= engineGetCipherTextBlockSize;
  end;
end;

function TXRTLBlockCipher.GetOutputBlockSize: Integer;
begin
  CheckState;
  Result:= -1;
  case Mode of
    cspmDirect:  Result:= engineGetCipherTextBlockSize;
    cspmReverse: Result:= engineGetPlainTextBlockSize;
  end;
end;

function TXRTLBlockCipher.GetInputSize(OutputSize: Integer;
 const Operation: TXRTLStreamProcessorOperation): Integer;
var
  LInputBlockSize: Integer;
  LOutputBlockSize: Integer;
begin
  LInputBlockSize:= GetInputBlockSize;
  LOutputBlockSize:= GetOutputBlockSize;
  Result:= OutputSize div LOutputBlockSize * LInputBlockSize;
  if (OutputSize mod LOutputBlockSize) <> 0 then
    Inc(Result, LInputBlockSize);
  if (Operation = spoFinish) and Assigned(FCipherPadding) then
    Inc(Result, FCipherPadding.GetMaximumPadSize(Self));
end;

function TXRTLBlockCipher.GetOutputSize(InputSize: Integer;
  const Operation: TXRTLStreamProcessorOperation): Integer;
var
  LInputBlockSize: Integer;
  LOutputBlockSize: Integer;
begin
  LInputBlockSize:= GetInputBlockSize;
  LOutputBlockSize:= GetOutputBlockSize;
  Result:= InputSize div LInputBlockSize * LOutputBlockSize;
  if (InputSize mod LInputBlockSize) <> 0 then
    Inc(Result, LOutputBlockSize);
  if (Operation = spoFinish) and Assigned(FCipherPadding) then
    Inc(Result, FCipherPadding.GetMaximumPadSize(Self));
end;

function TXRTLBlockCipher.engineUpdateEncipher(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LPBlockSize: Integer;
  PaddingRequired: Boolean;
begin
  LPBlockSize:= GetPlainTextBlockSize;
  if Operation = spoFinish then
  begin
    if Assigned(CipherPadding) then
    begin
      CipherPadding.enginePadBlock(Self, InBuffer, InAvail);
      if (InAvail mod LPBlockSize) <> 0 then
        raise EXRTLBlockCipherException.CreateFmt(SInvalidCipherBlockSize, [InAvail, LPBlockSize]);
    end
    else
    begin
//  PaddingRequired = True if CipherMode is nil (assuming ECB mode) or
//  if CipherMode requires padding
      PaddingRequired:= not Assigned(CipherMode) or
                        (Assigned(CipherMode) and (bcmoPaddingRequired in CipherMode.GetOptions));
      if (InAvail > 0) and PaddingRequired then
        raise EXRTLBlockCipherException.CreateFmt(SInvalidCipherBlockSize, [InAvail, LPBlockSize]);
    end;
  end;
  Result:= FCipherEngine.engineUpdateEncipher(Self, InBuffer, InAvail, OutBuffer, OutAvail, Operation);
end;

function TXRTLBlockCipher.engineUpdateDecipher(var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LCBlockSize: Integer;
  LOutBuffer: Pointer;
  LOutAvail, LFinalOutAvail: Integer;
begin
  LCBlockSize:= GetCipherTextBlockSize;
  LOutBuffer:= OutBuffer;
  LOutAvail:= OutAvail;
  Result:= FCipherEngine.engineUpdateDecipher(Self, InBuffer, InAvail, OutBuffer, OutAvail, Operation);
  if Operation = spoFinish then
  begin
    if InAvail > 0 then
      raise EXRTLBlockCipherException.CreateFmt(SInvalidCipherBlockSize, [InAvail, LCBlockSize]);
    if Assigned(CipherPadding) then
    begin
      LFinalOutAvail:= LOutAvail - OutAvail;
      CipherPadding.engineUnPadBlock(Self, LOutBuffer, LFinalOutAvail);
      OutBuffer:= XRTLPointerAdd(LOutBuffer, LFinalOutAvail);
      OutAvail:= LOutAvail - LFinalOutAvail;
    end;
  end;
end;

procedure TXRTLBlockCipher.engineInitEncipher;
begin
  inherited;
  if Assigned(FCipherPadding) then
    FCipherPadding.Initialize(Self);
  if Assigned(FCipherMode) then
    FCipherMode.Initialize(Self);
end;

procedure TXRTLBlockCipher.engineInitDecipher;
begin
  inherited;
  if Assigned(FCipherPadding) then
    FCipherPadding.Initialize(Self);
  if Assigned(FCipherMode) then
    FCipherMode.Initialize(Self);
end;

procedure TXRTLBlockCipher.engineReset;
begin
  inherited;
  SetCipherEngine(nil);
  if Assigned(FCipherPadding) then
    FCipherPadding.Reset(Self);
  if Assigned(FCipherMode) then
    FCipherMode.Reset(Self);
end;

function TXRTLBlockCipher.GetCipherTextBlockSize: Integer;
begin
  Result:= engineGetCipherTextBlockSize;
end;

function TXRTLBlockCipher.GetPlainTextBlockSize: Integer;
begin
  Result:= engineGetPlainTextBlockSize;
end;

function TXRTLBlockCipher.GetBlockSize: Integer;
begin
  Result:= engineGetBlockSize;
end;

end.

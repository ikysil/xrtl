unit xrtl_crypt_BlockCipherMode;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Math,
  xrtl_util_MemoryUtils, xrtl_util_MemoryManager,
  xrtl_crypt_BlockCipher;

type
  TXRTLECBCipherMode = class(TXRTLBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); override;
    procedure  Reset(const ACipher: TXRTLBlockCipher); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLChainBlockCipherMode = class(TXRTLBlockCipherMode)
  private
    FChainCount: Integer;
    FInitVector: PByteArray;
    FInitVectorSize: Integer;
    FChain: array of PByteArray;
    FChainLength: Integer;
    FCipherEngine: TXRTLBlockCipherEngine;
    procedure  FreeChainData;
    procedure  InitChainData(const ABlockSize: Integer);
    procedure  CheckBlockSize(const ACipher: TXRTLBlockCipher); virtual;
  protected
    function   CreateCipherEngine: TXRTLBlockCipherEngine; virtual;
    function   GetChainCount: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  AfterConstruction; override;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); override;
    procedure  SetInitVector(AInitVector: Pointer; AInitVectorSize: Integer); overload;
    procedure  SetInitVector(const AInitVector: string); overload;
    property   InitVector: PByteArray read FInitVector;
    property   InitVectorSize: Integer read FInitVectorSize;
    procedure  Reset(const ACipher: TXRTLBlockCipher); override;
  end;

  TXRTLCBCCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLCFB8BitCipherMode = class(TXRTLChainBlockCipherMode)
  protected
    function   CreateCipherEngine: TXRTLBlockCipherEngine; override;
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    function   GetOptions: TXRTLBlockCipherModeOptions; override;
    class function GetDisplayName: string; override;
  end;

  TXRTLCFBBlockCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLOFB8BitCipherMode = class(TXRTLChainBlockCipherMode)
  protected
    function   CreateCipherEngine: TXRTLBlockCipherEngine; override;
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    function   GetOptions: TXRTLBlockCipherModeOptions; override;
    class function GetDisplayName: string; override;
  end;

  TXRTLOFBBlockCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLBCCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLPCBCCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLCBCCCipherMode = class(TXRTLChainBlockCipherMode)
  protected
  public
    procedure  engineUpdateEncipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipher(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipherEngine;

{ TXRTLECBCipherMode }

procedure TXRTLECBCipherMode.Initialize(const ACipher: TXRTLBlockCipher);
begin
  inherited;
end;

procedure TXRTLECBCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer);
end;

procedure TXRTLECBCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer);
end;

procedure TXRTLECBCipherMode.Reset(const ACipher: TXRTLBlockCipher);
begin
  inherited;
  ACipher.SetCipherEngine(nil);
end;

class function TXRTLECBCipherMode.GetDisplayName: string;
begin
  Result:= 'ECB';
end;

{ TXRTLChainBlockCipherMode }

constructor TXRTLChainBlockCipherMode.Create;
begin
  inherited Create;
  FInitVector:= nil;
  FInitVectorSize:= 0;
  FChain:= nil;
  FChainLength:= 0;
  FCipherEngine:= nil;
end;

destructor TXRTLChainBlockCipherMode.Destroy;
begin
  FreeAndNil(FCipherEngine);
  FreeChainData;
  XRTLFreeMemory(Pointer(FInitVector));
  inherited;
end;

procedure TXRTLChainBlockCipherMode.AfterConstruction;
begin
  inherited;
  FChainCount:= Max(0, GetChainCount);
  FCipherEngine:= CreateCipherEngine;
end;

function TXRTLChainBlockCipherMode.CreateCipherEngine: TXRTLBlockCipherEngine;
begin
  Result:= TXRTLBlockModeBlockCipherEngine.Create;
end;

function TXRTLChainBlockCipherMode.GetChainCount: Integer;
begin
  Result:= 1;
end;

procedure TXRTLChainBlockCipherMode.InitChainData(const ABlockSize: Integer);
var
  I: Integer;
begin
  SetLength(FChain, FChainCount);
  for I:= 0 to High(FChain) do
    FChain[I]:= XRTLGetMemory(ABlockSize * SizeOf(Byte), [gmoZeroMemory]);
end;

procedure TXRTLChainBlockCipherMode.FreeChainData;
var
  I: Integer;
begin
  for I:= 0 to High(FChain) do
    XRTLFreeMemory(Pointer(FChain[I]));
  SetLength(FChain, 0);
end;

procedure TXRTLChainBlockCipherMode.CheckBlockSize(const ACipher: TXRTLBlockCipher);
begin
  if ACipher.GetPlainTextBlockSize <> ACipher.GetCipherTextBlockSize then
    raise EXRTLBlockCipherModeException.Create('Ciphers with different plain text and cipher text sizes are not supported.');
end;

procedure TXRTLChainBlockCipherMode.Initialize(const ACipher: TXRTLBlockCipher);
begin
  inherited;
  CheckBlockSize(ACipher);
  FreeChainData;
  FChainLength:= Max(ACipher.GetPlainTextBlockSize, ACipher.GetCipherTextBlockSize);
  InitChainData(FChainLength);
  XRTLMoveMemory(FInitVector, FChain[0], Min(FChainLength, FInitVectorSize));
  ACipher.SetCipherEngine(FCipherEngine);
end;

procedure TXRTLChainBlockCipherMode.SetInitVector(AInitVector: Pointer;
  AInitVectorSize: Integer);
begin
  XRTLGetMemory(Pointer(FInitVector), AInitVectorSize, [gmoZeroMemory]);
  FInitVectorSize:= AInitVectorSize;
  if Assigned(AInitVector) then
  begin
    XRTLMoveMemory(AInitVector, FInitVector, AInitVectorSize);
  end;
end;

procedure TXRTLChainBlockCipherMode.SetInitVector(const AInitVector: string);
begin
  SetInitVector(@AInitVector[1], Length(AInitVector));
end;

procedure TXRTLChainBlockCipherMode.Reset(const ACipher: TXRTLBlockCipher);
begin
  inherited;
  ACipher.SetCipherEngine(nil);
  FreeChainData;
  XRTLFreeMemory(Pointer(FInitVector));
end;

{ TXRTLCBCCipherMode }

procedure TXRTLCBCCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  ACipher.engineUpdateEncipherBlockECB(OutBuffer, OutBuffer);
  XRTLMoveMemory(OutBuffer, FChain[0], ACipher.GetBlockSize);
end;

procedure TXRTLCBCCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer);
  XRTLXorBlock(OutBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLMoveMemory(InBuffer, FChain[0], ACipher.GetBlockSize);
end;

class function TXRTLCBCCipherMode.GetDisplayName: string;
begin
  Result:= 'CBC';
end;

{ TXRTLCFB8BitCipherMode }

function TXRTLCFB8BitCipherMode.CreateCipherEngine: TXRTLBlockCipherEngine;
begin
  Result:= TXRTL8BitModeBlockCipherEngine.Create;
end;

procedure TXRTLCFB8BitCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  OutBuffer[0]:= InBuffer[0] xor FChain[0, FChainLength - 1];
  XRTLMoveMemory(@FChain[0, 0], @FChain[0, 1], FChainLength - 1);
  FChain[0, 0]:= OutBuffer[0];
end;

procedure TXRTLCFB8BitCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  OutBuffer[0]:= InBuffer[0] xor FChain[0, FChainLength - 1];
  XRTLMoveMemory(@FChain[0, 0], @FChain[0, 1], FChainLength - 1);
  FChain[0, 0]:= InBuffer[0];
end;

function TXRTLCFB8BitCipherMode.GetOptions: TXRTLBlockCipherModeOptions;
begin
  Result:= [];
end;

class function TXRTLCFB8BitCipherMode.GetDisplayName: string;
begin
  Result:= 'CFB8Bit';
end;

{ TXRTLCFBBlockCipherMode }

procedure TXRTLCFBBlockCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLMoveMemory(OutBuffer, FChain[0], ACipher.GetBlockSize);
end;

procedure TXRTLCFBBlockCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLMoveMemory(InBuffer, FChain[0], ACipher.GetBlockSize);
end;

class function TXRTLCFBBlockCipherMode.GetDisplayName: string;
begin
  Result:= 'CFBBlock';
end;

{ TXRTLOFB8BitCipherMode }

function TXRTLOFB8BitCipherMode.CreateCipherEngine: TXRTLBlockCipherEngine;
begin
  Result:= TXRTL8BitModeBlockCipherEngine.Create;
end;

procedure TXRTLOFB8BitCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  OutBuffer[0]:= InBuffer[0] xor FChain[0, FChainLength - 1];
end;

procedure TXRTLOFB8BitCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  OutBuffer[0]:= InBuffer[0] xor FChain[0, FChainLength - 1];
end;

function TXRTLOFB8BitCipherMode.GetOptions: TXRTLBlockCipherModeOptions;
begin
  Result:= [];
end;

class function TXRTLOFB8BitCipherMode.GetDisplayName: string;
begin
  Result:= 'OFB8Bit';
end;

{ TXRTLOFBBlockCipherMode }

procedure TXRTLOFBBlockCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
end;

procedure TXRTLOFBBlockCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(FChain[0], FChain[0]);
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
end;

class function TXRTLOFBBlockCipherMode.GetDisplayName: string;
begin
  Result:= 'OFBBlock';
end;

{ TXRTLBCCipherMode }

procedure TXRTLBCCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  ACipher.engineUpdateEncipherBlockECB(OutBuffer, OutBuffer);
  XRTLXorBlock(FChain[0], OutBuffer, FChain[0], ACipher.GetBlockSize);
end;

procedure TXRTLBCCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer);
  XRTLXorBlock(OutBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLXorBlock(FChain[0], InBuffer, FChain[0], ACipher.GetBlockSize);
end;

class function TXRTLBCCipherMode.GetDisplayName: string;
begin
  Result:= 'BC';
end;

{ TXRTLPCBCCipherMode }

procedure TXRTLPCBCCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  ACipher.engineUpdateEncipherBlockECB(OutBuffer, OutBuffer);
  XRTLXorBlock(InBuffer, OutBuffer, FChain[0], ACipher.GetBlockSize);
end;

procedure TXRTLPCBCCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer);
  XRTLXorBlock(OutBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLXorBlock(InBuffer, OutBuffer, FChain[0], ACipher.GetBlockSize);
end;

class function TXRTLPCBCCipherMode.GetDisplayName: string;
begin
  Result:= 'PCBC';
end;

{ TXRTLCBCCCipherMode }

procedure TXRTLCBCCCipherMode.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  XRTLXorBlock(InBuffer, FChain[0], FChain[0], ACipher.GetBlockSize);
  XRTLMoveMemory(FChain[0], OutBuffer, ACipher.GetBlockSize);
  ACipher.engineUpdateEncipherBlockECB(OutBuffer, OutBuffer);
end;

procedure TXRTLCBCCCipherMode.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, InBuffer);
  XRTLXorBlock(InBuffer, FChain[0], OutBuffer, ACipher.GetBlockSize);
  XRTLMoveMemory(InBuffer, FChain[0], ACipher.GetBlockSize);
end;

class function TXRTLCBCCCipherMode.GetDisplayName: string;
begin
  Result:= 'CBCC';
end;

end.

unit xrtl_crypt_BlockCipherEngine;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, 
  xrtl_util_CPUUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_BlockCipher;

type
  TXRTLCustomBlockCipherEngine = class(TXRTLBlockCipherEngine)
  public
    function   engineUpdateEncipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateDecipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    procedure  engineUpdateEncipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
    procedure  engineUpdateDecipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
  end;

  TXRTLDefaultBlockCipherEngine = class(TXRTLCustomBlockCipherEngine)
  public
    procedure  engineUpdateEncipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
  end;

  TXRTLBlockModeBlockCipherEngine = class(TXRTLCustomBlockCipherEngine)
  public
    procedure  engineUpdateEncipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
  end;

  TXRTLCustom8BitModeBlockCipherEngine = class(TXRTLBlockCipherEngine)
  public
    function   engineUpdateEncipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    function   engineUpdateDecipher(const ACipher: TXRTLBlockCipher;
                                    var InBuffer: PByteArray;
                                    var InAvail: Integer;
                                    var OutBuffer: PByteArray;
                                    var OutAvail: Integer;
                                    const Operation: TXRTLStreamProcessorOperation): Boolean; override;
    procedure  engineUpdateEncipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
    procedure  engineUpdateDecipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); virtual; abstract;
  end;

  TXRTL8BitModeBlockCipherEngine = class(TXRTLCustom8BitModeBlockCipherEngine)
  public
    procedure  engineUpdateEncipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlock(const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray); override;
  end;

implementation

{ TXRTLCustomBlockCipherEngine }

function TXRTLCustomBlockCipherEngine.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LPBlockSize, LCBlockSize: Integer;
begin
  LPBlockSize:= ACipher.GetPlainTextBlockSize;
  LCBlockSize:= ACipher.GetCipherTextBlockSize;
  while (InAvail >= LPBlockSize) and (OutAvail >= LCBlockSize) do
  begin
    engineUpdateEncipherBlock(ACipher, InBuffer, OutBuffer);
    Dec(InAvail, LPBlockSize);
    Dec(OutAvail, LCBlockSize);
    InBuffer:=  XRTLPointerAdd(InBuffer, LPBlockSize);
    OutBuffer:= XRTLPointerAdd(OutBuffer, LCBlockSize);
  end;
  Result:= (InAvail >= LPBlockSize);
end;

function TXRTLCustomBlockCipherEngine.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  LPBlockSize, LCBlockSize: Integer;
begin
  LPBlockSize:= ACipher.GetPlainTextBlockSize;
  LCBlockSize:= ACipher.GetCipherTextBlockSize;
  while (InAvail >= LCBlockSize) and (OutAvail >= LPBlockSize) do
  begin
    engineUpdateDecipherBlock(ACipher, InBuffer, OutBuffer);
    Dec(InAvail, LCBlockSize);
    Dec(OutAvail, LPBlockSize);
    InBuffer:=  XRTLPointerAdd(InBuffer, LCBlockSize);
    OutBuffer:= XRTLPointerAdd(OutBuffer, LPBlockSize);
  end;
  Result:= (InAvail >= LCBlockSize);
end;

{ TXRTLDefaultBlockCipherEngine }

procedure TXRTLDefaultBlockCipherEngine.engineUpdateEncipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer);
end;

procedure TXRTLDefaultBlockCipherEngine.engineUpdateDecipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer);
end;

{ TXRTLBlockModeBlockCipherEngine }

procedure TXRTLBlockModeBlockCipherEngine.engineUpdateEncipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.CipherMode.engineUpdateEncipher(ACipher, InBuffer, OutBuffer);
end;

procedure TXRTLBlockModeBlockCipherEngine.engineUpdateDecipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.CipherMode.engineUpdateDecipher(ACipher, InBuffer, OutBuffer);
end;

{ TXRTLCustom8BitModeBlockCipherEngine }

function TXRTLCustom8BitModeBlockCipherEngine.engineUpdateEncipher(
  const ACipher: TXRTLBlockCipher; var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail >= 1) and (OutAvail >= 1) do
  begin
    engineUpdateEncipherBlock(ACipher, InBuffer, OutBuffer);
    Dec(InAvail);
    Dec(OutAvail);
    InBuffer:=  XRTLPointerAdd(InBuffer, 1);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 1);
  end;
  Result:= (InAvail >= 1);
end;

function TXRTLCustom8BitModeBlockCipherEngine.engineUpdateDecipher(
  const ACipher: TXRTLBlockCipher; var InBuffer: PByteArray;
  var InAvail: Integer; var OutBuffer: PByteArray; var OutAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation): Boolean;
begin
  while (InAvail >= 1) and (OutAvail >= 1) do
  begin
    engineUpdateDecipherBlock(ACipher, InBuffer, OutBuffer);
    Dec(InAvail, 1);
    Dec(OutAvail, 1);
    InBuffer:=  XRTLPointerAdd(InBuffer, 1);
    OutBuffer:= XRTLPointerAdd(OutBuffer, 1);
  end;
  Result:= (InAvail >= 1);
end;

{ TXRTL8BitModeBlockCipherEngine }

procedure TXRTL8BitModeBlockCipherEngine.engineUpdateEncipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.CipherMode.engineUpdateEncipher(ACipher, InBuffer, OutBuffer);
end;

procedure TXRTL8BitModeBlockCipherEngine.engineUpdateDecipherBlock(
  const ACipher: TXRTLBlockCipher; InBuffer, OutBuffer: PByteArray);
begin
  ACipher.CipherMode.engineUpdateDecipher(ACipher, InBuffer, OutBuffer);
end;

end.

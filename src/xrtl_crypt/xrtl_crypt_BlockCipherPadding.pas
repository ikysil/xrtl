unit xrtl_crypt_BlockCipherPadding;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipher;

type
  TXRTLOneAndZeroesBlockCipherPadding = class(TXRTLBlockCipherPadding)
  protected
  public
    procedure  enginePadBlock(const ACipher: TXRTLBlockCipher;
                              const Buffer: PByteArray;
                              var   Avail: Integer); override;
    procedure  engineUnPadBlock(const ACipher: TXRTLBlockCipher;
                                const Buffer: PByteArray;
                                var   Avail: Integer); override;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); override;
    function   GetMaximumPadSize(const ACipher: TXRTLBlockCipher): Integer; override;
    procedure  Reset(const ACipher: TXRTLBlockCipher); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLPKCS5BlockCipherPadding = class(TXRTLBlockCipherPadding)
  protected
  public
    procedure  enginePadBlock(const ACipher: TXRTLBlockCipher;
                              const Buffer: PByteArray;
                              var   Avail: Integer); override;
    procedure  engineUnPadBlock(const ACipher: TXRTLBlockCipher;
                                const Buffer: PByteArray;
                                var   Avail: Integer); override;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); override;
    function   GetMaximumPadSize(const ACipher: TXRTLBlockCipher): Integer; override;
    procedure  Reset(const ACipher: TXRTLBlockCipher); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLPKCS7BlockCipherPadding = class(TXRTLBlockCipherPadding)
  protected
  public
    procedure  enginePadBlock(const ACipher: TXRTLBlockCipher;
                              const Buffer: PByteArray;
                              var   Avail: Integer); override;
    procedure  engineUnPadBlock(const ACipher: TXRTLBlockCipher;
                                const Buffer: PByteArray;
                                var   Avail: Integer); override;
    procedure  Initialize(const ACipher: TXRTLBlockCipher); override;
    function   GetMaximumPadSize(const ACipher: TXRTLBlockCipher): Integer; override;
    procedure  Reset(const ACipher: TXRTLBlockCipher); override;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  xrtl_crypt_ResourceStrings;

{ TXRTLOneAndZeroesBlockCipherPadding }

procedure TXRTLOneAndZeroesBlockCipherPadding.Initialize(const ACipher: TXRTLBlockCipher);
begin
  inherited;
end;

procedure TXRTLOneAndZeroesBlockCipherPadding.enginePadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
var
  LPadSize: Integer;
begin
  inherited;
  LPadSize:= ACipher.GetPlainTextBlockSize - Avail mod ACipher.GetPlainTextBlockSize;
  PByte(XRTLPointerAdd(Buffer, Avail))^:= $80;
  FillMemory(XRTLPointerAdd(Buffer, Avail + 1), LPadSize - 1, 0);
  Inc(Avail, LPadSize);
end;

procedure TXRTLOneAndZeroesBlockCipherPadding.engineUnPadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
begin
  while Avail > 0 do
  begin
    if PByte(XRTLPointerAdd(Buffer, Avail))^ <> $80 then
      Dec(Avail)
    else
      Break;
  end;
end;

function TXRTLOneAndZeroesBlockCipherPadding.GetMaximumPadSize(const ACipher: TXRTLBlockCipher): Integer;
begin
  Result:= ACipher.GetPlainTextBlockSize;
end;

procedure TXRTLOneAndZeroesBlockCipherPadding.Reset(const ACipher: TXRTLBlockCipher);
begin
  inherited;
end;

class function TXRTLOneAndZeroesBlockCipherPadding.GetDisplayName: string;
begin
  Result:= 'One and zeroes';
end;

{ TXRTLPKCS5BlockCipherPadding }

procedure TXRTLPKCS5BlockCipherPadding.Initialize(const ACipher: TXRTLBlockCipher);
begin
  inherited;
  if ACipher.GetPlainTextBlockSize <> 8 then
    raise EXRTLBlockCipherPaddingException.CreateFmt(SInvalidCipherBlockSize, [ACipher.GetPlainTextBlockSize, 8]);
end;

procedure TXRTLPKCS5BlockCipherPadding.enginePadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
var
  LPadSize: Integer;
begin
  inherited;
  LPadSize:= ACipher.GetPlainTextBlockSize - Avail mod ACipher.GetPlainTextBlockSize;
  FillMemory(XRTLPointerAdd(Buffer, Avail), LPadSize, LPadSize);
  Inc(Avail, LPadSize);
end;

procedure TXRTLPKCS5BlockCipherPadding.engineUnPadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
var
  LPadSize: Integer;
begin
  inherited;
  LPadSize:= PByte(XRTLPointerAdd(Buffer, Avail - 1))^;
  Dec(Avail, LPadSize);
end;

function TXRTLPKCS5BlockCipherPadding.GetMaximumPadSize(
  const ACipher: TXRTLBlockCipher): Integer;
begin
  Result:= 8;
end;

procedure TXRTLPKCS5BlockCipherPadding.Reset(const ACipher: TXRTLBlockCipher);
begin
  inherited;
end;

class function TXRTLPKCS5BlockCipherPadding.GetDisplayName: string;
begin
  Result:= 'PKCS#5';
end;

{ TXRTLPKCS7BlockCipherPadding }

procedure TXRTLPKCS7BlockCipherPadding.Initialize(const ACipher: TXRTLBlockCipher);
begin
  inherited;
  if ACipher.GetPlainTextBlockSize >= 256 then
    raise EXRTLBlockCipherPaddingException.CreateFmt(SInvalidCipherBlockSize, [ACipher.GetPlainTextBlockSize, 8]);
end;

procedure TXRTLPKCS7BlockCipherPadding.enginePadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
var
  LPadSize: Integer;
begin
  inherited;
  LPadSize:= ACipher.GetPlainTextBlockSize - Avail mod ACipher.GetPlainTextBlockSize;
  FillMemory(XRTLPointerAdd(Buffer, Avail), LPadSize, LPadSize);
  Inc(Avail, LPadSize);
end;

procedure TXRTLPKCS7BlockCipherPadding.engineUnPadBlock(
  const ACipher: TXRTLBlockCipher; const Buffer: PByteArray;
  var Avail: Integer);
var
  LPadSize: Integer;
begin
  inherited;
  LPadSize:= PByte(XRTLPointerAdd(Buffer, Avail - 1))^;
  Dec(Avail, LPadSize);
end;

function TXRTLPKCS7BlockCipherPadding.GetMaximumPadSize(
  const ACipher: TXRTLBlockCipher): Integer;
begin
  Result:= 256;
end;

procedure TXRTLPKCS7BlockCipherPadding.Reset(const ACipher: TXRTLBlockCipher);
begin
  inherited;
end;

class function TXRTLPKCS7BlockCipherPadding.GetDisplayName: string;
begin
  Result:= 'PKCS#7';
end;

end.

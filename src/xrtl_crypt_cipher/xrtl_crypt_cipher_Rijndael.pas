unit xrtl_crypt_cipher_Rijndael;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_MemoryUtils,
  xrtl_crypt_cipher_RijndaelBase;

type
  TXRTLRijndael128Cipher = class(TXRTLRijndaelCipher)
  public
    constructor Create;
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLRijndael160Cipher = class(TXRTLRijndaelCipher)
  private
  public
    constructor Create;
    procedure engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLRijndael192Cipher = class(TXRTLRijndaelCipher)
  public
    constructor Create;
    procedure engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLRijndael224Cipher = class(TXRTLRijndaelCipher)
  public
    constructor Create;
    procedure engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

  TXRTLRijndael256Cipher = class(TXRTLRijndaelCipher)
  public
    constructor Create;
    procedure engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  xrtl_crypt_cipher_RijndaelUtil;

{ TXRTLRijndael128Cipher }

constructor TXRTLRijndael128Cipher.Create;
begin
  inherited Create(rbs128Bits);
end;

procedure TXRTLRijndael128Cipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 4 * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    WTemp[0]:= WA[0] xor FEncKey[I, 0];
    WTemp[1]:= WA[1] xor FEncKey[I, 1];
    WTemp[2]:= WA[2] xor FEncKey[I, 2];
    WTemp[3]:= WA[3] xor FEncKey[I, 3];
    WA[0]:= WT1[Temp[ShiftMod[0, 0], 0]] xor
            WT2[Temp[ShiftMod[1, 0], 1]] xor
            WT3[Temp[ShiftMod[2, 0], 2]] xor
            WT4[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT1[Temp[ShiftMod[0, 1], 0]] xor
            WT2[Temp[ShiftMod[1, 1], 1]] xor
            WT3[Temp[ShiftMod[2, 1], 2]] xor
            WT4[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT1[Temp[ShiftMod[0, 2], 0]] xor
            WT2[Temp[ShiftMod[1, 2], 1]] xor
            WT3[Temp[ShiftMod[2, 2], 2]] xor
            WT4[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT1[Temp[ShiftMod[0, 3], 0]] xor
            WT2[Temp[ShiftMod[1, 3], 1]] xor
            WT3[Temp[ShiftMod[2, 3], 2]] xor
            WT4[Temp[ShiftMod[3, 3], 3]];
  end;
  WTemp[0]:= WA[0] xor FEncKey[Nr - 1, 0];
  WTemp[1]:= WA[1] xor FEncKey[Nr - 1, 1];
  WTemp[2]:= WA[2] xor FEncKey[Nr - 1, 2];
  WTemp[3]:= WA[3] xor FEncKey[Nr - 1, 3];
  A[0, 0]:= Sd[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Sd[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Sd[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Sd[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Sd[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Sd[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Sd[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Sd[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Sd[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Sd[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Sd[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Sd[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Sd[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Sd[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Sd[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Sd[Temp[ShiftMod[3, 3], 3]];
  WA[0]:= WA[0] xor FEncKey[Nr, 0];
  WA[1]:= WA[1] xor FEncKey[Nr, 1];
  WA[2]:= WA[2] xor FEncKey[Nr, 2];
  WA[3]:= WA[3] xor FEncKey[Nr, 3];
  XRTLMoveMemory(@A, OutBuffer, 4 * SizeOf(Cardinal));
end;

procedure TXRTLRijndael128Cipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 4 * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    WTemp[0]:= WA[0] xor FDecKey[I, 0];
    WTemp[1]:= WA[1] xor FDecKey[I, 1];
    WTemp[2]:= WA[2] xor FDecKey[I, 2];
    WTemp[3]:= WA[3] xor FDecKey[I, 3];
    WA[0]:= WT5[Temp[ShiftMod[0, 0], 0]] xor
            WT6[Temp[ShiftMod[1, 0], 1]] xor
            WT7[Temp[ShiftMod[2, 0], 2]] xor
            WT8[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT5[Temp[ShiftMod[0, 1], 0]] xor
            WT6[Temp[ShiftMod[1, 1], 1]] xor
            WT7[Temp[ShiftMod[2, 1], 2]] xor
            WT8[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT5[Temp[ShiftMod[0, 2], 0]] xor
            WT6[Temp[ShiftMod[1, 2], 1]] xor
            WT7[Temp[ShiftMod[2, 2], 2]] xor
            WT8[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT5[Temp[ShiftMod[0, 3], 0]] xor
            WT6[Temp[ShiftMod[1, 3], 1]] xor
            WT7[Temp[ShiftMod[2, 3], 2]] xor
            WT8[Temp[ShiftMod[3, 3], 3]];
  end;
  WTemp[0]:= WA[0] xor FDecKey[1, 0];
  WTemp[1]:= WA[1] xor FDecKey[1, 1];
  WTemp[2]:= WA[2] xor FDecKey[1, 2];
  WTemp[3]:= WA[3] xor FDecKey[1, 3];
  A[0, 0]:= Si[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Si[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Si[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Si[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Si[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Si[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Si[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Si[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Si[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Si[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Si[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Si[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Si[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Si[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Si[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Si[Temp[ShiftMod[3, 3], 3]];
  WA[0]:= WA[0] xor FDecKey[0, 0];
  WA[1]:= WA[1] xor FDecKey[0, 1];
  WA[2]:= WA[2] xor FDecKey[0, 2];
  WA[3]:= WA[3] xor FDecKey[0, 3];
  XRTLMoveMemory(@A, OutBuffer, 4 * SizeOf(Cardinal));
end;

class function TXRTLRijndael128Cipher.GetDisplayName: string;
begin
  Result:= inherited GetDisplayName + ' (128)';
end;

{ TXRTLRijndael160Cipher }

constructor TXRTLRijndael160Cipher.Create;
begin
  inherited Create(rbs160Bits);
end;

procedure TXRTLRijndael160Cipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 5 * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    WTemp[0]:= WA[0] xor FEncKey[I, 0];
    WTemp[1]:= WA[1] xor FEncKey[I, 1];
    WTemp[2]:= WA[2] xor FEncKey[I, 2];
    WTemp[3]:= WA[3] xor FEncKey[I, 3];
    WTemp[4]:= WA[4] xor FEncKey[I, 4];
    WA[0]:= WT1[Temp[ShiftMod[0, 0], 0]] xor
            WT2[Temp[ShiftMod[1, 0], 1]] xor
            WT3[Temp[ShiftMod[2, 0], 2]] xor
            WT4[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT1[Temp[ShiftMod[0, 1], 0]] xor
            WT2[Temp[ShiftMod[1, 1], 1]] xor
            WT3[Temp[ShiftMod[2, 1], 2]] xor
            WT4[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT1[Temp[ShiftMod[0, 2], 0]] xor
            WT2[Temp[ShiftMod[1, 2], 1]] xor
            WT3[Temp[ShiftMod[2, 2], 2]] xor
            WT4[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT1[Temp[ShiftMod[0, 3], 0]] xor
            WT2[Temp[ShiftMod[1, 3], 1]] xor
            WT3[Temp[ShiftMod[2, 3], 2]] xor
            WT4[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT1[Temp[ShiftMod[0, 4], 0]] xor
            WT2[Temp[ShiftMod[1, 4], 1]] xor
            WT3[Temp[ShiftMod[2, 4], 2]] xor
            WT4[Temp[ShiftMod[3, 4], 3]];
  end;
  WTemp[0]:= WA[0] xor FEncKey[Nr - 1, 0];
  WTemp[1]:= WA[1] xor FEncKey[Nr - 1, 1];
  WTemp[2]:= WA[2] xor FEncKey[Nr - 1, 2];
  WTemp[3]:= WA[3] xor FEncKey[Nr - 1, 3];
  WTemp[4]:= WA[4] xor FEncKey[Nr - 1, 4];
  A[0, 0]:= Sd[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Sd[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Sd[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Sd[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Sd[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Sd[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Sd[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Sd[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Sd[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Sd[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Sd[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Sd[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Sd[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Sd[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Sd[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Sd[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Sd[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Sd[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Sd[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Sd[Temp[ShiftMod[3, 4], 3]];
  WA[0]:= WA[0] xor FEncKey[Nr, 0];
  WA[1]:= WA[1] xor FEncKey[Nr, 1];
  WA[2]:= WA[2] xor FEncKey[Nr, 2];
  WA[3]:= WA[3] xor FEncKey[Nr, 3];
  WA[4]:= WA[4] xor FEncKey[Nr, 4];
  XRTLMoveMemory(@A, OutBuffer, 5 * SizeOf(Cardinal));
end;

procedure TXRTLRijndael160Cipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 5 * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    WTemp[0]:= WA[0] xor FDecKey[I, 0];
    WTemp[1]:= WA[1] xor FDecKey[I, 1];
    WTemp[2]:= WA[2] xor FDecKey[I, 2];
    WTemp[3]:= WA[3] xor FDecKey[I, 3];
    WTemp[4]:= WA[4] xor FDecKey[I, 4];
    WA[0]:= WT5[Temp[ShiftMod[0, 0], 0]] xor
            WT6[Temp[ShiftMod[1, 0], 1]] xor
            WT7[Temp[ShiftMod[2, 0], 2]] xor
            WT8[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT5[Temp[ShiftMod[0, 1], 0]] xor
            WT6[Temp[ShiftMod[1, 1], 1]] xor
            WT7[Temp[ShiftMod[2, 1], 2]] xor
            WT8[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT5[Temp[ShiftMod[0, 2], 0]] xor
            WT6[Temp[ShiftMod[1, 2], 1]] xor
            WT7[Temp[ShiftMod[2, 2], 2]] xor
            WT8[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT5[Temp[ShiftMod[0, 3], 0]] xor
            WT6[Temp[ShiftMod[1, 3], 1]] xor
            WT7[Temp[ShiftMod[2, 3], 2]] xor
            WT8[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT5[Temp[ShiftMod[0, 4], 0]] xor
            WT6[Temp[ShiftMod[1, 4], 1]] xor
            WT7[Temp[ShiftMod[2, 4], 2]] xor
            WT8[Temp[ShiftMod[3, 4], 3]];
  end;
  WTemp[0]:= WA[0] xor FDecKey[1, 0];
  WTemp[1]:= WA[1] xor FDecKey[1, 1];
  WTemp[2]:= WA[2] xor FDecKey[1, 2];
  WTemp[3]:= WA[3] xor FDecKey[1, 3];
  WTemp[4]:= WA[4] xor FDecKey[1, 4];
  A[0, 0]:= Si[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Si[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Si[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Si[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Si[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Si[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Si[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Si[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Si[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Si[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Si[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Si[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Si[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Si[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Si[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Si[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Si[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Si[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Si[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Si[Temp[ShiftMod[3, 4], 3]];
  WA[0]:= WA[0] xor FDecKey[0, 0];
  WA[1]:= WA[1] xor FDecKey[0, 1];
  WA[2]:= WA[2] xor FDecKey[0, 2];
  WA[3]:= WA[3] xor FDecKey[0, 3];
  WA[4]:= WA[4] xor FDecKey[0, 4];
  XRTLMoveMemory(@A, OutBuffer, 5 * SizeOf(Cardinal));
end;

class function TXRTLRijndael160Cipher.GetDisplayName: string;
begin
  Result:= inherited GetDisplayName + ' (160)';
end;

{ TXRTLRijndael192Cipher }

constructor TXRTLRijndael192Cipher.Create;
begin
  inherited Create(rbs192Bits);
end;

procedure TXRTLRijndael192Cipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 6 * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    WTemp[0]:= WA[0] xor FEncKey[I, 0];
    WTemp[1]:= WA[1] xor FEncKey[I, 1];
    WTemp[2]:= WA[2] xor FEncKey[I, 2];
    WTemp[3]:= WA[3] xor FEncKey[I, 3];
    WTemp[4]:= WA[4] xor FEncKey[I, 4];
    WTemp[5]:= WA[5] xor FEncKey[I, 5];
    WA[0]:= WT1[Temp[ShiftMod[0, 0], 0]] xor
            WT2[Temp[ShiftMod[1, 0], 1]] xor
            WT3[Temp[ShiftMod[2, 0], 2]] xor
            WT4[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT1[Temp[ShiftMod[0, 1], 0]] xor
            WT2[Temp[ShiftMod[1, 1], 1]] xor
            WT3[Temp[ShiftMod[2, 1], 2]] xor
            WT4[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT1[Temp[ShiftMod[0, 2], 0]] xor
            WT2[Temp[ShiftMod[1, 2], 1]] xor
            WT3[Temp[ShiftMod[2, 2], 2]] xor
            WT4[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT1[Temp[ShiftMod[0, 3], 0]] xor
            WT2[Temp[ShiftMod[1, 3], 1]] xor
            WT3[Temp[ShiftMod[2, 3], 2]] xor
            WT4[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT1[Temp[ShiftMod[0, 4], 0]] xor
            WT2[Temp[ShiftMod[1, 4], 1]] xor
            WT3[Temp[ShiftMod[2, 4], 2]] xor
            WT4[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT1[Temp[ShiftMod[0, 5], 0]] xor
            WT2[Temp[ShiftMod[1, 5], 1]] xor
            WT3[Temp[ShiftMod[2, 5], 2]] xor
            WT4[Temp[ShiftMod[3, 5], 3]];
  end;
  WTemp[0]:= WA[0] xor FEncKey[Nr - 1, 0];
  WTemp[1]:= WA[1] xor FEncKey[Nr - 1, 1];
  WTemp[2]:= WA[2] xor FEncKey[Nr - 1, 2];
  WTemp[3]:= WA[3] xor FEncKey[Nr - 1, 3];
  WTemp[4]:= WA[4] xor FEncKey[Nr - 1, 4];
  WTemp[5]:= WA[5] xor FEncKey[Nr - 1, 5];
  A[0, 0]:= Sd[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Sd[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Sd[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Sd[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Sd[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Sd[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Sd[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Sd[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Sd[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Sd[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Sd[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Sd[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Sd[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Sd[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Sd[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Sd[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Sd[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Sd[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Sd[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Sd[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Sd[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Sd[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Sd[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Sd[Temp[ShiftMod[3, 5], 3]];
  WA[0]:= WA[0] xor FEncKey[Nr, 0];
  WA[1]:= WA[1] xor FEncKey[Nr, 1];
  WA[2]:= WA[2] xor FEncKey[Nr, 2];
  WA[3]:= WA[3] xor FEncKey[Nr, 3];
  WA[4]:= WA[4] xor FEncKey[Nr, 4];
  WA[5]:= WA[5] xor FEncKey[Nr, 5];
  XRTLMoveMemory(@A, OutBuffer, 6 * SizeOf(Cardinal));
end;

procedure TXRTLRijndael192Cipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 6 * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    WTemp[0]:= WA[0] xor FDecKey[I, 0];
    WTemp[1]:= WA[1] xor FDecKey[I, 1];
    WTemp[2]:= WA[2] xor FDecKey[I, 2];
    WTemp[3]:= WA[3] xor FDecKey[I, 3];
    WTemp[4]:= WA[4] xor FDecKey[I, 4];
    WTemp[5]:= WA[5] xor FDecKey[I, 5];
    WA[0]:= WT5[Temp[ShiftMod[0, 0], 0]] xor
            WT6[Temp[ShiftMod[1, 0], 1]] xor
            WT7[Temp[ShiftMod[2, 0], 2]] xor
            WT8[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT5[Temp[ShiftMod[0, 1], 0]] xor
            WT6[Temp[ShiftMod[1, 1], 1]] xor
            WT7[Temp[ShiftMod[2, 1], 2]] xor
            WT8[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT5[Temp[ShiftMod[0, 2], 0]] xor
            WT6[Temp[ShiftMod[1, 2], 1]] xor
            WT7[Temp[ShiftMod[2, 2], 2]] xor
            WT8[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT5[Temp[ShiftMod[0, 3], 0]] xor
            WT6[Temp[ShiftMod[1, 3], 1]] xor
            WT7[Temp[ShiftMod[2, 3], 2]] xor
            WT8[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT5[Temp[ShiftMod[0, 4], 0]] xor
            WT6[Temp[ShiftMod[1, 4], 1]] xor
            WT7[Temp[ShiftMod[2, 4], 2]] xor
            WT8[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT5[Temp[ShiftMod[0, 5], 0]] xor
            WT6[Temp[ShiftMod[1, 5], 1]] xor
            WT7[Temp[ShiftMod[2, 5], 2]] xor
            WT8[Temp[ShiftMod[3, 5], 3]];
  end;
  WTemp[0]:= WA[0] xor FDecKey[1, 0];
  WTemp[1]:= WA[1] xor FDecKey[1, 1];
  WTemp[2]:= WA[2] xor FDecKey[1, 2];
  WTemp[3]:= WA[3] xor FDecKey[1, 3];
  WTemp[4]:= WA[4] xor FDecKey[1, 4];
  WTemp[5]:= WA[5] xor FDecKey[1, 5];
  A[0, 0]:= Si[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Si[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Si[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Si[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Si[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Si[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Si[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Si[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Si[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Si[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Si[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Si[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Si[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Si[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Si[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Si[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Si[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Si[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Si[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Si[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Si[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Si[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Si[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Si[Temp[ShiftMod[3, 5], 3]];
  WA[0]:= WA[0] xor FDecKey[0, 0];
  WA[1]:= WA[1] xor FDecKey[0, 1];
  WA[2]:= WA[2] xor FDecKey[0, 2];
  WA[3]:= WA[3] xor FDecKey[0, 3];
  WA[4]:= WA[4] xor FDecKey[0, 4];
  WA[5]:= WA[5] xor FDecKey[0, 5];
  XRTLMoveMemory(@A, OutBuffer, 6 * SizeOf(Cardinal));
end;

class function TXRTLRijndael192Cipher.GetDisplayName: string;
begin
  Result:= inherited GetDisplayName + ' (192)';
end;

{ TXRTLRijndael224Cipher }

constructor TXRTLRijndael224Cipher.Create;
begin
  inherited Create(rbs224Bits);
end;

procedure TXRTLRijndael224Cipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 7 * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    WTemp[0]:= WA[0] xor FEncKey[I, 0];
    WTemp[1]:= WA[1] xor FEncKey[I, 1];
    WTemp[2]:= WA[2] xor FEncKey[I, 2];
    WTemp[3]:= WA[3] xor FEncKey[I, 3];
    WTemp[4]:= WA[4] xor FEncKey[I, 4];
    WTemp[5]:= WA[5] xor FEncKey[I, 5];
    WTemp[6]:= WA[6] xor FEncKey[I, 6];
    WA[0]:= WT1[Temp[ShiftMod[0, 0], 0]] xor
            WT2[Temp[ShiftMod[1, 0], 1]] xor
            WT3[Temp[ShiftMod[2, 0], 2]] xor
            WT4[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT1[Temp[ShiftMod[0, 1], 0]] xor
            WT2[Temp[ShiftMod[1, 1], 1]] xor
            WT3[Temp[ShiftMod[2, 1], 2]] xor
            WT4[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT1[Temp[ShiftMod[0, 2], 0]] xor
            WT2[Temp[ShiftMod[1, 2], 1]] xor
            WT3[Temp[ShiftMod[2, 2], 2]] xor
            WT4[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT1[Temp[ShiftMod[0, 3], 0]] xor
            WT2[Temp[ShiftMod[1, 3], 1]] xor
            WT3[Temp[ShiftMod[2, 3], 2]] xor
            WT4[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT1[Temp[ShiftMod[0, 4], 0]] xor
            WT2[Temp[ShiftMod[1, 4], 1]] xor
            WT3[Temp[ShiftMod[2, 4], 2]] xor
            WT4[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT1[Temp[ShiftMod[0, 5], 0]] xor
            WT2[Temp[ShiftMod[1, 5], 1]] xor
            WT3[Temp[ShiftMod[2, 5], 2]] xor
            WT4[Temp[ShiftMod[3, 5], 3]];
    WA[6]:= WT1[Temp[ShiftMod[0, 6], 0]] xor
            WT2[Temp[ShiftMod[1, 6], 1]] xor
            WT3[Temp[ShiftMod[2, 6], 2]] xor
            WT4[Temp[ShiftMod[3, 6], 3]];
  end;
  WTemp[0]:= WA[0] xor FEncKey[Nr - 1, 0];
  WTemp[1]:= WA[1] xor FEncKey[Nr - 1, 1];
  WTemp[2]:= WA[2] xor FEncKey[Nr - 1, 2];
  WTemp[3]:= WA[3] xor FEncKey[Nr - 1, 3];
  WTemp[4]:= WA[4] xor FEncKey[Nr - 1, 4];
  WTemp[5]:= WA[5] xor FEncKey[Nr - 1, 5];
  WTemp[6]:= WA[6] xor FEncKey[Nr - 1, 6];
  A[0, 0]:= Sd[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Sd[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Sd[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Sd[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Sd[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Sd[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Sd[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Sd[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Sd[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Sd[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Sd[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Sd[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Sd[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Sd[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Sd[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Sd[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Sd[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Sd[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Sd[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Sd[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Sd[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Sd[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Sd[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Sd[Temp[ShiftMod[3, 5], 3]];
  A[6, 0]:= Sd[Temp[ShiftMod[0, 6], 0]];
  A[6, 1]:= Sd[Temp[ShiftMod[1, 6], 1]];
  A[6, 2]:= Sd[Temp[ShiftMod[2, 6], 2]];
  A[6, 3]:= Sd[Temp[ShiftMod[3, 6], 3]];
  WA[0]:= WA[0] xor FEncKey[Nr, 0];
  WA[1]:= WA[1] xor FEncKey[Nr, 1];
  WA[2]:= WA[2] xor FEncKey[Nr, 2];
  WA[3]:= WA[3] xor FEncKey[Nr, 3];
  WA[4]:= WA[4] xor FEncKey[Nr, 4];
  WA[5]:= WA[5] xor FEncKey[Nr, 5];
  WA[6]:= WA[6] xor FEncKey[Nr, 6];
  XRTLMoveMemory(@A, OutBuffer, 7 * SizeOf(Cardinal));
end;

procedure TXRTLRijndael224Cipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 7 * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    WTemp[0]:= WA[0] xor FDecKey[I, 0];
    WTemp[1]:= WA[1] xor FDecKey[I, 1];
    WTemp[2]:= WA[2] xor FDecKey[I, 2];
    WTemp[3]:= WA[3] xor FDecKey[I, 3];
    WTemp[4]:= WA[4] xor FDecKey[I, 4];
    WTemp[5]:= WA[5] xor FDecKey[I, 5];
    WTemp[6]:= WA[6] xor FDecKey[I, 6];
    WA[0]:= WT5[Temp[ShiftMod[0, 0], 0]] xor
            WT6[Temp[ShiftMod[1, 0], 1]] xor
            WT7[Temp[ShiftMod[2, 0], 2]] xor
            WT8[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT5[Temp[ShiftMod[0, 1], 0]] xor
            WT6[Temp[ShiftMod[1, 1], 1]] xor
            WT7[Temp[ShiftMod[2, 1], 2]] xor
            WT8[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT5[Temp[ShiftMod[0, 2], 0]] xor
            WT6[Temp[ShiftMod[1, 2], 1]] xor
            WT7[Temp[ShiftMod[2, 2], 2]] xor
            WT8[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT5[Temp[ShiftMod[0, 3], 0]] xor
            WT6[Temp[ShiftMod[1, 3], 1]] xor
            WT7[Temp[ShiftMod[2, 3], 2]] xor
            WT8[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT5[Temp[ShiftMod[0, 4], 0]] xor
            WT6[Temp[ShiftMod[1, 4], 1]] xor
            WT7[Temp[ShiftMod[2, 4], 2]] xor
            WT8[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT5[Temp[ShiftMod[0, 5], 0]] xor
            WT6[Temp[ShiftMod[1, 5], 1]] xor
            WT7[Temp[ShiftMod[2, 5], 2]] xor
            WT8[Temp[ShiftMod[3, 5], 3]];
    WA[6]:= WT5[Temp[ShiftMod[0, 6], 0]] xor
            WT6[Temp[ShiftMod[1, 6], 1]] xor
            WT7[Temp[ShiftMod[2, 6], 2]] xor
            WT8[Temp[ShiftMod[3, 6], 3]];
  end;
  WTemp[0]:= WA[0] xor FDecKey[1, 0];
  WTemp[1]:= WA[1] xor FDecKey[1, 1];
  WTemp[2]:= WA[2] xor FDecKey[1, 2];
  WTemp[3]:= WA[3] xor FDecKey[1, 3];
  WTemp[4]:= WA[4] xor FDecKey[1, 4];
  WTemp[5]:= WA[5] xor FDecKey[1, 5];
  WTemp[6]:= WA[6] xor FDecKey[1, 6];
  A[0, 0]:= Si[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Si[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Si[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Si[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Si[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Si[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Si[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Si[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Si[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Si[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Si[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Si[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Si[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Si[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Si[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Si[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Si[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Si[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Si[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Si[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Si[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Si[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Si[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Si[Temp[ShiftMod[3, 5], 3]];
  A[6, 0]:= Si[Temp[ShiftMod[0, 6], 0]];
  A[6, 1]:= Si[Temp[ShiftMod[1, 6], 1]];
  A[6, 2]:= Si[Temp[ShiftMod[2, 6], 2]];
  A[6, 3]:= Si[Temp[ShiftMod[3, 6], 3]];
  WA[0]:= WA[0] xor FDecKey[0, 0];
  WA[1]:= WA[1] xor FDecKey[0, 1];
  WA[2]:= WA[2] xor FDecKey[0, 2];
  WA[3]:= WA[3] xor FDecKey[0, 3];
  WA[4]:= WA[4] xor FDecKey[0, 4];
  WA[5]:= WA[5] xor FDecKey[0, 5];
  WA[6]:= WA[6] xor FDecKey[0, 6];
  XRTLMoveMemory(@A, OutBuffer, 7 * SizeOf(Cardinal));
end;

class function TXRTLRijndael224Cipher.GetDisplayName: string;
begin
  Result:= inherited GetDisplayName + ' (224)';
end;

{ TXRTLRijndael256Cipher }

constructor TXRTLRijndael256Cipher.Create;
begin
  inherited Create(rbs256Bits);
end;

procedure TXRTLRijndael256Cipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 8 * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    WTemp[0]:= WA[0] xor FEncKey[I, 0];
    WTemp[1]:= WA[1] xor FEncKey[I, 1];
    WTemp[2]:= WA[2] xor FEncKey[I, 2];
    WTemp[3]:= WA[3] xor FEncKey[I, 3];
    WTemp[4]:= WA[4] xor FEncKey[I, 4];
    WTemp[5]:= WA[5] xor FEncKey[I, 5];
    WTemp[6]:= WA[6] xor FEncKey[I, 6];
    WTemp[7]:= WA[7] xor FEncKey[I, 7];
    WA[0]:= WT1[Temp[ShiftMod[0, 0], 0]] xor
            WT2[Temp[ShiftMod[1, 0], 1]] xor
            WT3[Temp[ShiftMod[2, 0], 2]] xor
            WT4[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT1[Temp[ShiftMod[0, 1], 0]] xor
            WT2[Temp[ShiftMod[1, 1], 1]] xor
            WT3[Temp[ShiftMod[2, 1], 2]] xor
            WT4[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT1[Temp[ShiftMod[0, 2], 0]] xor
            WT2[Temp[ShiftMod[1, 2], 1]] xor
            WT3[Temp[ShiftMod[2, 2], 2]] xor
            WT4[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT1[Temp[ShiftMod[0, 3], 0]] xor
            WT2[Temp[ShiftMod[1, 3], 1]] xor
            WT3[Temp[ShiftMod[2, 3], 2]] xor
            WT4[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT1[Temp[ShiftMod[0, 4], 0]] xor
            WT2[Temp[ShiftMod[1, 4], 1]] xor
            WT3[Temp[ShiftMod[2, 4], 2]] xor
            WT4[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT1[Temp[ShiftMod[0, 5], 0]] xor
            WT2[Temp[ShiftMod[1, 5], 1]] xor
            WT3[Temp[ShiftMod[2, 5], 2]] xor
            WT4[Temp[ShiftMod[3, 5], 3]];
    WA[6]:= WT1[Temp[ShiftMod[0, 6], 0]] xor
            WT2[Temp[ShiftMod[1, 6], 1]] xor
            WT3[Temp[ShiftMod[2, 6], 2]] xor
            WT4[Temp[ShiftMod[3, 6], 3]];
    WA[7]:= WT1[Temp[ShiftMod[0, 7], 0]] xor
            WT2[Temp[ShiftMod[1, 7], 1]] xor
            WT3[Temp[ShiftMod[2, 7], 2]] xor
            WT4[Temp[ShiftMod[3, 7], 3]];
  end;
  WTemp[0]:= WA[0] xor FEncKey[Nr - 1, 0];
  WTemp[1]:= WA[1] xor FEncKey[Nr - 1, 1];
  WTemp[2]:= WA[2] xor FEncKey[Nr - 1, 2];
  WTemp[3]:= WA[3] xor FEncKey[Nr - 1, 3];
  WTemp[4]:= WA[4] xor FEncKey[Nr - 1, 4];
  WTemp[5]:= WA[5] xor FEncKey[Nr - 1, 5];
  WTemp[6]:= WA[6] xor FEncKey[Nr - 1, 6];
  WTemp[7]:= WA[7] xor FEncKey[Nr - 1, 7];
  A[0, 0]:= Sd[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Sd[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Sd[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Sd[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Sd[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Sd[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Sd[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Sd[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Sd[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Sd[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Sd[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Sd[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Sd[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Sd[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Sd[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Sd[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Sd[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Sd[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Sd[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Sd[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Sd[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Sd[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Sd[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Sd[Temp[ShiftMod[3, 5], 3]];
  A[6, 0]:= Sd[Temp[ShiftMod[0, 6], 0]];
  A[6, 1]:= Sd[Temp[ShiftMod[1, 6], 1]];
  A[6, 2]:= Sd[Temp[ShiftMod[2, 6], 2]];
  A[6, 3]:= Sd[Temp[ShiftMod[3, 6], 3]];
  A[7, 0]:= Sd[Temp[ShiftMod[0, 7], 0]];
  A[7, 1]:= Sd[Temp[ShiftMod[1, 7], 1]];
  A[7, 2]:= Sd[Temp[ShiftMod[2, 7], 2]];
  A[7, 3]:= Sd[Temp[ShiftMod[3, 7], 3]];
  WA[0]:= WA[0] xor FEncKey[Nr, 0];
  WA[1]:= WA[1] xor FEncKey[Nr, 1];
  WA[2]:= WA[2] xor FEncKey[Nr, 2];
  WA[3]:= WA[3] xor FEncKey[Nr, 3];
  WA[4]:= WA[4] xor FEncKey[Nr, 4];
  WA[5]:= WA[5] xor FEncKey[Nr, 5];
  WA[6]:= WA[6] xor FEncKey[Nr, 6];
  WA[7]:= WA[7] xor FEncKey[Nr, 7];
  XRTLMoveMemory(@A, OutBuffer, 8 * SizeOf(Cardinal));
end;

procedure TXRTLRijndael256Cipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, 8 * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    WTemp[0]:= WA[0] xor FDecKey[I, 0];
    WTemp[1]:= WA[1] xor FDecKey[I, 1];
    WTemp[2]:= WA[2] xor FDecKey[I, 2];
    WTemp[3]:= WA[3] xor FDecKey[I, 3];
    WTemp[4]:= WA[4] xor FDecKey[I, 4];
    WTemp[5]:= WA[5] xor FDecKey[I, 5];
    WTemp[6]:= WA[6] xor FDecKey[I, 6];
    WTemp[7]:= WA[7] xor FDecKey[I, 7];
    WA[0]:= WT5[Temp[ShiftMod[0, 0], 0]] xor
            WT6[Temp[ShiftMod[1, 0], 1]] xor
            WT7[Temp[ShiftMod[2, 0], 2]] xor
            WT8[Temp[ShiftMod[3, 0], 3]];
    WA[1]:= WT5[Temp[ShiftMod[0, 1], 0]] xor
            WT6[Temp[ShiftMod[1, 1], 1]] xor
            WT7[Temp[ShiftMod[2, 1], 2]] xor
            WT8[Temp[ShiftMod[3, 1], 3]];
    WA[2]:= WT5[Temp[ShiftMod[0, 2], 0]] xor
            WT6[Temp[ShiftMod[1, 2], 1]] xor
            WT7[Temp[ShiftMod[2, 2], 2]] xor
            WT8[Temp[ShiftMod[3, 2], 3]];
    WA[3]:= WT5[Temp[ShiftMod[0, 3], 0]] xor
            WT6[Temp[ShiftMod[1, 3], 1]] xor
            WT7[Temp[ShiftMod[2, 3], 2]] xor
            WT8[Temp[ShiftMod[3, 3], 3]];
    WA[4]:= WT5[Temp[ShiftMod[0, 4], 0]] xor
            WT6[Temp[ShiftMod[1, 4], 1]] xor
            WT7[Temp[ShiftMod[2, 4], 2]] xor
            WT8[Temp[ShiftMod[3, 4], 3]];
    WA[5]:= WT5[Temp[ShiftMod[0, 5], 0]] xor
            WT6[Temp[ShiftMod[1, 5], 1]] xor
            WT7[Temp[ShiftMod[2, 5], 2]] xor
            WT8[Temp[ShiftMod[3, 5], 3]];
    WA[6]:= WT5[Temp[ShiftMod[0, 6], 0]] xor
            WT6[Temp[ShiftMod[1, 6], 1]] xor
            WT7[Temp[ShiftMod[2, 6], 2]] xor
            WT8[Temp[ShiftMod[3, 6], 3]];
    WA[7]:= WT5[Temp[ShiftMod[0, 7], 0]] xor
            WT6[Temp[ShiftMod[1, 7], 1]] xor
            WT7[Temp[ShiftMod[2, 7], 2]] xor
            WT8[Temp[ShiftMod[3, 7], 3]];
  end;
  WTemp[0]:= WA[0] xor FDecKey[1, 0];
  WTemp[1]:= WA[1] xor FDecKey[1, 1];
  WTemp[2]:= WA[2] xor FDecKey[1, 2];
  WTemp[3]:= WA[3] xor FDecKey[1, 3];
  WTemp[4]:= WA[4] xor FDecKey[1, 4];
  WTemp[5]:= WA[5] xor FDecKey[1, 5];
  WTemp[6]:= WA[6] xor FDecKey[1, 6];
  WTemp[7]:= WA[7] xor FDecKey[1, 7];
  A[0, 0]:= Si[Temp[ShiftMod[0, 0], 0]];
  A[0, 1]:= Si[Temp[ShiftMod[1, 0], 1]];
  A[0, 2]:= Si[Temp[ShiftMod[2, 0], 2]];
  A[0, 3]:= Si[Temp[ShiftMod[3, 0], 3]];
  A[1, 0]:= Si[Temp[ShiftMod[0, 1], 0]];
  A[1, 1]:= Si[Temp[ShiftMod[1, 1], 1]];
  A[1, 2]:= Si[Temp[ShiftMod[2, 1], 2]];
  A[1, 3]:= Si[Temp[ShiftMod[3, 1], 3]];
  A[2, 0]:= Si[Temp[ShiftMod[0, 2], 0]];
  A[2, 1]:= Si[Temp[ShiftMod[1, 2], 1]];
  A[2, 2]:= Si[Temp[ShiftMod[2, 2], 2]];
  A[2, 3]:= Si[Temp[ShiftMod[3, 2], 3]];
  A[3, 0]:= Si[Temp[ShiftMod[0, 3], 0]];
  A[3, 1]:= Si[Temp[ShiftMod[1, 3], 1]];
  A[3, 2]:= Si[Temp[ShiftMod[2, 3], 2]];
  A[3, 3]:= Si[Temp[ShiftMod[3, 3], 3]];
  A[4, 0]:= Si[Temp[ShiftMod[0, 4], 0]];
  A[4, 1]:= Si[Temp[ShiftMod[1, 4], 1]];
  A[4, 2]:= Si[Temp[ShiftMod[2, 4], 2]];
  A[4, 3]:= Si[Temp[ShiftMod[3, 4], 3]];
  A[5, 0]:= Si[Temp[ShiftMod[0, 5], 0]];
  A[5, 1]:= Si[Temp[ShiftMod[1, 5], 1]];
  A[5, 2]:= Si[Temp[ShiftMod[2, 5], 2]];
  A[5, 3]:= Si[Temp[ShiftMod[3, 5], 3]];
  A[6, 0]:= Si[Temp[ShiftMod[0, 6], 0]];
  A[6, 1]:= Si[Temp[ShiftMod[1, 6], 1]];
  A[6, 2]:= Si[Temp[ShiftMod[2, 6], 2]];
  A[6, 3]:= Si[Temp[ShiftMod[3, 6], 3]];
  A[7, 0]:= Si[Temp[ShiftMod[0, 7], 0]];
  A[7, 1]:= Si[Temp[ShiftMod[1, 7], 1]];
  A[7, 2]:= Si[Temp[ShiftMod[2, 7], 2]];
  A[7, 3]:= Si[Temp[ShiftMod[3, 7], 3]];
  WA[0]:= WA[0] xor FDecKey[0, 0];
  WA[1]:= WA[1] xor FDecKey[0, 1];
  WA[2]:= WA[2] xor FDecKey[0, 2];
  WA[3]:= WA[3] xor FDecKey[0, 3];
  WA[4]:= WA[4] xor FDecKey[0, 4];
  WA[5]:= WA[5] xor FDecKey[0, 5];
  WA[6]:= WA[6] xor FDecKey[0, 6];
  WA[7]:= WA[7] xor FDecKey[0, 7];
  XRTLMoveMemory(@A, OutBuffer, 8 * SizeOf(Cardinal));
end;

class function TXRTLRijndael256Cipher.GetDisplayName: string;
begin
  Result:= inherited GetDisplayName + ' (256)';
end;

end.

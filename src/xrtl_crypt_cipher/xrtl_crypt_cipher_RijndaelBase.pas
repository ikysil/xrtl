unit xrtl_crypt_cipher_RijndaelBase;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_crypt_BlockCipher;

const
  XRTLRijndaelMaxRounds = 14;
  XRTLRijndaelMaxNb     = 8;

type
  TXRTLRijndaelBlockSize = (rbs128Bits, rbs160Bits, rbs192Bits, rbs224Bits,
                            rbs256Bits);

  PXRTLRijndaelState = ^TXRTLRijndaelState;
  TXRTLRijndaelState = array[0 .. XRTLRijndaelMaxNb - 1, 0 .. 3] of Byte;

  PXRTLRijndaelWordState = ^TXRTLRijndaelWordState;
  TXRTLRijndaelWordState = array[0 .. XRTLRijndaelMaxNb - 1] of Cardinal;

  PXRTLRijndaelExpandedKey = ^TXRTLRijndaelExpandedKey;
  TXRTLRijndaelExpandedKey = array[0 .. XRTLRijndaelMaxRounds, 0 .. XRTLRijndaelMaxNb - 1] of Cardinal;

  TXRTLRijndaelCipher = class(TXRTLBlockCipher)
  private
  protected
    FBlockSize: TXRTLRijndaelBlockSize;
    Nk, Nb, Nr: Integer;
    FEncKey, FDecKey: TXRTLRijndaelExpandedKey;
    procedure  SetParameters;
// size of cipher block in BYTES
    function   engineGetBlockSize: Integer; override;
// size of key in BYTES
    function   engineGetMaximumKeySize: Integer; override;
    procedure  engineSetKey; override;
    procedure  engineInitEncipher; override;
    procedure  engineInitDecipher; override;
    procedure  engineReset; override;
  public
    constructor Create(const ABlockSize: TXRTLRijndaelBlockSize = rbs128Bits);
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  Math,
  xrtl_util_MemoryUtils, xrtl_util_CPUUtils,
  xrtl_crypt_cipher_RijndaelUtil;

{ TXRTLRijndaelCipher }

constructor TXRTLRijndaelCipher.Create(const ABlockSize: TXRTLRijndaelBlockSize = rbs128Bits);
begin
  inherited Create;
  FBlockSize:= ABlockSize;
end;

procedure TXRTLRijndaelCipher.engineReset;
begin
  inherited;
  FillChar(FEncKey, SizeOf(FEncKey), $FF);
  FillChar(FDecKey, SizeOf(FDecKey), $FF);
end;

procedure TXRTLRijndaelCipher.engineInitDecipher;
begin
  inherited;
  SetParameters;
end;

procedure TXRTLRijndaelCipher.engineInitEncipher;
begin
  inherited;
  SetParameters;
end;

procedure TXRTLRijndaelCipher.SetParameters;
begin
  Nb:= Min(XRTLRijndaelMaxNb, GetBlockSize div 4); // * 8 / 32
  Nk:= Max(KeySize, 16) div 4; // * 8 / 32
  Nr:= Max(Nk, Nb) + 6;
end;

function TXRTLRijndaelCipher.engineGetBlockSize: Integer;
begin
  Result:= RijndaelBlockSize[FBlockSize];
end;

function TXRTLRijndaelCipher.engineGetMaximumKeySize: Integer;
begin
  Result:= 32; // 256 bits
end;

procedure TXRTLRijndaelCipher.engineSetKey;

  function SubByte(Value: Cardinal): Cardinal;
  var
    VBytes, RBytes: PCardinalBytes;
  begin
    VBytes:= @Value;
    RBytes:= @Result;
    RBytes.Bytes[0]:= Sd[VBytes.Bytes[0]];
    RBytes.Bytes[1]:= Sd[VBytes.Bytes[1]];
    RBytes.Bytes[2]:= Sd[VBytes.Bytes[2]];
    RBytes.Bytes[3]:= Sd[VBytes.Bytes[3]];
  end;

var
  tk: TXRTLRijndaelState;
  WTK: PXRTLRijndaelWordState;
  PRK: PDWordArray;
  I: Integer;
  Temp: Cardinal;
begin
  SetParameters;
  WTK:= @TK;
  FillChar(FEncKey, Sizeof(FEncKey), 0);
  FillChar(FDecKey, Sizeof(FEncKey), 0);
  FillChar(tk, Sizeof(tk), 0);
  XRTLMoveMemory(Key, @tk, KeySize);
  PRK:= nil;
  try
    XRTLGetMemory(Pointer(PRK), (Nb * (Nr + 1) + Nk) * SizeOf(DWord));
    for I:= 0 to Nk - 1 do
    begin
      PRK[I]:= WTK[I];
    end;
    for I:= Nk to Nb * (Nr + 1) - 1 do
    begin
      Temp:= PRK[I - 1];
      if (I mod Nk) = 0 then
        Temp:= SubByte(XRTLROR32(Temp, 8)) xor Rcon[I div Nk - 1]
      else
        if ((I mod Nk) = 4) and (Nk > 6) then
          Temp:= SubByte(Temp);
      PRK[I]:= PRK[I - Nk] xor Temp;
    end;
    for I:= 0 to Nr do
    begin
      XRTLMoveMemory(@PRK[I * Nb], @FEncKey[I, 0], Nb * 4);
    end;
    Move(FEncKey, FDecKey, SizeOf(FEncKey));
    for I:= 1 to Nr - 1 do
      InvMixColumn(@FDecKey[I], Nb);
  finally
    XRTLFreeMemory(Pointer(PRK));
  end;
end;

procedure TXRTLRijndaelCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I, J: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsEnc[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, Nb * SizeOf(Cardinal));
  for I:= 0 to Nr - 2 do
  begin
    for J:= 0 to Nb - 1 do
    begin
      WTemp[J]:= WA[J] xor FEncKey[I, J];
    end;
    for J:= 0 to Nb - 1 do
    begin
      WA[J]:= WT1[Temp[ShiftMod[0, J], 0]] xor
              WT2[Temp[ShiftMod[1, J], 1]] xor
              WT3[Temp[ShiftMod[2, J], 2]] xor
              WT4[Temp[ShiftMod[3, J], 3]];
    end;
  end;
  for J:= 0 to Nb - 1 do
  begin
    WTemp[J]:= WA[J] xor FEncKey[Nr - 1, J];
  end;
  for J:= 0 to Nb - 1 do
  begin
    A[J, 0]:= Sd[Temp[ShiftMod[0, J], 0]];
    A[J, 1]:= Sd[Temp[ShiftMod[1, J], 1]];
    A[J, 2]:= Sd[Temp[ShiftMod[2, J], 2]];
    A[J, 3]:= Sd[Temp[ShiftMod[3, J], 3]];
    WA[J]:= WA[J] xor FEncKey[Nr, J];
  end;
  XRTLMoveMemory(@A, OutBuffer, Nb * SizeOf(Cardinal));
end;

procedure TXRTLRijndaelCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  I, J: Integer;
  ShiftMod: PXRTLRijndaelShiftMod;
  A, Temp: TXRTLRijndaelState;
  WA, WTemp: PXRTLRijndaelWordState;
begin
  WA:= @A;
  WTemp:= @Temp;
  ShiftMod:= @ShiftsDec[FBlockSize];
  XRTLMoveMemory(InBuffer, @A, Nb * SizeOf(Cardinal));
  for I:= Nr downto 2 do
  begin
    for J:= 0 to Nb - 1 do
    begin
      WTemp[J]:= WA[J] xor FDecKey[I, J];
    end;
    for J:= 0 to Nb - 1 do
    begin
      WA[J]:= WT5[Temp[ShiftMod[0, J], 0]] xor
              WT6[Temp[ShiftMod[1, J], 1]] xor
              WT7[Temp[ShiftMod[2, J], 2]] xor
              WT8[Temp[ShiftMod[3, J], 3]];
    end;
  end;
  for J:= 0 to Nb - 1 do
  begin
    WTemp[J]:= WA[J] xor FDecKey[1, J];
  end;
  for J:= 0 to Nb - 1 do
  begin
    A[J, 0]:= Si[Temp[ShiftMod[0, J], 0]];
    A[J, 1]:= Si[Temp[ShiftMod[1, J], 1]];
    A[J, 2]:= Si[Temp[ShiftMod[2, J], 2]];
    A[J, 3]:= Si[Temp[ShiftMod[3, J], 3]];
    WA[J]:= WA[J] xor FDecKey[0, J];
  end;
  XRTLMoveMemory(@A, OutBuffer, Nb * SizeOf(Cardinal));
end;

class function TXRTLRijndaelCipher.GetDisplayName: string;
begin
  Result:= 'Rijndael';
end;

end.

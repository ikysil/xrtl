unit xrtl_crypt_cipher_Anubis;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipher;

const
  XRTLAnubisBlockSize      = 16; // 128 bits
  XRTLAnubisMaxN           = 10;
  XRTLAnubisMaxRounds      = XRTLAnubisMaxN + 8;
  XRTLAnubisMaximumKeySize = 4 * XRTLAnubisMaxN;

type
  PXRTLAnubisBlock    = ^TXRTLAnubisBlock;
  TXRTLAnubisBlock    = array[0 .. 3] of Cardinal;
  PXRTLAnubisState    = ^TXRTLAnubisState;
  TXRTLAnubisState    = array[0 .. 3, 0 .. 3] of Byte;
  PXRTLAnubisRoundKey = ^TXRTLAnubisRoundKey;
  TXRTLAnubisRoundKey = array[0 .. XRTLAnubisMaxRounds, 0 .. 4] of Cardinal;

  TXRTLAnubisCipher = class(TXRTLBlockCipher)
  private
    FRounds: Integer;
    FRoundKeyEnc, FRoundKeyDec: TXRTLAnubisRoundKey;
  protected
// size of cipher block in BYTES
    function   engineGetBlockSize: Integer; override;
// size of key in BYTES
    function   engineGetMaximumKeySize: Integer; override;
    function   engineGetMinimumKeySize: Integer; override;
    procedure  engineSetKey; override;
    procedure  engineCrypt(InBuffer, OutBuffer: PXRTLAnubisBlock; RoundKey: PXRTLAnubisRoundKey);
  public
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

implementation

type
  TTBox = array[0 .. 255] of Cardinal;

const
  SBox: array[0 .. 255] of Byte = (
    $BA, $54, $2F, $74, $53, $D3, $D2, $4D, $50, $AC, $8D, $BF, $70, $52, $9A, $4C,
    $EA, $D5, $97, $D1, $33, $51, $5B, $A6, $DE, $48, $A8, $99, $DB, $32, $B7, $FC,
    $E3, $9E, $91, $9B, $E2, $BB, $41, $6E, $A5, $CB, $6B, $95, $A1, $F3, $B1, $02,
    $CC, $C4, $1D, $14, $C3, $63, $DA, $5D, $5F, $DC, $7D, $CD, $7F, $5A, $6C, $5C,
    $F7, $26, $FF, $ED, $E8, $9D, $6F, $8E, $19, $A0, $F0, $89, $0F, $07, $AF, $FB,
    $08, $15, $0D, $04, $01, $64, $DF, $76, $79, $DD, $3D, $16, $3F, $37, $6D, $38,
    $B9, $73, $E9, $35, $55, $71, $7B, $8C, $72, $88, $F6, $2A, $3E, $5E, $27, $46,
    $0C, $65, $68, $61, $03, $C1, $57, $D6, $D9, $58, $D8, $66, $D7, $3A, $C8, $3C,
    $FA, $96, $A7, $98, $EC, $B8, $C7, $AE, $69, $4B, $AB, $A9, $67, $0A, $47, $F2,
    $B5, $22, $E5, $EE, $BE, $2B, $81, $12, $83, $1B, $0E, $23, $F5, $45, $21, $CE,
    $49, $2C, $F9, $E6, $B6, $28, $17, $82, $1A, $8B, $FE, $8A, $09, $C9, $87, $4E,
    $E1, $2E, $E4, $E0, $EB, $90, $A4, $1E, $85, $60, $00, $25, $F4, $F1, $94, $0B,
    $E7, $75, $EF, $34, $31, $D4, $D0, $86, $7E, $AD, $FD, $29, $30, $3B, $9F, $F8,
    $C6, $13, $06, $05, $C5, $11, $77, $7C, $7A, $78, $36, $1C, $39, $59, $18, $56,
    $B3, $B0, $24, $20, $B2, $92, $A3, $C0, $44, $62, $10, $B4, $84, $43, $93, $C2,
    $4A, $BD, $8F, $2D, $BC, $9C, $6A, $40, $CF, $A2, $80, $4F, $1F, $CA, $AA, $42
  );

var
  T0, T1, T2, T3, T4, T5: TTBox;

procedure PreCompTBox;

  procedure Reduce(var Value: Cardinal);
  begin
    if (Value >= $100) then
      Value:= Value xor $11D; // reduce Value (mod ROOT)
  end;

var
  X, X2, X4, X6, X8, S1, S2, S4, S6, S8: Cardinal;
begin
  ZeroMemory(@T0, SizeOf(T0));
  ZeroMemory(@T1, SizeOf(T1));
  ZeroMemory(@T2, SizeOf(T2));
  ZeroMemory(@T3, SizeOf(T3));
  ZeroMemory(@T4, SizeOf(T4));
  ZeroMemory(@T5, SizeOf(T5));
  for X:= 0 to 255 do
  begin
    S1:= SBox[X];
    S2:= S1 shl 1;
    Reduce(S2);
    S4:= S2 shl 1;
    Reduce(S4);
    S6:= S4 xor S2;
    S8:= S4 shl 1;
    Reduce(S8);
    X2:= X shl 1;
    Reduce(X2);
    X4:= X2 shl 1;
    Reduce(X4);
    X6:= X2 xor X4;
    X8:= X4 shl 1;
    Reduce(X8);
    T0[X]:= (S1 shl 24) or (S2 shl 16) or (S4 shl 8) or S6; // [ S[X], 2S[X], 4S[X], 6S[X]]
    T1[X]:= (S2 shl 24) or (S1 shl 16) or (S6 shl 8) or S4; // [2S[X],  S[X], 6S[X], 4S[X]]
    T2[X]:= (S4 shl 24) or (S6 shl 16) or (S1 shl 8) or S2; // [4S[X], 6S[X],  S[X], 2S[X]]
    T3[X]:= (S6 shl 24) or (S4 shl 16) or (S2 shl 8) or S1; // [6S[X], 4S[X], 2S[X],  S[X]]
    T4[X]:= (S1 shl 24) or (S1 shl 16) or (S1 shl 8) or S1; // [ S[X],  S[X],  S[X],  S[X]]
    T5[X]:= (X  shl 24) or (X2 shl 16) or (X6 shl 8) or X8; // [   X,    2X,    6X,    8X]
  end;
end;

{ TXRTLAnubisCipher }

function TXRTLAnubisCipher.engineGetBlockSize: Integer;
begin
  Result:= XRTLAnubisBlockSize; // 128 bits
end;

function TXRTLAnubisCipher.engineGetMinimumKeySize: Integer;
begin
  Result:= 16; // 128 bits
end;

function TXRTLAnubisCipher.engineGetMaximumKeySize: Integer;
begin
  Result:= XRTLAnubisMaximumKeySize;
end;

procedure TXRTLAnubisCipher.engineSetKey;
var
  N, I, R, T, V: Integer;
  K0, K1, K2, K3: Cardinal;
  Kappa, Inter: array[0 .. XRTLAnubisMaxN - 1] of Cardinal;
begin
  ZeroMemory(@Kappa, SizeOf(Kappa));
  ZeroMemory(@Inter, SizeOf(Inter));
  ZeroMemory(@FRoundKeyEnc, SizeOf(FRoundKeyEnc));
  ZeroMemory(@FRoundKeyDec, SizeOf(FRoundKeyDec));
// determine the N length parameter:
// consider only the first 4*N bytes
  N:= KeySize div 4;
// determine number of rounds from key size:
  FRounds:= 8 + N;
// map byte array cipher key to initial key state (mu):
  for I:= 0 to N - 1 do
  begin
    Kappa[I]:= ((Key[I * 4{ + 0}]) shl 24) xor
               ((Key[I * 4 + 1]) shl 16) xor
               ((Key[I * 4 + 2]) shl 8) xor
               ((Key[I * 4 + 3]));
  end;
// generate R + 1 round keys:
  for R:= 0 to FRounds do
  begin
    K0:= T4[(Kappa[N - 1] shr 24) and $FF];
    K1:= T4[(Kappa[N - 1] shr 16) and $FF];
    K2:= T4[(Kappa[N - 1] shr  8) and $FF];
    K3:= T4[(Kappa[N - 1]       ) and $FF];
    for T:= N - 2 downto 0 do
    begin
      K0:=  T4[(Kappa[T] shr 24)       ] xor
           (T5[(K0 shr 24)        ] and $FF000000) xor
           (T5[(K0 shr 16) and $FF] and $00FF0000) xor
           (T5[(K0 shr  8) and $FF] and $0000FF00) xor
           (T5[(K0       ) and $FF] and $000000FF);
      K1:=  T4[(Kappa[T] shr 16) and $FF] xor
           (T5[(K1 shr 24)        ] and $FF000000) xor
           (T5[(K1 shr 16) and $FF] and $00FF0000) xor
           (T5[(K1 shr  8) and $FF] and $0000FF00) xor
           (T5[(K1       ) and $FF] and $000000FF);
      K2:=  T4[(Kappa[T] shr  8) and $FF] xor
           (T5[(K2 shr 24)        ] and $FF000000) xor
           (T5[(K2 shr 16) and $FF] and $00FF0000) xor
           (T5[(K2 shr  8) and $FF] and $0000FF00) xor
           (T5[(K2       ) and $FF] and $000000FF);
      K3:=  T4[(Kappa[T]       ) and $FF] xor
           (T5[(K3 shr 24)        ] and $FF000000) xor
           (T5[(K3 shr 16) and $FF] and $00FF0000) xor
           (T5[(K3 shr  8) and $FF] and $0000FF00) xor
           (T5[(K3       ) and $FF] and $000000FF);
    end;
    FRoundKeyEnc[R, 0]:= K0;
    FRoundKeyEnc[R, 1]:= K1;
    FRoundKeyEnc[R, 2]:= K2;
    FRoundKeyEnc[R, 3]:= K3;
    for I:= 0 to N - 1 do
    begin
      Inter[I]:= T0[(Kappa[     I           ] shr 24)        ] xor
                 T1[(Kappa[(N + I - 1) mod N] shr 16) and $FF] xor
                 T2[(Kappa[(N + I - 2) mod N] shr  8) and $FF] xor
                 T3[(Kappa[(N + I - 3) mod N]       ) and $FF];
    end;
    Kappa[0]:= (T0[4 * R    ] and $FF000000) xor
               (T1[4 * R + 1] and $00FF0000) xor
               (T2[4 * R + 2] and $0000FF00) xor
               (T3[4 * R + 3] and $000000FF) xor
               Inter[0];
    for I:= 1 to N - 1 do
      Kappa[I]:= Inter[I];
  end;
// generate inverse key schedule: K'^0 = K^R, K'^R = K^0, K'^r = theta(K^{R-r}):
  for I:= 0 to 3 do
  begin
    FRoundKeyDec[0, I]:= FRoundKeyEnc[FRounds, I];
    FRoundKeyDec[FRounds, I]:= FRoundKeyEnc[0, I];
  end;
  for R:= 1 to FRounds - 1 do
  begin
    for I:= 0 to 3 do
    begin
      V:= FRoundKeyEnc[FRounds - R, I];
      FRoundKeyDec[R, I]:= T0[T4[(V shr 24)        ] and $FF] xor
                           T1[T4[(V shr 16) and $FF] and $FF] xor
                           T2[T4[(V shr  8) and $FF] and $FF] xor
                           T3[T4[(V       ) and $FF] and $FF];
    end;
  end;
end;

procedure TXRTLAnubisCipher.engineCrypt(InBuffer, OutBuffer: PXRTLAnubisBlock; RoundKey: PXRTLAnubisRoundKey);
var
  R: Integer;
  State, Inter: array[0 .. 3] of Cardinal;
  StateBytes: TXRTLAnubisState absolute State;
begin
//  input transform
  State[0]:= XRTLSwapHiLo32(InBuffer[0]) xor RoundKey[0, 0];
  State[1]:= XRTLSwapHiLo32(InBuffer[1]) xor RoundKey[0, 1];
  State[2]:= XRTLSwapHiLo32(InBuffer[2]) xor RoundKey[0, 2];
  State[3]:= XRTLSwapHiLo32(InBuffer[3]) xor RoundKey[0, 3];
//  FRounds - 1 full rounds:
  for R:= 1 to FRounds - 1 do
  begin
                Inter[0]:= T0[StateBytes[0, 3]] xor T1[StateBytes[1, 3]] xor
               T2[StateBytes[2, 3]] xor T3[StateBytes[3, 3]] xor
               RoundKey[R, 0];
    Inter[1]:= T0[StateBytes[0, 2]] xor T1[StateBytes[1, 2]] xor
               T2[StateBytes[2, 2]] xor T3[StateBytes[3, 2]] xor
               RoundKey[R, 1];
    Inter[2]:= T0[StateBytes[0, 1]] xor T1[StateBytes[1, 1]] xor
               T2[StateBytes[2, 1]] xor T3[StateBytes[3, 1]] xor
               RoundKey[R, 2];
    Inter[3]:= T0[StateBytes[0, 0]] xor T1[StateBytes[1, 0]] xor
               T2[StateBytes[2, 0]] xor T3[StateBytes[3, 0]] xor
               RoundKey[R, 3];
    State[0]:= Inter[0];
    State[1]:= Inter[1];
    State[2]:= Inter[2];
    State[3]:= Inter[3];
  end;
//  last round and output transform
  OutBuffer[0]:= XRTLSwapHiLo32(
                   (T0[StateBytes[0 , 3]] and $FF000000) xor
                   (T1[StateBytes[1 , 3]] and $00FF0000) xor
                   (T2[StateBytes[2 , 3]] and $0000FF00) xor
                   (T3[StateBytes[3 , 3]] and $000000FF) xor
                   RoundKey[FRounds, 0]);
  OutBuffer[1]:= XRTLSwapHiLo32(
                   (T0[StateBytes[0 , 2]] and $FF000000) xor
                   (T1[StateBytes[1 , 2]] and $00FF0000) xor
                   (T2[StateBytes[2 , 2]] and $0000FF00) xor
                   (T3[StateBytes[3 , 2]] and $000000FF) xor
                   RoundKey[FRounds, 1]);
  OutBuffer[2]:= XRTLSwapHiLo32(
                   (T0[StateBytes[0 , 1]] and $FF000000) xor
                   (T1[StateBytes[1 , 1]] and $00FF0000) xor
                   (T2[StateBytes[2 , 1]] and $0000FF00) xor
                   (T3[StateBytes[3 , 1]] and $000000FF) xor
                   RoundKey[FRounds, 2]);
  OutBuffer[3]:= XRTLSwapHiLo32(
                   (T0[StateBytes[0 , 0]] and $FF000000) xor
                   (T1[StateBytes[1 , 0]] and $00FF0000) xor
                   (T2[StateBytes[2 , 0]] and $0000FF00) xor
                   (T3[StateBytes[3 , 0]] and $000000FF) xor
                   RoundKey[FRounds, 3]);
end;

procedure TXRTLAnubisCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  engineCrypt(PXRTLAnubisBlock(InBuffer), PXRTLAnubisBlock(OutBuffer), @FRoundKeyEnc);
end;

procedure TXRTLAnubisCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  engineCrypt(PXRTLAnubisBlock(InBuffer), PXRTLAnubisBlock(OutBuffer), @FRoundKeyDec);
end;

class function TXRTLAnubisCipher.GetDisplayName: string;
begin
  Result:= 'Anubis';
end;

initialization
begin
  PreCompTBox;
end;

end.

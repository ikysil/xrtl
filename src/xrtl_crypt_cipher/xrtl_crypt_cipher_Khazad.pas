unit xrtl_crypt_cipher_Khazad;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipher;

const
  XRTLKhazadBlockSize      = 8;  // 64 bits
  XRTLKhazadRounds         = 8;
  XRTLKhazadMinimumKeySize = 16; // 128 bits
  XRTLKhazadMaximumKeySize = 16; // 128 bits

type
  PXRTLKhazadBlock    = ^TXRTLKhazadBlock;
  TXRTLKhazadBlock    = array[0 .. XRTLKhazadBlockSize - 1] of Byte;

  PXRTLKhazadRoundKey = ^TXRTLKhazadRoundKey;
  TXRTLKhazadRoundKey = array[0 .. XRTLKhazadRounds] of Int64;

  TXRTLKhazadCipher = class(TXRTLBlockCipher)
  private
    FRoundKeyEnc, FRoundKeyDec: TXRTLKhazadRoundKey;
  protected
// size of cipher block in BYTES
    function   engineGetBlockSize: Integer; override;
// size of key in BYTES
    function   engineGetMaximumKeySize: Integer; override;
    function   engineGetMinimumKeySize: Integer; override;
    procedure  engineSetKey; override;
    procedure  engineCrypt(InBuffer, OutBuffer: PXRTLKhazadBlock; RoundKey: PXRTLKhazadRoundKey);
  public
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function  GetDisplayName: string; override;
  end;

implementation

type
  TTBox = array[0 .. 7, 0 .. 255] of Int64;

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
  T: TTBox;
  S: array[0 .. 255] of Cardinal;
  C: array[0 .. XRTLKhazadRounds] of Int64;

procedure PreCompTBox;

  procedure Reduce(var Value: Int64);
  begin
    if (Value >= $100) then
      Value:= Value xor $11D; // reduce Value (mod ROOT)
  end;

var
  X, R: Byte;
  S1, S2, S3, S4, S5, S6, S7, S8, Sb: Int64;
begin
  ZeroMemory(@T, SizeOf(T));
  ZeroMemory(@S, SizeOf(S));
  ZeroMemory(@C, SizeOf(C));
  for X:= 0 to 255 do
  begin
    S1:= SBox[X];
    S2:= S1 shl 1;
    Reduce(S2);
    S3:= S2 xor S1;
    S4:= S2 shl 1;
    Reduce(S4);
    S5:= S4 xor S1;
    S6:= S4 xor S2;
    S7:= S6 xor S1;
    S8:= S4 shl 1;
    Reduce(S8);
    Sb:= S8 xor S2 xor S1;
    T[0, X]:= (S1 shl 56) or (S3 shl 48) or (S4 shl 40) or (S5 shl 32) or (S6 shl 24) or (S8 shl 16) or (Sb shl 8) or S7;
    T[1, X]:= (S3 shl 56) or (S1 shl 48) or (S5 shl 40) or (S4 shl 32) or (S8 shl 24) or (S6 shl 16) or (S7 shl 8) or Sb;
    T[2, X]:= (S4 shl 56) or (S5 shl 48) or (S1 shl 40) or (S3 shl 32) or (Sb shl 24) or (S7 shl 16) or (S6 shl 8) or S8;
    T[3, X]:= (S5 shl 56) or (S4 shl 48) or (S3 shl 40) or (S1 shl 32) or (S7 shl 24) or (Sb shl 16) or (S8 shl 8) or S6;
    T[4, X]:= (S6 shl 56) or (S8 shl 48) or (Sb shl 40) or (S7 shl 32) or (S1 shl 24) or (S3 shl 16) or (S4 shl 8) or S5;
    T[5, X]:= (S8 shl 56) or (S6 shl 48) or (S7 shl 40) or (Sb shl 32) or (S3 shl 24) or (S1 shl 16) or (S5 shl 8) or S4;
    T[6, X]:= (Sb shl 56) or (S7 shl 48) or (S6 shl 40) or (S8 shl 32) or (S4 shl 24) or (S5 shl 16) or (S1 shl 8) or S3;
    T[7, X]:= (S7 shl 56) or (Sb shl 48) or (S8 shl 40) or (S6 shl 32) or (S5 shl 24) or (S4 shl 16) or (S3 shl 8) or S1;
    S[X]:= S1;
  end;
  for R:= 0 to XRTLKhazadRounds do
  begin
    C[R]:= (Int64(SBox[8 * R + 1]) shl 48) or (Int64(SBox[8 * R + 0]) shl 56) or
           (Int64(SBox[8 * R + 3]) shl 32) or (Int64(SBox[8 * R + 2]) shl 40) or
           (Int64(SBox[8 * R + 5]) shl 16) or (Int64(SBox[8 * R + 4]) shl 24) or
           (Int64(SBox[8 * R + 7]) shl  0) or (Int64(SBox[8 * R + 6]) shl  8);
  end;
end;

{ TXRTLKhazadCipher }

function TXRTLKhazadCipher.engineGetBlockSize: Integer;
begin
  Result:= XRTLKhazadBlockSize; // 64 bits
end;

function TXRTLKhazadCipher.engineGetMinimumKeySize: Integer;
begin
  Result:= XRTLKhazadMinimumKeySize;
end;

function TXRTLKhazadCipher.engineGetMaximumKeySize: Integer;
begin
  Result:= XRTLKhazadMaximumKeySize;
end;

procedure TXRTLKhazadCipher.engineSetKey;
var
  R: Byte;
  K1, K2: Int64;
begin
  ZeroMemory(@FRoundKeyEnc, SizeOf(FRoundKeyEnc));
  ZeroMemory(@FRoundKeyDec, SizeOf(FRoundKeyDec));
  K2:= (Int64(Key[ 0]) shl 56) xor
       (Int64(Key[ 1]) shl 48) xor
       (Int64(Key[ 2]) shl 40) xor
       (Int64(Key[ 3]) shl 32) xor
       (Int64(Key[ 4]) shl 24) xor
       (Int64(Key[ 5]) shl 16) xor
       (Int64(Key[ 6]) shl  8) xor
       (Int64(Key[ 7])       );
  K1:= (Int64(Key[ 8]) shl 56) xor
       (Int64(Key[ 9]) shl 48) xor
       (Int64(Key[10]) shl 40) xor
       (Int64(Key[11]) shl 32) xor
       (Int64(Key[12]) shl 24) xor
       (Int64(Key[13]) shl 16) xor
       (Int64(Key[14]) shl  8) xor
       (Int64(Key[15])       );
// compute the round keys:
  for R:= 0 to XRTLKhazadRounds do
  begin
    FRoundKeyEnc[R]:= T[0, (K1 shr 56) and $FF] xor
                      T[1, (K1 shr 48) and $FF] xor
                      T[2, (K1 shr 40) and $FF] xor
                      T[3, (K1 shr 32) and $FF] xor
                      T[4, (K1 shr 24) and $FF] xor
                      T[5, (K1 shr 16) and $FF] xor
                      T[6, (K1 shr  8) and $FF] xor
                      T[7, (K1       ) and $FF] xor
                      C[R] xor K2;
    K2:= K1;
    K1:= FRoundKeyEnc[R];
  end;
// compute the inverse key schedule:
        FRoundKeyDec[0]:= FRoundKeyEnc[XRTLKhazadRounds];
  for R:= 1 to XRTLKhazadRounds - 1 do
  begin
    K1:= FRoundKeyEnc[XRTLKhazadRounds - R];
    FRoundKeyDec[R]:= T[0, S[(K1 shr 56) and $FF]] xor
                      T[1, S[(K1 shr 48) and $FF]] xor
                      T[2, S[(K1 shr 40) and $FF]] xor
                      T[3, S[(K1 shr 32) and $FF]] xor
                      T[4, S[(K1 shr 24) and $FF]] xor
                      T[5, S[(K1 shr 16) and $FF]] xor
                      T[6, S[(K1 shr  8) and $FF]] xor
                      T[7, S[(K1       ) and $FF]];
  end;
  FRoundKeyDec[XRTLKhazadRounds]:= FRoundKeyEnc[0];
end;

procedure TXRTLKhazadCipher.engineCrypt(InBuffer, OutBuffer: PXRTLKhazadBlock; RoundKey: PXRTLKhazadRoundKey);
var
  R: Byte;
  State: Int64;
  StateBytes: PXRTLKhazadBlock;
begin
  StateBytes:= @State;
  State:= XRTLSwapHiLo64(PInt64(InBuffer)^) xor RoundKey[0];
// R - 1 full rounds:
  for R:= 1 to XRTLKhazadRounds - 1 do
  begin
    State:= T[0, StateBytes[7]] xor
            T[1, StateBytes[6]] xor
            T[2, StateBytes[5]] xor
            T[3, StateBytes[4]] xor
            T[4, StateBytes[3]] xor
            T[5, StateBytes[2]] xor
            T[6, StateBytes[1]] xor
            T[7, StateBytes[0]] xor
            RoundKey[R];
  end;
// last round
  State:= (T[0, StateBytes[7]] and $FF00000000000000) xor
          (T[1, StateBytes[6]] and $00FF000000000000) xor
          (T[2, StateBytes[5]] and $0000FF0000000000) xor
          (T[3, StateBytes[4]] and $000000FF00000000) xor
          (T[4, StateBytes[3]] and $00000000FF000000) xor
          (T[5, StateBytes[2]] and $0000000000FF0000) xor
          (T[6, StateBytes[1]] and $000000000000FF00) xor
          (T[7, StateBytes[0]] and $00000000000000FF) xor
          RoundKey[XRTLKhazadRounds];
  PInt64(OutBuffer)^:= XRTLSwapHiLo64(State);
end;

procedure TXRTLKhazadCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  engineCrypt(PXRTLKhazadBlock(InBuffer), PXRTLKhazadBlock(OutBuffer), @FRoundKeyEnc);
end;

procedure TXRTLKhazadCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  engineCrypt(PXRTLKhazadBlock(InBuffer), PXRTLKhazadBlock(OutBuffer), @FRoundKeyDec);
end;

class function TXRTLKhazadCipher.GetDisplayName: string;
begin
  Result:= 'Khazad';
end;

initialization
begin
  PreCompTBox;
end;

end.

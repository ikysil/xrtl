unit xrtl_crypt_cipher_RijndaelUtil;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_util_CPUUtils,
  xrtl_crypt_cipher_RijndaelBase;
  
type
  PXRTLRijndaelShift = ^TXRTLRijndaelShift;
  TXRTLRijndaelShift = array[0 .. 7] of Byte;

  PXRTLRijndaelShiftMod = ^TXRTLRijndaelShiftMod;
  TXRTLRijndaelShiftMod = array[0 .. 3] of TXRTLRijndaelShift;

  TXRTLRijndaelSBox = array[0 .. 255] of Byte;
  TXRTLRijndaelTBox = array[0 .. 255, 0 .. 3] of Byte;

  PXRTLRijndaelWordTBox = ^TXRTLRijndaelWordTBox;
  TXRTLRijndaelWordTBox = array[0 .. 255] of Cardinal;

const
  RijndaelBlockSize: array[TXRTLRijndaelBlockSize] of Integer = (
    16, 20, 24, 28, 32
  );

var
  T1, T2, T3, T4, T5, T6, T7, T8, U1, U2, U3, U4: TXRTLRijndaelTBox;
  Si: TXRTLRijndaelSBox;

  ShiftsEnc: array[TXRTLRijndaelBlockSize] of TXRTLRijndaelShiftMod;
  ShiftsDec: array[TXRTLRijndaelBlockSize] of TXRTLRijndaelShiftMod;

  WT1: TXRTLRijndaelWordTBox absolute T1;
  WT2: TXRTLRijndaelWordTBox absolute T2;
  WT3: TXRTLRijndaelWordTBox absolute T3;
  WT4: TXRTLRijndaelWordTBox absolute T4;
  WT5: TXRTLRijndaelWordTBox absolute T5;
  WT6: TXRTLRijndaelWordTBox absolute T6;
  WT7: TXRTLRijndaelWordTBox absolute T7;
  WT8: TXRTLRijndaelWordTBox absolute T8;
  WU1: TXRTLRijndaelWordTBox absolute U1;
  WU2: TXRTLRijndaelWordTBox absolute U2;
  WU3: TXRTLRijndaelWordTBox absolute U3;
  WU4: TXRTLRijndaelWordTBox absolute U4;

const
  Sd: array[0 .. 255] of Byte = (
     99, 124, 119, 123, 242, 107, 111, 197,  48,   1, 103,  43, 254, 215, 171, 118,
    202, 130, 201, 125, 250,  89,  71, 240, 173, 212, 162, 175, 156, 164, 114, 192,
    183, 253, 147,  38,  54,  63, 247, 204,  52, 165, 229, 241, 113, 216,  49,  21,
      4, 199,  35, 195,  24, 150,   5, 154,   7,  18, 128, 226, 235,  39, 178, 117,
      9, 131,  44,  26,  27, 110,  90, 160,  82,  59, 214, 179,  41, 227,  47, 132,
     83, 209,   0, 237,  32, 252, 177,  91, 106, 203, 190,  57,  74,  76,  88, 207,
    208, 239, 170, 251,  67,  77,  51, 133,  69, 249,   2, 127,  80,  60, 159, 168,
     81, 163,  64, 143, 146, 157,  56, 245, 188, 182, 218,  33,  16, 255, 243, 210,
    205,  12,  19, 236,  95, 151,  68,  23, 196, 167, 126,  61, 100,  93,  25, 115,
     96, 129,  79, 220,  34,  42, 144, 136,  70, 238, 184,  20, 222,  94,  11, 219,
    224,  50,  58,  10,  73,   6,  36,  92, 194, 211, 172,  98, 145, 149, 228, 121,
    231, 200,  55, 109, 141, 213,  78, 169, 108,  86, 244, 234, 101, 122, 174,   8,
    186, 120,  37,  46,  28, 166, 180, 198, 232, 221, 116,  31,  75, 189, 139, 138,
    112,  62, 181, 102,  72,   3, 246,  14,  97,  53,  87, 185, 134, 193,  29, 158,
    225, 248, 152,  17, 105, 217, 142, 148, 155,  30, 135, 233, 206,  85,  40, 223,
    140, 161, 137,  13, 191, 230,  66, 104,  65, 153,  45,  15, 176,  84, 187,  22);

  Rcon: array[0 .. 29] of Cardinal = (
    $01, $02, $04, $08, $10, $20, $40, $80, $1B, $36, $6C, $D8, $AB, $4D, $9A,
    $2F, $5E, $BC, $63, $C6, $97, $35, $6A, $D4, $B3, $7D, $FA, $EF, $C5, $91);

  ShiftSeed: array[TXRTLRijndaelBlockSize, 0 .. 3] of Integer = (
    (0, 1, 2, 3),
    (0, 1, 2, 3),
    (0, 1, 2, 3),
    (0, 1, 2, 4),
    (0, 1, 3, 4)
  );

procedure InitTables;
procedure InvMixColumn(a: PByteArray; BC: byte);

implementation

procedure InitTables;
  procedure Reduce(var Value: Integer);
  begin
    if (Value >= $100) then
      Value:= Value xor $11B; // reduce Value (mod ROOT)
  end;

  procedure GenerateShifts(var ShiftsMod: TXRTLRijndaelShiftMod; Modulo: Integer);
  var
    I, J: Integer;
  begin
    for I:= 0 to 3 do
    begin
      for J:= 1 to 7 do
      begin
        ShiftsMod[I, J]:= (ShiftsMod[I, 0] + J) mod Modulo;
      end;
    end;
  end;

var
  I, S, S2, S3, I2, I4, I8, I9, Ib, Id, Ie: Integer;
  BS: TXRTLRijndaelBlockSize;
begin
  for I:= 0 to 255 do
  begin
    S:= Sd[I];
    Si[S]:= I;
    S2:= S shl 1;
    Reduce(S2);
    S3:= S2 xor S;
    I2:= I shl 1;
    Reduce(I2);
    I4:= I2 shl 1;
    Reduce(I4);
    I8:= I4 shl 1;
    Reduce(I8);
    I9:= I8 xor I;
    Ib:= I9 xor I2;
    Id:= I9 xor I4;
    Ie:= I8 xor I4 xor I2;

    T1[I, 0]:= S2;
    T1[I, 1]:= S;
    T1[I, 2]:= S;
    T1[I, 3]:= S3;
    WT2[I]:= XRTLROL32(WT1[I], 8);
    WT3[I]:= XRTLROL32(WT2[I], 8);
    WT4[I]:= XRTLROL32(WT3[I], 8);

    T5[S, 0]:= Ie;
    T5[S, 1]:= I9;
    T5[S, 2]:= Id;
    T5[S, 3]:= Ib;
    WT6[S]:= XRTLROL32(WT5[S], 8);
    WT7[S]:= XRTLROL32(WT6[S], 8);
    WT8[S]:= XRTLROL32(WT7[S], 8);

    WU1[I]:= WT5[S];
    WU2[I]:= WT6[S];
    WU3[I]:= WT7[S];
    WU4[I]:= WT8[S];
  end;
  for BS:= rbs128Bits to rbs256Bits do
  begin
    for I:= 0 to 3 do
    begin
      ShiftsEnc[BS, I, 0]:= ShiftSeed[BS, I];
      if I = 0 then
        ShiftsDec[BS, I, 0]:= ShiftsEnc[BS, I, 0]
      else
        ShiftsDec[BS, I, 0]:= RijndaelBlockSize[BS] div 4 - ShiftsEnc[BS, I, 0];
    end;
    GenerateShifts(ShiftsEnc[BS], RijndaelBlockSize[BS] div 4);
    GenerateShifts(ShiftsDec[BS], RijndaelBlockSize[BS] div 4);
  end;
end;

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longint;
begin
  for j:= 0 to (BC-1) do
  begin
    PDWord(@a[j*4])^:= PDWord(@U1[a[j*4+0]])^ xor
                       PDWord(@U2[a[j*4+1]])^ xor
                       PDWord(@U3[a[j*4+2]])^ xor
                       PDWord(@U4[a[j*4+3]])^;
  end;
end;

initialization
begin
  InitTables;
end;

end.

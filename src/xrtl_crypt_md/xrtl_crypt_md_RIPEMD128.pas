unit xrtl_crypt_md_RIPEMD128;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_BlockMessageDigest;

const
  XRTLRIPEMD128BlockSize = 64; // 512 bits
  XRTLRIPEMD128SumSize   = 16;

type
  PXRTLRIPEMD128Context = ^TXRTLRIPEMD128Context;
  TXRTLRIPEMD128Context = array[0 .. 3] of Cardinal;

  PXRTLRIPEMD128Sum     = ^TXRTLRIPEMD128Sum;
  TXRTLRIPEMD128Sum     = array[0 .. XRTLRIPEMD128SumSize - 1] of Byte;

  PXRTLRIPEMD128Buffer  = ^TXRTLRIPEMD128Buffer;
  TXRTLRIPEMD128Buffer  = array[0 .. XRTLRIPEMD128BlockSize - 1] of Byte;

  TXRTLRIPEMD128 = class(TXRTLBlockMessageDigest)
  private
  protected
    Context: TXRTLRIPEMD128Context;
    FCount: Int64;
    procedure  engineUpdateDigestBlock(InBuffer: PByteArray); override;
    procedure  engineUpdateDigestBlockFinal(InBuffer: PByteArray;
                                            InAvail: Integer); override;
    procedure  engineInit; override;
  public
    constructor Create;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  Windows,
  xrtl_util_MemoryUtils, xrtl_util_CPUUtils;

const
//  selection of message word
  R: array[0 .. 63] of Byte = (
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    7,  4, 13,  1, 10,  6, 15,  3, 12,  0,  9,  5,  2, 14, 11,  8,
    3, 10, 14,  4,  9, 15,  8,  1,  2,  7,  0,  6, 13, 11,  5, 12,
    1,  9, 11, 10,  0,  8, 12,  4, 13,  3,  7, 15, 14,  5,  6,  2
  );

  Rp: array[0 .. 63] of Byte = (
     5, 14,  7,  0,  9,  2, 11,  4, 13,  6, 15,  8,  1, 10,  3, 12,
     6, 11,  3,  7,  0, 13,  5, 10, 14, 15,  8, 12,  4,  9,  1,  2,
    15,  5,  1,  3,  7, 14,  6,  9, 11,  8, 12,  2, 10,  0,  4, 13,
     8,  6,  4,  1,  3, 11, 15,  0,  5, 12,  2, 13,  9,  7, 10, 14
  );

// amount for rotate left (rol)
  S: array[0 .. 63] of Byte = (
    11, 14, 15, 12,  5,  8,  7,  9, 11, 13, 14, 15,  6,  7,  9,  8,
     7,  6,  8, 13, 11,  9,  7, 15,  7, 12, 15,  9, 11,  7, 13, 12,
    11, 13,  6,  7, 14,  9, 13, 15, 14,  8, 13,  6,  5, 12,  7,  5,
    11, 12, 14, 15, 14, 15,  9,  8,  9, 14,  5,  6,  8,  6,  5, 12
  );

  Sp: array[0 .. 63] of Byte = (
     8,  9,  9, 11, 13, 15, 15,  5,  7,  7,  8, 11, 14, 14, 12,  6,
     9, 13, 15,  7, 12,  8,  9, 11,  7,  7, 12,  7,  6, 15, 13, 11,
     9,  7, 15, 11,  8,  6,  6, 14, 12, 13,  5, 14, 13, 13,  7,  5,
    15,  5,  8, 11, 14, 14,  6, 14,  6,  9, 12,  9, 12,  5, 15,  8
  );

type
  TXRTLRIPEMD128Func = function(B, C, D: Cardinal): Cardinal;

function F1(B, C, D: Cardinal): Cardinal;
begin
  Result:= B xor C xor D;
end;

function F2(B, C, D: Cardinal): Cardinal;
begin
  Result:= (B and C) or (not B and D);
end;

function F3(B, C, D: Cardinal): Cardinal;
begin
  Result:= (B or not C) xor D;
end;

function F4(B, C, D: Cardinal): Cardinal;
begin
  Result:= (B and D) or (C and not D);
end;

function F5(B, C, D: Cardinal): Cardinal;
begin
  Result:= B xor (C or not D);
end;

function F1p(Bp, Cp, Dp: Cardinal): Cardinal;
begin
  Result:= (Bp and Dp) or (Cp and not Dp);
end;

function F2p(Bp, Cp, Dp: Cardinal): Cardinal;
begin
  Result:= (Bp or not Cp) xor Dp;
end;

function F3p(Bp, Cp, Dp: Cardinal): Cardinal;
begin
  Result:= (Bp and Cp) or (not Bp and Dp);
end;

function F4p(Bp, Cp, Dp: Cardinal): Cardinal;
begin
  Result:= Bp xor Cp xor Dp;
end;

{ TXRTLRIPEMD128 }

constructor TXRTLRIPEMD128.Create;
begin
  inherited Create(XRTLRIPEMD128SumSize, XRTLRIPEMD128BlockSize);
end;

procedure TXRTLRIPEMD128.engineInit;
begin
  inherited;
  Context[0]:= $67452301;
  Context[1]:= $EFCDAB89;
  Context[2]:= $98BADCFE;
  Context[3]:= $10325476;
  FCount:= 0;
end;

procedure TXRTLRIPEMD128.engineUpdateDigestBlock(InBuffer: PByteArray);
const
  F:  array[0 .. 3] of TXRTLRIPEMD128Func = (F1, F2, F3, F4);
  Fp: array[0 .. 3] of TXRTLRIPEMD128Func = (F1p, F2p, F3p, F4p);
  K:  array[0 .. 3] of Cardinal = (0, $5A827999, $6ED9EBA1, $8F1BBCDC);
  Kp: array[0 .. 3] of Cardinal = ($50A28BE6, $5C4DD124, $6D703EF3, 0);
var
  X: PCardinalArray;
  A, B, C, D, Ap, Bp, Cp, Dp, T, Sx, I, Rx: Cardinal;
begin
  Inc(FCount, XRTLRIPEMD128BlockSize);
  X:= PCardinalArray(InBuffer);

// encode 64 bytes from input block into an array of 16 unsigned integers.
  Ap:= Context[0];
  Bp:= Context[1];
  Cp:= Context[2];
  Dp:= Context[3];
  A:= Ap;
  B:= Bp;
  C:= Cp;
  D:= Dp;

  for I:= 0 to 63 do
  begin
    Rx:= I div 16;
    Sx:= S[I];
    T:= A + F[Rx](B, C, D) + X[R[I]] + K[Rx];
    A:= D; D:= C; C:= B;
    B:= XRTLROL32(T, Sx);

    Sx:= Sp[I];
    T:= Ap + Fp[Rx](Bp, Cp, Dp) + X[Rp[I]] + Kp[Rx];
    Ap:= Dp; Dp:= Cp; Cp:= Bp;
    Bp:= XRTLROL32(T, Sx);
  end;

  T:= Context[1] + C + Dp;
  Context[1]:= Context[2] + D + Ap;
  Context[2]:= Context[3] + A + Bp;
  Context[3]:= Context[0] + B + Cp;
  Context[0]:= T;
end;

procedure TXRTLRIPEMD128.engineUpdateDigestBlockFinal(InBuffer: PByteArray;
  InAvail: Integer);
var
  Buf: TXRTLRIPEMD128Buffer;
  WorkBuf: PInt64Array;
  LCount: Int64;
begin
  LCount:= FCount + InAvail;
  ZeroMemory(@Buf, SizeOf(Buf));
  XRTLMoveMemory(InBuffer, @Buf, InAvail);
  Buf[InAvail]:= $80;
  if InAvail + 1 > XRTLRIPEMD128BlockSize - 8 then
  begin
    engineUpdateDigestBlock(@Buf);
    ZeroMemory(@Buf, SizeOf(Buf));
  end;
  WorkBuf:= @Buf;
  WorkBuf^[7]:= LCount * 8;
  engineUpdateDigestBlock(@Buf);
  XRTLMoveMemory(@Context, FBytes, XRTLRIPEMD128SumSize);
end;

class function TXRTLRIPEMD128.GetDisplayName: string;
begin
  Result:= 'RIPEMD128';
end;

end.

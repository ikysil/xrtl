unit xrtl_crypt_md_MD4;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Compat,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_BlockMessageDigest;

const
  XRTLMD4BlockSize = 64; // 512 bits
  XRTLMD4SumSize   = 16;

type
  PXRTLMD4Context = ^TXRTLMD4Context;
  TXRTLMD4Context = array[0 .. 3] of Cardinal;

  PXRTLMD4Work    = ^TXRTLMD4Work;
  TXRTLMD4Work    = array[0 .. 15] of Cardinal;

  PXRTLMD4Sum     = ^TXRTLMD4Sum;
  TXRTLMD4Sum     = array[0 .. XRTLMD4SumSize - 1] of Byte;

  PXRTLMD4Buffer  = ^TXRTLMD4Buffer;
  TXRTLMD4Buffer  = array[0 .. XRTLMD4BlockSize - 1] of Byte;

  PXRTLMD4Tail    = ^TXRTLMD4Tail;
  TXRTLMD4Tail    = array[0 .. 127 + XRTLMD4BlockSize] of Byte;

  TXRTLMD4 = class(TXRTLBlockMessageDigest)
  private
  protected
    FContext: TXRTLMD4Context;
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
  xrtl_util_MemoryUtils;

function FF(A, B, C, D, X, S: Cardinal): Cardinal;
var
  T: Cardinal;
begin
  T:= A + ((B and C) or ((not B) and D)) + X;
  Result:= (T shl S) or (T shr (32 - S));
end;

function GG(A, B, C, D, X, S: Cardinal): Cardinal;
var
  T: Cardinal;
begin
  T:= A + ((B and (C or D)) or (C and D)) + X + $5A827999;
  Result:= (T shl S) or (T shr (32 - S));
end;

function HH(A, B, C, D, X, S: Cardinal): Cardinal;
var
  T: Cardinal;
begin
  T:= A + (B xor C xor D) + X + $6ED9EBA1;
  Result:= (T shl S) or (T shr (32 - S));
end;

{ TXRTLMD4 }

constructor TXRTLMD4.Create;
begin
  inherited Create(XRTLMD4SumSize, XRTLMD4BlockSize);
end;

procedure TXRTLMD4.engineInit;
begin
  inherited;
  FContext[0]:= $67452301;
  FContext[1]:= $EFCDAB89;
  FContext[2]:= $98BADCFE;
  FContext[3]:= $10325476;
  FCount:= 0;
end;

procedure TXRTLMD4.engineUpdateDigestBlock(InBuffer: PByteArray);
var
  I: Integer;
  IBlock: PIntegerArray;
  A, B, C, D: Cardinal;
  X: TXRTLMD4Work;
begin
  Inc(FCount, XRTLMD4BlockSize);
  IBlock:= PIntegerArray(InBuffer);
  for I:= 0 to 15 do
  begin
    X[I]:= IBlock[I];
  end;
  A:= FContext[0];
  B:= FContext[1];
  C:= FContext[2];
  D:= FContext[3];

  A:= FF(A, B, C, D, X[ 0],  3);
  D:= FF(D, A, B, C, X[ 1],  7);
  C:= FF(C, D, A, B, X[ 2], 11);
  B:= FF(B, C, D, A, X[ 3], 19);
  A:= FF(A, B, C, D, X[ 4],  3);
  D:= FF(D, A, B, C, X[ 5],  7);
  C:= FF(C, D, A, B, X[ 6], 11);
  B:= FF(B, C, D, A, X[ 7], 19);
  A:= FF(A, B, C, D, X[ 8],  3);
  D:= FF(D, A, B, C, X[ 9],  7);
  C:= FF(C, D, A, B, X[10], 11);
  B:= FF(B, C, D, A, X[11], 19);
  A:= FF(A, B, C, D, X[12],  3);
  D:= FF(D, A, B, C, X[13],  7);
  C:= FF(C, D, A, B, X[14], 11);
  B:= FF(B, C, D, A, X[15], 19);

  A:= GG(A, B, C, D, X[ 0],  3);
  D:= GG(D, A, B, C, X[ 4],  5);
  C:= GG(C, D, A, B, X[ 8],  9);
  B:= GG(B, C, D, A, X[12], 13);
  A:= GG(A, B, C, D, X[ 1],  3);
  D:= GG(D, A, B, C, X[ 5],  5);
  C:= GG(C, D, A, B, X[ 9],  9);
  B:= GG(B, C, D, A, X[13], 13);
  A:= GG(A, B, C, D, X[ 2],  3);
  D:= GG(D, A, B, C, X[ 6],  5);
  C:= GG(C, D, A, B, X[10],  9);
  B:= GG(B, C, D, A, X[14], 13);
  A:= GG(A, B, C, D, X[ 3],  3);
  D:= GG(D, A, B, C, X[ 7],  5);
  C:= GG(C, D, A, B, X[11],  9);
  B:= GG(B, C, D, A, X[15], 13);

  A:= HH(A, B, C, D, X[ 0],  3);
  D:= HH(D, A, B, C, X[ 8],  9);
  C:= HH(C, D, A, B, X[ 4], 11);
  B:= HH(B, C, D, A, X[12], 15);
  A:= HH(A, B, C, D, X[ 2],  3);
  D:= HH(D, A, B, C, X[10],  9);
  C:= HH(C, D, A, B, X[ 6], 11);
  B:= HH(B, C, D, A, X[14], 15);
  A:= HH(A, B, C, D, X[ 1],  3);
  D:= HH(D, A, B, C, X[ 9],  9);
  C:= HH(C, D, A, B, X[ 5], 11);
  B:= HH(B, C, D, A, X[13], 15);
  A:= HH(A, B, C, D, X[ 3],  3);
  D:= HH(D, A, B, C, X[11],  9);
  C:= HH(C, D, A, B, X[ 7], 11);
  B:= HH(B, C, D, A, X[15], 15);

  Inc(FContext[0], A);
  Inc(FContext[1], B);
  Inc(FContext[2], C);
  Inc(FContext[3], D);
end;

procedure TXRTLMD4.engineUpdateDigestBlockFinal(InBuffer: PByteArray;
  InAvail: Integer);
var
  Tail: TXRTLMD4Tail;
  BufferNdx, PadLen: Byte;
  TailLen: Integer;
  PBArray: PByteArray;
begin
  Inc(FCount, InAvail);
  BufferNdx:= FCount mod XRTLMD4BlockSize;
  if BufferNdx < 56 then
    PadLen:= 56 - BufferNdx
  else
    PadLen:= 120 - BufferNdx;
  TailLen:= InAvail + PadLen + 8;
  ZeroMemory(@Tail, SizeOf(Tail));
  XRTLMoveMemory(InBuffer, @Tail, InAvail);
  Tail[InAvail]:= $80;
  PInt64(@Tail[InAvail + PadLen])^:= FCount * 8;
  PBArray:= @Tail;
  engineUpdateDigest(PBArray, TailLen, spoRun);
  XRTLMoveMemory(@FContext, FBytes, XRTLMD4SumSize);
end;

class function TXRTLMD4.GetDisplayName: string;
begin
  Result:= 'MD4';
end;

end.

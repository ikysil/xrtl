unit xrtl_crypt_md_SHA;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_BlockMessageDigest;

const
  XRTLSHABlockSize = 64; // 512 bits
  XRTLSHASumSize   = 20; // 160 bits

type
  PXRTLSHADigest  = ^TXRTLSHADigest;
  TXRTLSHADigest  = array[0 .. 4] of Cardinal;

  PXRTLSHAWork    = ^TXRTLSHAWork;
  TXRTLSHAWork    = array[0 .. 79] of Cardinal;

  PXRTLSHASum     = ^TXRTLSHASum;
  TXRTLSHASum     = array[0 .. XRTLSHASumSize - 1] of Byte;

  PXRTLSHABuffer  = ^TXRTLSHABuffer;
  TXRTLSHABuffer  = array[0 .. XRTLSHABlockSize - 1] of Byte;

  TXRTLSHA = class(TXRTLBlockMessageDigest)
  private
  protected
    Digest: TXRTLSHADigest;
    FCount: Int64;
    procedure  engineUpdateDigestBlock(InBuffer: PByteArray); override;
    procedure  engineUpdateDigestBlockFinal(InBuffer: PByteArray;
                                            InAvail: Integer); override;
    procedure  engineInit; override;
    procedure  engineExpand(var W: TXRTLSHAWork); virtual; abstract;
  public
    constructor Create;
  end;

implementation

uses
  Windows,
  xrtl_util_MemoryUtils, xrtl_util_CPUUtils;
  
type
  TXRTLSHAFunc = function(A, B, C: Cardinal): Cardinal;  

function F1(A, B, C: Cardinal): Cardinal;
begin
  Result:= (A and B) or (not A and C);
end;

function F2(A, B, C: Cardinal): Cardinal;
begin
  Result:= A xor B xor C;
end;

function F3(A, B, C: Cardinal): Cardinal;
begin
  Result:= (A and B) or (A and C) or (B and C);
end;

function F4(A, B, C: Cardinal): Cardinal;
begin
  Result:= A xor B xor C;
end;

{ TXRTLSHA }

constructor TXRTLSHA.Create;
begin
  inherited Create(XRTLSHASumSize, XRTLSHABlockSize);
end;

procedure TXRTLSHA.engineInit;
begin
  inherited;
  Digest[0]:= $67452301;
  Digest[1]:= $EFCDAB89;
  Digest[2]:= $98BADCFE;
  Digest[3]:= $10325476;
  Digest[4]:= $C3D2E1F0;
  FCount:= 0;
end;

procedure TXRTLSHA.engineUpdateDigestBlock(InBuffer: PByteArray);
const
  Fx: array[0 .. 3] of TXRTLSHAFunc = (F1, F2, F3, F4);
  Kx: array[0 .. 3] of Cardinal = ($5A827999, $6ED9EBA1, $8F1BBCDC, $CA62C1D6);
var
  X: PIntegerArray;
  A, B, C, D, E: Cardinal;
  I: Integer;
  Temp, F: Cardinal;
  W: TXRTLSHAWork;
begin
  Inc(FCount, XRTLSHABlockSize);
  X:= PIntegerArray(InBuffer);
  A:= Digest[0];
  B:= Digest[1];
  C:= Digest[2];
  D:= Digest[3];
  E:= Digest[4];

  for I:= 0 to 15 do
  begin
    W[I]:= XRTLSwapHiLo32(X[I]);
  end;

  engineExpand(W);

  for I:= 0 to 79 do
  begin
    F:= I div 20;
    Temp:= XRTLROL32(A, 5) + Fx[F](B, C, D) + Kx[F] + E + W[I];
    E:= D;
    D:= C;
    C:= XRTLROL32(B, 30);
    B:= A;
    A:= Temp;
  end;

  Inc(Digest[0], A);
  Inc(Digest[1], B);
  Inc(Digest[2], C);
  Inc(Digest[3], D);
  Inc(Digest[4], E);
end;

procedure TXRTLSHA.engineUpdateDigestBlockFinal(InBuffer: PByteArray;
  InAvail: Integer);
var
  Buf: TXRTLSHABuffer;
  WorkBuf: PInt64Array;
begin
  Inc(FCount, InAvail);
  ZeroMemory(@Buf, SizeOf(Buf));
  XRTLMoveMemory(InBuffer, @Buf, InAvail);
  Buf[InAvail]:= $80;
  if InAvail + 1 > XRTLSHABlockSize - 8 then
  begin
    engineUpdateDigestBlock(@Buf);
    ZeroMemory(@Buf, SizeOf(Buf));
  end;
  WorkBuf:= @Buf;
  WorkBuf^[7]:= XRTLSwapHiLo64(FCount * 8);
  engineUpdateDigestBlock(@Buf);
  PCardinal(@FBytes[0])^:= XRTLSwapHiLo32(Digest[0]);
  PCardinal(@FBytes[4])^:= XRTLSwapHiLo32(Digest[1]);
  PCardinal(@FBytes[8])^:= XRTLSwapHiLo32(Digest[2]);
  PCardinal(@FBytes[12])^:= XRTLSwapHiLo32(Digest[3]);
  PCardinal(@FBytes[16])^:= XRTLSwapHiLo32(Digest[4]);
end;

end.

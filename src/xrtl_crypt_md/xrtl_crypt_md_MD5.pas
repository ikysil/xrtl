unit xrtl_crypt_md_MD5;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Compat,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_BlockMessageDigest;

const
  XRTLMD5BlockSize = 64; // 512 bits
  XRTLMD5SumSize   = 16;

type
  PXRTLMD5Digest  = ^TXRTLMD5Digest;
  TXRTLMD5Digest  = array[0 .. 3] of Cardinal;

  PXRTLMD5Sum     = ^TXRTLMD5Sum;
  TXRTLMD5Sum     = array[0 .. XRTLMD5SumSize - 1] of Byte;

  PXRTLMD5Buffer  = ^TXRTLMD5Buffer;
  TXRTLMD5Buffer  = array[0 .. XRTLMD5BlockSize - 1] of Byte;

  TXRTLMD5 = class(TXRTLBlockMessageDigest)
  private
  protected
    Digest: TXRTLMD5Digest;
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

function F(X, Y, Z: Cardinal): Cardinal;
begin
  Result:= Z xor (X and (Y xor Z));
end;

function G(X, Y, Z: Cardinal): Cardinal;
begin
  Result:= F(Z, X, Y);
//  Result:= Y xor (Z and (X xor Y));
end;

function H(X, Y, Z: Cardinal): Cardinal;
begin
  Result:= X xor Y xor Z;
end;

function I(X, Y, Z: Cardinal): Cardinal;
begin
  Result:= Y xor (X or not Z);
end;

function FF(A, B, C, D, K, S, T: Cardinal): Cardinal;
begin
  A:= A + K + T + F(B, C, D);
  A:= (A shl S) or (A shr -S);
  Result:= A + B;
end;

function GG(A, B, C, D, K, S, T: Cardinal): Cardinal;
begin
  A:= A + K + T + G(B, C, D);
  A:= (A shl S) or (A shr -S);
  Result:= A + B;
end;

function HH(A, B, C, D, K, S, T: Cardinal): Cardinal;
begin
  A:= A + K + T + H(B, C, D);
  A:= (A shl S) or (A shr -S);
  Result:= A + B;
end;

function II(A, B, C, D, K, S, T: Cardinal): Cardinal;
begin
  A:= A + K + T + I(B, C, D);
  A:= (A shl S) or (A shr -S);
  Result:= A + B;
end;

{ TXRTLMD5 }

constructor TXRTLMD5.Create;
begin
  inherited Create(XRTLMD5SumSize, XRTLMD5BlockSize);
end;

procedure TXRTLMD5.engineInit;
begin
  inherited;
  Digest[0]:= $67452301;
  Digest[1]:= $EFCDAB89;
  Digest[2]:= $98BADCFE;
  Digest[3]:= $10325476;
  FCount:= 0;
end;

procedure TXRTLMD5.engineUpdateDigestBlock(InBuffer: PByteArray);
var
  M: PIntegerArray;
  A, B, C, D: Cardinal;
begin
  Inc(FCount, XRTLMD5BlockSize);
  M:= PIntegerArray(InBuffer);
  A:= Digest[0];
  B:= Digest[1];
  C:= Digest[2];
  D:= Digest[3];

  A:= FF(A,B,C,D,M[ 0], 7,$D76AA478);
  D:= FF(D,A,B,C,M[ 1],12,$E8C7B756);
  C:= FF(C,D,A,B,M[ 2],17,$242070DB);
  B:= FF(B,C,D,A,M[ 3],22,$C1BDCEEE);
  A:= FF(A,B,C,D,M[ 4], 7,$F57C0FAF);
  D:= FF(D,A,B,C,M[ 5],12,$4787C62A);
  C:= FF(C,D,A,B,M[ 6],17,$A8304613);
  B:= FF(B,C,D,A,M[ 7],22,$FD469501);
  A:= FF(A,B,C,D,M[ 8], 7,$698098D8);
  D:= FF(D,A,B,C,M[ 9],12,$8B44F7AF);
  C:= FF(C,D,A,B,M[10],17,$FFFF5BB1);
  B:= FF(B,C,D,A,M[11],22,$895CD7BE);
  A:= FF(A,B,C,D,M[12], 7,$6B901122);
  D:= FF(D,A,B,C,M[13],12,$FD987193);
  C:= FF(C,D,A,B,M[14],17,$A679438E);
  B:= FF(B,C,D,A,M[15],22,$49B40821);

  A:= GG(A,B,C,D,M[ 1], 5,$F61E2562);
  D:= GG(D,A,B,C,M[ 6], 9,$C040B340);
  C:= GG(C,D,A,B,M[11],14,$265E5A51);
  B:= GG(B,C,D,A,M[ 0],20,$E9B6C7AA);
  A:= GG(A,B,C,D,M[ 5], 5,$D62F105D);
  D:= GG(D,A,B,C,M[10], 9,$02441453);
  C:= GG(C,D,A,B,M[15],14,$D8A1E681);
  B:= GG(B,C,D,A,M[ 4],20,$E7D3FBC8);
  A:= GG(A,B,C,D,M[ 9], 5,$21E1CDE6);
  D:= GG(D,A,B,C,M[14], 9,$C33707D6);
  C:= GG(C,D,A,B,M[ 3],14,$F4D50D87);
  B:= GG(B,C,D,A,M[ 8],20,$455A14ED);
  A:= GG(A,B,C,D,M[13], 5,$A9E3E905);
  D:= GG(D,A,B,C,M[ 2], 9,$FCEFA3F8);
  C:= GG(C,D,A,B,M[ 7],14,$676F02D9);
  B:= GG(B,C,D,A,M[12],20,$8D2A4C8A);

  A:= HH(A,B,C,D,M[ 5], 4,$FFFA3942);
  D:= HH(D,A,B,C,M[ 8],11,$8771F681);
  C:= HH(C,D,A,B,M[11],16,$6D9D6122);
  B:= HH(B,C,D,A,M[14],23,$FDE5380C);
  A:= HH(A,B,C,D,M[ 1], 4,$A4BEEA44);
  D:= HH(D,A,B,C,M[ 4],11,$4BDECFA9);
  C:= HH(C,D,A,B,M[ 7],16,$F6BB4B60);
  B:= HH(B,C,D,A,M[10],23,$BEBFBC70);
  A:= HH(A,B,C,D,M[13], 4,$289B7EC6);
  D:= HH(D,A,B,C,M[ 0],11,$EAA127FA);
  C:= HH(C,D,A,B,M[ 3],16,$D4EF3085);
  B:= HH(B,C,D,A,M[ 6],23,$04881D05);
  A:= HH(A,B,C,D,M[ 9], 4,$D9D4D039);
  D:= HH(D,A,B,C,M[12],11,$E6DB99E5);
  C:= HH(C,D,A,B,M[15],16,$1FA27CF8);
  B:= HH(B,C,D,A,M[ 2],23,$C4AC5665);

  A:= II(A,B,C,D,M[ 0], 6,$F4292244);
  D:= II(D,A,B,C,M[ 7],10,$432AFF97);
  C:= II(C,D,A,B,M[14],15,$AB9423A7);
  B:= II(B,C,D,A,M[ 5],21,$FC93A039);
  A:= II(A,B,C,D,M[12], 6,$655B59C3);
  D:= II(D,A,B,C,M[ 3],10,$8F0CCC92);
  C:= II(C,D,A,B,M[10],15,$FFEFF47D);
  B:= II(B,C,D,A,M[ 1],21,$85845DD1);
  A:= II(A,B,C,D,M[ 8], 6,$6FA87E4F);
  D:= II(D,A,B,C,M[15],10,$FE2CE6E0);
  C:= II(C,D,A,B,M[ 6],15,$A3014314);
  B:= II(B,C,D,A,M[13],21,$4E0811A1);
  A:= II(A,B,C,D,M[ 4], 6,$F7537E82);
  D:= II(D,A,B,C,M[11],10,$BD3AF235);
  C:= II(C,D,A,B,M[ 2],15,$2AD7D2BB);
  B:= II(B,C,D,A,M[ 9],21,$EB86D391);

  Inc(Digest[0], A);
  Inc(Digest[1], B);
  Inc(Digest[2], C);
  Inc(Digest[3], D);
end;

procedure TXRTLMD5.engineUpdateDigestBlockFinal(InBuffer: PByteArray;
  InAvail: Integer);
var
  Buf: TXRTLMD5Buffer;
  WorkBuf: PInt64Array;
  LCount: Int64;
begin
  LCount:= FCount + InAvail;
  ZeroMemory(@Buf, SizeOf(Buf));
  XRTLMoveMemory(InBuffer, @Buf, InAvail);
  Buf[InAvail]:= $80;
  if InAvail + 1 > XRTLMD5BlockSize - 8 then
  begin
    engineUpdateDigestBlock(@Buf);
    ZeroMemory(@Buf, SizeOf(Buf));
  end;
  WorkBuf:= @Buf;
  WorkBuf^[7]:= LCount * 8;
  engineUpdateDigestBlock(@Buf);
  XRTLMoveMemory(@Digest, FBytes, XRTLMD5SumSize);
end;

class function TXRTLMD5.GetDisplayName: string;
begin
  Result:= 'MD5';
end;

end.

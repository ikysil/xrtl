unit xrtl_crypt_md_MD2;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_BlockMessageDigest;

const
  XRTLMD2BlockSize = 16; // 128 bits
  XRTLMD2SumSize   = 16; // 128 bits

type
  PXRTLMD2Buffer = ^TXRTLMD2Buffer;
  TXRTLMD2Sum    = array[0 .. XRTLMD2BlockSize - 1] of Byte;
  TXRTLMD2Buffer = array[0 .. XRTLMD2BlockSize - 1] of Byte;
  TXRTLMD2Work   = array[0 .. XRTLMD2BlockSize * 3 - 1] of Byte;

  TXRTLMD2 = class(TXRTLBlockMessageDigest)
  private
  protected
    FCheckSum: TXRTLMD2Sum;
    X: TXRTLMD2Work;
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

const
  S: array[0 .. 255] of Byte = (
     41,  46,  67, 201, 162, 216, 124,   1,  61,  54,  84, 161, 236, 240,   6,  19,
     98, 167,   5, 243, 192, 199, 115, 140, 152, 147,  43, 217, 188,  76, 130, 202,
     30, 155,  87,  60, 253, 212, 224,  22, 103,  66, 111,  24, 138,  23, 229,  18,
    190,  78, 196, 214, 218, 158, 222,  73, 160, 251, 245, 142, 187,  47, 238, 122,
    169, 104, 121, 145,  21, 178,   7,  63, 148, 194,  16, 137,  11,  34,  95,  33,
    128, 127,  93, 154,  90, 144,  50,  39,  53,  62, 204, 231, 191, 247, 151,   3,
    255,  25,  48, 179,  72, 165, 181, 209, 215,  94, 146,  42, 172,  86, 170, 198,
     79, 184,  56, 210, 150, 164, 125, 182, 118, 252, 107, 226, 156, 116,   4, 241,
     69, 157, 112,  89, 100, 113, 135,  32, 134,  91, 207, 101, 230,  45, 168,   2,
     27,  96,  37, 173, 174, 176, 185, 246,  28,  70,  97, 105,  52,  64, 126,  15,
     85,  71, 163,  35, 221,  81, 175,  58, 195,  92, 249, 206, 186, 197, 234,  38,
     44,  83,  13, 110, 133,  40, 132,   9, 211, 223, 205, 244,  65, 129,  77,  82,
    106, 220,  55, 200, 108, 193, 171, 250,  36, 225, 123,   8,  12, 189, 177,  74,
    120, 136, 149, 139, 227,  99, 232, 109, 233, 203, 213, 254,  59,   0,  29,  57,
    242, 239, 183,  14, 102,  88, 208, 228, 166, 119, 114, 248, 235, 117,  75,  10,
     49,  68,  80, 180, 143, 237,  31,  26, 219, 153, 141,  51, 159,  17, 131,  20
  );

{ TXRTLMD2 }

constructor TXRTLMD2.Create;
begin
  inherited Create(XRTLMD2SumSize, XRTLMD2BlockSize);
end;

procedure TXRTLMD2.engineInit;
begin
  inherited;
  ZeroMemory(@FCheckSum, SizeOf(FCheckSum));
  ZeroMemory(@X, SizeOf(X));
end;

procedure TXRTLMD2.engineUpdateDigestBlock(InBuffer: PByteArray);
var
  I, J: Integer;
  T: Byte;
begin
  for I:= 0 to XRTLMD2BlockSize - 1 do
  begin
    X[16 + I]:= InBuffer[I];
    X[32 + I]:= X[I] xor X[16 + I];
  end;
//  Encrypt block (18 rounds)
  T:= 0;
  for I:= 0 to 17 do
  begin
    for J:= 0 to 47 do
    begin
      X[J]:= X[J] xor S[T];
      T:= X[J];
    end;
    T:= (T + I) and $FF;
  end;
//  update checksum
  T:= FCheckSum[XRTLMD2BlockSize - 1];
  for I:= 0 to XRTLMD2BlockSize - 1 do
  begin
    FCheckSum[I]:= FCheckSum[I] xor S[InBuffer[I] xor T];
    T:= FCheckSum[I];
  end;
end;

procedure TXRTLMD2.engineUpdateDigestBlockFinal(InBuffer: PByteArray;
  InAvail: Integer);
var
  Buf: TXRTLMD2Buffer;
  PadLen: Byte;
begin
  PadLen:= XRTLMD2BlockSize - InAvail;
  FillMemory(@Buf, SizeOf(Buf), PadLen);
  XRTLMoveMemory(InBuffer, @Buf, InAvail);
  engineUpdateDigestBlock(@Buf);
  engineUpdateDigestBlock(@FCheckSum);
  XRTLMoveMemory(@X, FBytes, XRTLMD2SumSize);
end;

class function TXRTLMD2.GetDisplayName: string;
begin
  Result:= 'MD2';
end;

end.

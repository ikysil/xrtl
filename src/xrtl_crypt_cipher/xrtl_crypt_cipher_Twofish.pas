unit xrtl_crypt_cipher_Twofish;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Math,
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipher;

const
  XRTLTwofishBlockSize    = 16; // 128 bits
  XRTLTwofishInputWhiten  = 0;
  XRTLTwofishOutputWhiten = (XRTLTwofishInputWhiten + XRTLTwofishBlockSize div 4);
  XRTLTwofishRounds       = 16;
  XRTLTwofishRoundSubKeys = (XRTLTwofishOutputWhiten + XRTLTwofishBlockSize div 4);
  XRTLTwofishTotalSubKeys = (XRTLTwofishRoundSubKeys + XRTLTwofishRounds * 2);

type
  PXRTLTwofishBlock = ^TXRTLTwofishBlock;
  TXRTLTwofishBlock = array[0 .. 3] of LongWord;

  TXRTLTwofishCipher = class(TXRTLBlockCipher)
  private
    SubKeys: array[0 .. XRTLTwofishTotalSubKeys - 1] of DWORD;
    sboxKeys: array[0 .. 3] of DWORD;
    sbox: array[0 .. 3, 0 .. 255] of DWORD;
  protected
// size of cipher block in BYTES
    function   engineGetBlockSize: Integer; override;
// size of key in BYTES
    function   engineGetMaximumKeySize: Integer; override;
    procedure  engineSetKey; override;
  public
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    class function GetDisplayName: string; override;
  end;

implementation

uses
  xrtl_util_MemoryUtils;

{$I xrtl_crypt_cipher_Twofish.inc}

const
  RS_GF_FDBK  = $14d;
  MDS_GF_FDBK = $169;
  P_00 = 1;
  P_01 = 0;
  P_02 = 0;
  P_03 = (P_01 xor 1);
  P_04 = 1;
  P_10 = 0;
  P_11 = 0;
  P_12 = 1;
  P_13 = (P_11 xor 1);
  P_14 = 0;
  P_20 = 1;
  P_21 = 1;
  P_22 = 0;
  P_23 = (P_21 xor 1);
  P_24 = 0;
  P_30 = 0;
  P_31 = 1;
  P_32 = 1;
  P_33 = (P_31 xor 1);
  P_34 = 1;
  SK_STEP= $02020202;
  SK_BUMP= $01010101;
  SK_ROTL= 9;

var
  MDS: array[0 .. 3, 0 .. 255] of DWORD;

function LFSR1(x: DWord): DWord;
begin
  if (x and 1)<> 0 then
    Result:= (x shr 1) xor (MDS_GF_FDBK div 2)
  else
    Result:= (x shr 1);
end;

function LFSR2(x: DWord): DWord;
begin
  if (x and 2)<> 0 then
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2)
  else
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2);
end;

function Mul_X(x: DWord): DWord;
begin
  Result:= x xor LFSR2(x);
end;

function Mul_Y(x: DWord): DWord;
begin
  Result:= x xor LFSR1(x) xor LFSR2(x);
end;

function RS_MDS_Encode(lK0, lK1: DWord): DWord;
var
  lR, nI, nJ, lG2, lG3: DWord;
  bB: byte;
begin
  lR:= 0;
  for nI:= 0 to 1 do
  begin
    if nI<> 0  then
      lR:= lR xor lK0
    else
      lR:= lR xor lK1;
    for nJ:= 0 to 3 do
    begin
      bB:= lR shr 24;
      if (bB and $80)<> 0 then
        lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
      else
        lG2:= (bB shl 1) and $FF;
      if (bB and 1)<> 0 then
        lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
      else
        lG3:= ((bB shr 1) and $7f) xor lG2;
      lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
    end;
  end;
  Result:= lR;
end;

function f32(x: DWord; K32: PDWordArray; Len: DWord): DWord;
var
  t0, t1, t2, t3: DWord;
begin
  t0:= x and $FF;
  t1:= (x shr 8) and $FF;
  t2:= (x shr 16) and $FF;
  t3:= x shr 24;
  if Len= 256 then
  begin
    t0:= p8x8[p_04,t0] xor ((K32^[3]) and $FF);
    t1:= p8x8[p_14,t1] xor ((K32^[3] shr  8) and $FF);
    t2:= p8x8[p_24,t2] xor ((K32^[3] shr 16) and $FF);
    t3:= p8x8[p_34,t3] xor ((K32^[3] shr 24));
  end;
  if Len>= 192 then
  begin
    t0:= p8x8[p_03,t0] xor ((K32^[2]) and $FF);
    t1:= p8x8[p_13,t1] xor ((K32^[2] shr  8) and $FF);
    t2:= p8x8[p_23,t2] xor ((K32^[2] shr 16) and $FF);
    t3:= p8x8[p_33,t3] xor ((K32^[2] shr 24));
  end;
  Result:= MDS[0,p8x8[p_01,p8x8[p_02,t0] xor ((K32^[1]) and $FF)] xor ((K32^[0]) and $FF)] xor
           MDS[1,p8x8[p_11,p8x8[p_12,t1] xor ((K32^[1] shr  8) and $FF)] xor ((K32^[0] shr  8) and $FF)] xor
           MDS[2,p8x8[p_21,p8x8[p_22,t2] xor ((K32^[1] shr 16) and $FF)] xor ((K32^[0] shr 16) and $FF)] xor
           MDS[3,p8x8[p_31,p8x8[p_32,t3] xor ((K32^[1] shr 24))] xor ((K32^[0] shr 24))];
end;

procedure PreCompMDS;
var
  m1, mx, my: array[0..1] of DWord;
  nI: longint;
begin
  for nI:= 0 to 255 do
  begin
    m1[0]:= p8x8[0,nI];
    mx[0]:= Mul_X(m1[0]);
    my[0]:= Mul_Y(m1[0]);
    m1[1]:= p8x8[1,nI];
    mx[1]:= Mul_X(m1[1]);
    my[1]:= Mul_Y(m1[1]);
    MDS[0,nI]:= (m1[P_00] shl 0) or
                (mx[p_00] shl 8) or
                (my[p_00] shl 16) or
                (my[p_00] shl 24);
    MDS[1,nI]:= (my[p_10] shl 0) or
                (my[p_10] shl 8) or
                (mx[p_10] shl 16) or
                (m1[p_10] shl 24);
    MDS[2,nI]:= (mx[p_20] shl 0) or
                (my[p_20] shl 8) or
                (m1[p_20] shl 16) or
                (my[p_20] shl 24);
    MDS[3,nI]:= (mx[p_30] shl 0) or
                (m1[p_30] shl 8) or
                (my[p_30] shl 16) or
                (mx[p_30] shl 24);
  end;
end;

procedure Xor256(Dst, Src: PDWordArray; v: byte);
var
  i: DWord;
begin
  for i:= 0 to 63 do
    Dst[i]:= Src[i] xor (v * $01010101);
end;

{ TXRTLTwofishCipher }

function TXRTLTwofishCipher.engineGetBlockSize: Integer;
begin
  Result:= XRTLTwofishBlockSize; // 128 bits
end;

function TXRTLTwofishCipher.engineGetMaximumKeySize: Integer;
begin
  Result:= 32; // 256 bits
end;

procedure TXRTLTwofishCipher.engineSetKey;
var
  Key32: array[0 .. 7] of DWORD;
  k32e, k32o: array[0 .. 3] of DWORD;
  k64Cnt, i, j, A, B, q: DWORD;
  L0, L1: array[0 .. 255] of Byte;
  Size: Integer;
begin
  FillChar(Key32, Sizeof(Key32), 0);
  XRTLMoveMemory(Key, @Key32, Min(SizeOf(Key32), KeySize));
  Size:= KeySize * 8;
  if Size <= 128 then           {pad the size to either 128bit, 192bit or 256bit}
    Size:= 128
  else
    if Size<= 192 then
      Size:= 192
    else
      Size:= 256;
  k64Cnt:= Size div 64;
  j:= k64Cnt - 1;
  for i:= 0 to j do
  begin
    k32e[i]:= Key32[2 * i];
    k32o[i]:= Key32[2 * i + 1];
    sboxKeys[j - i]:= RS_MDS_Encode(k32e[i], k32o[i]);
  end;
  q:= 0;
  for i:= 0 to (XRTLTwofishTotalSubKeys div 2) - 1 do
  begin
    A:= f32(q, @k32e, Size);
    B:= f32(q + SK_BUMP, @k32o, Size);
    B:= XRTLROL32(B, 8);
    SubKeys[2 * i]:= A + B;
    B:= A + 2 * B;
    SubKeys[2 * i + 1]:= XRTLROL32(B, SK_ROTL);
    Inc(q, SK_STEP);
  end;
  case Size of
    128:
    begin
      Xor256(@L0, @p8x8[p_02], sboxKeys[1] and $FF);
      A:= sboxKeys[0] and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[p_01,L0[i]] xor A];
        sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[p_01,L0[i+1]] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_12],(sboxKeys[1] shr 8) and $FF);
      A:= (sboxKeys[0] shr 8) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[p_11,L0[i]] xor A];
        sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[p_11,L0[i+1]] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_22],(sboxKeys[1] shr 16) and $FF);
      A:= (sboxKeys[0] shr 16) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[p_21,L0[i]] xor A];
        sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[p_21,L0[i+1]] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_32],(sboxKeys[1] shr 24));
      A:= (sboxKeys[0] shr 24);
      i:= 0;
      while i< 256 do
      begin
        sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[p_31,L0[i]] xor A];
        sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[p_31,L0[i+1]] xor A];
        Inc(i,2);
      end;
    end;
    192:
    begin
      Xor256(@L0,@p8x8[p_03],sboxKeys[2] and $FF);
      A:= sboxKeys[0] and $FF;
      B:= sboxKeys[1] and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i]] xor B] xor A];
        sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_13],(sboxKeys[2] shr 8) and $FF);
      A:= (sboxKeys[0] shr 8) and $FF;
      B:= (sboxKeys[1] shr 8) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i]] xor B] xor A];
        sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_23],(sboxKeys[2] shr 16) and $FF);
      A:= (sboxKeys[0] shr 16) and $FF;
      B:= (sboxKeys[1] shr 16) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i]] xor B] xor A];
        sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L0,@p8x8[p_33],(sboxKeys[2] shr 24));
      A:= (sboxKeys[0] shr 24);
      B:= (sboxKeys[1] shr 24);
      i:= 0;
      while i< 256 do
      begin
        sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i]] xor B] xor A];
        sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
    end;
    256:
    begin
      Xor256(@L1,@p8x8[p_04],(sboxKeys[3]) and $FF);
      i:= 0;
      while i< 256 do
      begin
        L0[i  ]:= p8x8[p_03,L1[i]];
        L0[i+1]:= p8x8[p_03,L1[i+1]];
        Inc(i,2);
      end;
      Xor256(@L0,@L0,(sboxKeys[2]) and $FF);
      A:= (sboxKeys[0]) and $FF;
      B:= (sboxKeys[1]) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i]] xor B] xor A];
        sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L1,@p8x8[p_14],(sboxKeys[3] shr  8) and $FF);
      i:= 0;
      while i< 256 do
      begin
        L0[i  ]:= p8x8[p_13,L1[i]];
        L0[i+1]:= p8x8[p_13,L1[i+1]];
        Inc(i,2);
      end;
      Xor256(@L0,@L0,(sboxKeys[2] shr  8) and $FF);
      A:= (sboxKeys[0] shr  8) and $FF;
      B:= (sboxKeys[1] shr  8) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i]] xor B] xor A];
        sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L1,@p8x8[p_24],(sboxKeys[3] shr 16) and $FF);
      i:= 0;
      while i< 256 do
      begin
        L0[i  ]:= p8x8[p_23,L1[i]];
        L0[i+1]:= p8x8[p_23,L1[i+1]];
        Inc(i,2);
      end;
      Xor256(@L0,@L0,(sboxKeys[2] shr 16) and $FF);
      A:= (sboxKeys[0] shr 16) and $FF;
      B:= (sboxKeys[1] shr 16) and $FF;
      i:= 0;
      while i< 256 do
      begin
        sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i]] xor B] xor A];
        sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
      Xor256(@L1,@p8x8[p_34],(sboxKeys[3] shr 24));
      i:= 0;
      while i< 256 do
      begin
        L0[i  ]:= p8x8[p_33,L1[i]];
        L0[i+1]:= p8x8[p_33,L1[i+1]];
        Inc(i,2);
      end;
      Xor256(@L0,@L0,(sboxKeys[2] shr 24));
      A:= (sboxKeys[0] shr 24);
      B:= (sboxKeys[1] shr 24);
      i:= 0;
      while i< 256 do
      begin
        sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i]] xor B] xor A];
        sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+1]] xor B] xor A];
        Inc(i,2);
      end;
    end;
  end;
end;

procedure TXRTLTwofishCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  InBlock, OutBlock: PXRTLTwofishBlock;
  i: longint;
  t0, t1: DWord;
  X: TXRTLTwofishBlock;
begin
  InBlock:= PXRTLTwofishBlock(InBuffer);
  OutBlock:= PXRTLTwofishBlock(OutBuffer);
  X[0]:= InBlock[0] xor SubKeys[XRTLTwofishInputWhiten];
  X[1]:= InBlock[1] xor SubKeys[XRTLTwofishInputWhiten + 1];
  X[2]:= InBlock[2] xor SubKeys[XRTLTwofishInputWhiten + 2];
  X[3]:= InBlock[3] xor SubKeys[XRTLTwofishInputWhiten + 3];
  i:= 0;
  while i <= XRTLTwofishRounds - 2 do
  begin
    t0:= sBox[0,2*(x[0] and $ff)] xor sBox[0,2*(((x[0]) shr 8) and $ff)+1]
      xor sBox[2,2*((x[0] shr 16) and $ff)] xor sBox[2,2*((x[0] shr 24) and $ff)+1];
    t1:= sBox[0,2*((x[1] shr 24) and $ff)] xor sBox[0,2*(x[1] and $ff)+1]
      xor sBox[2,2*((x[1] shr 8) and $ff)] xor sBox[2,2*((x[1] shr 16) and $ff)+1];
    x[3]:= XRTLROL32(x[3],1);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[XRTLTwofishRoundSubKeys+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[XRTLTwofishRoundSubKeys+2*i+1]);
    x[2]:= XRTLROR32(x[2],1);
    t0:= sBox[0,2*(x[2] and $ff)] xor sBox[0,2*((x[2] shr 8) and $ff)+1]
      xor sBox[2,2*((x[2] shr 16) and $ff)] xor sBox[2,2*((x[2] shr 24) and $ff)+1];
    t1:= sBox[0,2*((x[3] shr 24) and $ff)] xor sBox[0,2*(x[3] and $ff)+1]
      xor sBox[2,2*((x[3] shr 8) and $ff)] xor sBox[2,2*((x[3] shr 16) and $ff)+1];
    x[1]:= XRTLROL32(x[1],1);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[XRTLTwofishRoundSubKeys+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[XRTLTwofishRoundSubKeys+2*(i+1)+1]);
    x[0]:= XRTLROR32(x[0],1);
    Inc(i,2);
  end;
  OutBlock[0]:= X[2] xor SubKeys[XRTLTwofishOutputWhiten];
  OutBlock[1]:= X[3] xor SubKeys[XRTLTwofishOutputWhiten + 1];
  OutBlock[2]:= X[0] xor SubKeys[XRTLTwofishOutputWhiten + 2];
  OutBlock[3]:= X[1] xor SubKeys[XRTLTwofishOutputWhiten + 3];
end;

procedure TXRTLTwofishCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  InBlock, OutBlock: PXRTLTwofishBlock;
  i: longint;
  t0, t1: DWord;
  X: TXRTLTwofishBlock;
begin
  InBlock:= PXRTLTwofishBlock(InBuffer);
  OutBlock:= PXRTLTwofishBlock(OutBuffer);
  X[2]:= InBlock[0] xor SubKeys[XRTLTwofishOutputWhiten];
  X[3]:= InBlock[1] xor SubKeys[XRTLTwofishOutputWhiten + 1];
  X[0]:= InBlock[2] xor SubKeys[XRTLTwofishOutputWhiten + 2];
  X[1]:= InBlock[3] xor SubKeys[XRTLTwofishOutputWhiten + 3];
  i:= XRTLTwofishRounds - 2;
  while i >= 0 do
  begin
    t0:= sBox[0,2*(x[2] and $ff)] xor sBox[0,2*((x[2] shr 8) and $ff)+1]
      xor sBox[2,2*((x[2] shr 16) and $ff)] xor sBox[2,2*((x[2] shr 24) and $ff)+1];
    t1:= sBox[0,2*((x[3] shr 24) and $ff)] xor sBox[0,2*(x[3] and $ff)+1]
      xor sBox[2,2*((x[3] shr 8) and $ff)] xor sBox[2,2*((x[3] shr 16) and $ff)+1];
    x[0]:= XRTLROL32(x[0],1);
    x[0]:= x[0] xor (t0 +   t1 + Subkeys[XRTLTwofishRoundSubKeys+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + Subkeys[XRTLTwofishRoundSubKeys+2*(i+1)+1]);
    x[1]:= XRTLROR32(x[1],1);
    t0:= sBox[0,2*(x[0] and $ff)] xor sBox[0,2*((x[0] shr 8) and $ff)+1]
      xor sBox[2,2*((x[0] shr 16) and $ff)] xor sBox[2,2*((x[0] shr 24) and $ff)+1];
    t1:= sBox[0,2*((x[1] shr 24) and $ff)] xor sBox[0,2*(x[1] and $ff)+1]
      xor sBox[2,2*((x[1] shr 8) and $ff)] xor sBox[2,2*((x[1] shr 16) and $ff)+1];
    x[2]:= XRTLROL32(x[2],1);
    x[2]:= x[2] xor (t0 +   t1 + Subkeys[XRTLTwofishRoundSubKeys+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + Subkeys[XRTLTwofishRoundSubKeys+2*i+1]);
    x[3]:= XRTLROR32(x[3],1);
    Dec(i,2);
  end;
  OutBlock[0]:= X[0] xor SubKeys[XRTLTwofishInputWhiten];
  OutBlock[1]:= X[1] xor SubKeys[XRTLTwofishInputWhiten + 1];
  OutBlock[2]:= X[2] xor SubKeys[XRTLTwofishInputWhiten + 2];
  OutBlock[3]:= X[3] xor SubKeys[XRTLTwofishInputWhiten + 3];
end;

class function TXRTLTwofishCipher.GetDisplayName: string;
begin
  Result:= 'Twofish';
end;

initialization
  PreCompMDS;

end.


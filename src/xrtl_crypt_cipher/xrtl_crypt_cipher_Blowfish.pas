unit xrtl_crypt_cipher_Blowfish;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_util_CPUUtils,
  xrtl_crypt_BlockCipher;

type
  TPType = array[0 .. 17] of LongWord;
  TSType = array[0 .. 3, 0 .. 255] of LongWord;

  PXRTLBlowfishBlock = ^TXRTLBlowfishBlock;
  TXRTLBlowfishBlock = array[0 .. 1] of LongWord;

  TXRTLBlowfishCipher = class(TXRTLBlockCipher)
  private
    PBox: TPType;
    SBox: TSType;
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

{$I xrtl_crypt_cipher_Blowfish.inc}

{ TXRTLBlowfishCipher }

function TXRTLBlowfishCipher.engineGetBlockSize: Integer;
begin
  Result:= 8;  // 64 bits
end;

function TXRTLBlowfishCipher.engineGetMaximumKeySize: Integer;
begin
  Result:= 56; // 448 bits
end;

procedure TXRTLBlowfishCipher.engineSetKey;
var
  I, K: LongInt;
  A: DWord;
  Block: array[0 .. 7] of Byte;
begin
  Move(InitS, SBox, SizeOf(SBox));
  Move(InitP, PBox, SizeOf(PBox));
  K:= 0;
  if KeySize > 0 then
  begin
    for I:= 0 to 17 do
    begin
      A:= DWord(Key[(K + 3) mod KeySize]);
      A:= A + (DWord(Key[(K + 2) mod KeySize]) shl 8);
      A:= A + (DWord(Key[(K + 1) mod KeySize]) shl 16);
      A:= A + (DWord(Key[K]) shl 24);
      PBox[I]:= PBox[I] xor A;
      K:= (K + 4) mod KeySize;
    end;
  end;
  FillChar(Block, SizeOf(Block), 0);
  for I:= 0 to 8 do
  begin
    engineUpdateEncipherBlockECB(@Block, @Block);
    PBox[I * 2]:=     (DWord(Block[3])) +
                      (DWord(Block[2]) shl 8) +
                      (DWord(Block[1]) shl 16) +
                      (DWord(Block[0]) shl 24);
    PBox[I * 2 + 1]:= (DWord(Block[7])) +
                      (DWord(Block[6]) shl 8) +
                      (DWord(Block[5]) shl 16) +
                      (DWord(Block[4]) shl 24);
  end;
  for K:= 0 to 3 do
  begin
    for I:= 0 to 127 do
    begin
      engineUpdateEncipherBlockECB(@Block, @Block);
      SBox[K, I * 2]:=     (DWord(Block[3])) +
                           (DWord(Block[2]) shl 8) +
                           (DWord(Block[1]) shl 16) +
                           (DWord(Block[0]) shl 24);
      SBox[K, I * 2 + 1]:= (DWord(Block[7])) +
                           (DWord(Block[6]) shl 8) +
                           (DWord(Block[5]) shl 16) +
                           (DWord(Block[4]) shl 24);
    end;
  end;
end;

procedure TXRTLBlowfishCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  InBlock, OutBlock: PXRTLBlowfishBlock;
  xL, xR: DWord;
begin
  InBlock:= PXRTLBlowfishBlock(InBuffer);
  OutBlock:= PXRTLBlowfishBlock(OutBuffer);
  xL:= XRTLSwapHiLo32(InBlock[0]);
  xR:= XRTLSwapHiLo32(InBlock[1]);
  xL:= xL xor PBox[0];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[1];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[2];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[3];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[4];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[5];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[6];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[7];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[8];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[9];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[10];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[11];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[12];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[13];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[14];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[15];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[16];
  xR:= xR xor PBox[17];
  OutBlock[0]:= XRTLSwapHiLo32(xR);
  OutBlock[1]:= XRTLSwapHiLo32(xL);
end;

procedure TXRTLBlowfishCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
var
  InBlock, OutBlock: PXRTLBlowfishBlock;
  xL, xR: DWord;
begin
  InBlock:= PXRTLBlowfishBlock(InBuffer);
  OutBlock:= PXRTLBlowfishBlock(OutBuffer);
  xL:= XRTLSwapHiLo32(InBlock[0]);
  xR:= XRTLSwapHiLo32(InBlock[1]);
  xL:= xL xor PBox[17];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[16];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[15];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[14];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[13];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[12];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[11];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[10];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[9];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[8];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[7];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[6];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[5];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[4];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[3];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[2];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[1];
  xR:= xR xor PBox[0];
  OutBlock[0]:= XRTLSwapHiLo32(xR);
  OutBlock[1]:= XRTLSwapHiLo32(xL);
end;

class function TXRTLBlowfishCipher.GetDisplayName: string;
begin
  Result:= 'Blowfish';
end;

end.


unit xrtl_crypt_test_AES;

{$INCLUDE xrtl.inc}

interface

procedure TestTXRTLAESCipher;

implementation

uses
  SysUtils,
  xrtl_crypt_cipher_AES;

procedure TestTXRTLAESCipher;
const
  Key1: array[0 .. 15] of byte=
    ($00,$01,$02,$03,$05,$06,$07,$08,$0A,$0B,$0C,$0D,$0F,$10,$11,$12);
  InData1: array[0 .. 15] of byte=
    ($50,$68,$12,$A4,$5F,$08,$C8,$89,$B9,$7F,$59,$80,$03,$8B,$83,$59);
  OutData1: array[0 .. 15] of byte=
    ($D8,$F5,$32,$53,$82,$89,$EF,$7D,$06,$B5,$06,$A4,$FD,$5B,$E9,$C9);

  Key2: array[0 .. 23] of byte=
    ($A0,$A1,$A2,$A3,$A5,$A6,$A7,$A8,$AA,$AB,$AC,$AD,$AF,$B0,$B1,$B2,
     $B4,$B5,$B6,$B7,$B9,$BA,$BB,$BC);
  InData2: array[0 .. 15] of byte=
    ($4F,$1C,$76,$9D,$1E,$5B,$05,$52,$C7,$EC,$A8,$4D,$EA,$26,$A5,$49);
  OutData2: array[0 .. 15] of byte=
    ($F3,$84,$72,$10,$D5,$39,$1E,$23,$60,$60,$8E,$5A,$CB,$56,$05,$81);
    
  Key3: array[0 .. 31] of byte=
    ($00,$01,$02,$03,$05,$06,$07,$08,$0A,$0B,$0C,$0D,$0F,$10,$11,$12,
     $14,$15,$16,$17,$19,$1A,$1B,$1C,$1E,$1F,$20,$21,$23,$24,$25,$26);
  InData3: array[0 .. 15] of byte=
    ($5E,$25,$CA,$78,$F0,$DE,$55,$80,$25,$24,$D3,$8D,$A3,$FE,$44,$56);
  OutData3: array[0 .. 15] of byte=
    ($E8,$B7,$2B,$4E,$8B,$E2,$43,$43,$8C,$9F,$FF,$1F,$0E,$20,$58,$72);
var
  Cipher: TXRTLAESCipher;
  Data: array[0 .. 15] of Byte;
  Result: Boolean;
begin
  Cipher:= nil;
  try
//  test using 128 bits key
    Cipher:= TXRTLAESCipher.Create;
    Cipher.Reset;
    Cipher.SetKey(@Key1, SizeOf(Key1));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData1, @Data);
    Result:= CompareMem(@Data, @OutData1, Sizeof(OutData1));
    Cipher.Reset;
    Cipher.SetKey(@Key1, SizeOf(Key1));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData1, SizeOf(InData1)) and Result;
//  test using 192 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key2, SizeOf(Key2));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData2, @Data);
    Result:= CompareMem(@Data, @OutData2, SizeOf(OutData2)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key2, SizeOf(Key2));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData2, SizeOf(InData2)) and Result;
//  test using 256 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key3, SizeOf(Key3));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData3, @Data);
    Result:= CompareMem(@Data, @OutData3, SizeOf(OutData3)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key3, SizeOf(Key3));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData3, SizeOf(InData3)) and Result;
    if not Result then
      raise Exception.CreateFmt('Test for %s failed...', [Cipher.ClassName]);
  finally
    FreeAndNil(Cipher);
  end;
end;

end.

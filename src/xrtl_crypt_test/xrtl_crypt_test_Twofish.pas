unit xrtl_crypt_test_Twofish;

{$INCLUDE xrtl.inc}

interface

procedure TestTXRTLTwofishCipher;

implementation

uses
  SysUtils,
  xrtl_crypt_cipher_Twofish;

procedure TestTXRTLTwofishCipher;
const
  Key1:     array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData1:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0 .. 15] of Byte = ($9F,$58,$9F,$5C,$F6,$12,$2C,$32,$B6,$BF,$EC,$2F,$2A,$E8,$C3,$5A);

  Key2:     array[0 .. 23] of Byte = ($01,$23,$45,$67,$89,$AB,$CD,$EF,$FE,$DC,$BA,$98,$76,$54,$32,$10,$00,$11,$22,$33,$44,$55,$66,$77);
  InData2:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData2: array[0 .. 15] of Byte = ($CF,$D1,$D2,$E5,$A9,$BE,$9C,$DF,$50,$1F,$13,$B8,$92,$BD,$22,$48);

  Key3:     array[0 .. 31] of Byte = ($01,$23,$45,$67,$89,$AB,$CD,$EF,$FE,$DC,$BA,$98,$76,$54,$32,$10,$00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$AA,$BB,$CC,$DD,$EE,$FF);
  InData3:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData3: array[0 .. 15] of Byte = ($37,$52,$7B,$E0,$05,$23,$34,$B8,$9F,$0C,$FC,$CA,$E8,$7C,$FA,$20);
var
  Cipher: TXRTLTwofishCipher;
  Data: array[0 .. 15] of Byte;
  Result: Boolean;
begin
  Cipher:= nil;
  try
//  test using 128 bits key
    Cipher:= TXRTLTwofishCipher.Create;
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


unit xrtl_crypt_test_Anubis;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_crypt_BlockCipher;

procedure TestTXRTLAnubisCipher;

implementation

uses
  xrtl_crypt_cipher_Anubis;

procedure TestTXRTLAnubisCipher;
const
  Key1:     array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData1:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0 .. 15] of Byte = ($0A,$58,$F9,$C5,$67,$65,$7D,$EE,$8D,$95,$7B,$10,$71,$DA,$86,$95);

  Key2:     array[0 .. 19] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData2:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData2: array[0 .. 15] of Byte = ($1E,$0B,$3D,$2D,$13,$40,$76,$2A,$0B,$F5,$F6,$CF,$BB,$E1,$B0,$BE);

  Key3:     array[0 .. 23] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData3:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData3: array[0 .. 15] of Byte = ($FF,$51,$AC,$78,$33,$82,$B6,$A6,$54,$53,$B4,$93,$F0,$F6,$8B,$B3);

  Key4:     array[0 .. 27] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData4:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData4: array[0 .. 15] of Byte = ($45,$86,$D1,$05,$7C,$E5,$89,$37,$E4,$68,$48,$72,$8D,$33,$E9,$88);

  Key5:     array[0 .. 31] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData5:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData5: array[0 .. 15] of Byte = ($E5,$39,$19,$40,$AE,$20,$9C,$9E,$D3,$F2,$6B,$B9,$B2,$72,$D0,$84);

  Key6:     array[0 .. 35] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData6:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData6: array[0 .. 15] of Byte = ($0A,$EB,$FA,$77,$61,$7B,$A9,$8B,$54,$40,$A3,$0A,$97,$0B,$F7,$6D);

  Key7:     array[0 .. 39] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData7:  array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  OutData7: array[0 .. 15] of Byte = ($46,$F7,$83,$20,$F5,$61,$5E,$35,$E6,$B4,$79,$37,$00,$34,$E0,$ED);
var
  Cipher: TXRTLAnubisCipher;
  Data: array[0 .. 15] of Byte;
  Result: Boolean;
begin
  Cipher:= nil;
  try
//  test using 128 bits key
    Cipher:= TXRTLAnubisCipher.Create;
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
//  test using 160 bits key
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
//  test using 192 bits key
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
//  test using 224 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key4, SizeOf(Key4));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData4, @Data);
    Result:= CompareMem(@Data, @OutData4, SizeOf(OutData4)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key4, SizeOf(Key4));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData4, SizeOf(InData4)) and Result;
//  test using 256 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key5, SizeOf(Key5));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData5, @Data);
    Result:= CompareMem(@Data, @OutData5, SizeOf(OutData5)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key5, SizeOf(Key5));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData5, SizeOf(InData5)) and Result;
//  test using 288 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key6, SizeOf(Key6));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData6, @Data);
    Result:= CompareMem(@Data, @OutData6, SizeOf(OutData6)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key6, SizeOf(Key6));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData6, SizeOf(InData6)) and Result;
//  test using 320 bits key
    Cipher.Reset;
    Cipher.SetKey(@Key7, SizeOf(Key7));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData7, @Data);
    Result:= CompareMem(@Data, @OutData7, SizeOf(OutData7)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key7, SizeOf(Key7));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData7, SizeOf(InData7)) and Result;
    if not Result then
      raise Exception.CreateFmt('Test for %s failed...', [Cipher.ClassName]);
  finally
    FreeAndNil(Cipher);
  end;
end;

end.

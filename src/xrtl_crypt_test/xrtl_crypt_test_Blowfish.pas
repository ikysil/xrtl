unit xrtl_crypt_test_Blowfish;

{$INCLUDE xrtl.inc}

interface

procedure TestTXRTLBlowfishCipher;

implementation

uses
  SysUtils,
  xrtl_crypt_cipher_Blowfish;

procedure TestTXRTLBlowfishCipher;
const
  Key1:     array[0 .. 7] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00);
  InData1:  array[0 .. 7] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0 .. 7] of Byte = ($4E,$F9,$97,$45,$61,$98,$DD,$78);

  Key2:     array[0 .. 7] of Byte = ($7C,$A1,$10,$45,$4A,$1A,$6E,$57);
  InData2:  array[0 .. 7] of Byte = ($01,$A1,$D6,$D0,$39,$77,$67,$42);
  OutData2: array[0 .. 7] of Byte = ($59,$C6,$82,$45,$EB,$05,$28,$2B);
var
  Cipher: TXRTLBlowfishCipher;
  Data: array[0 .. 7] of Byte;
  Result: Boolean;
begin
  Cipher:= nil;
  try
    Cipher:= TXRTLBlowfishCipher.Create;
    Cipher.Reset;
    Cipher.SetKey(@Key1, SizeOf(Key1));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData1, @Data);
    Result:= CompareMem(@Data, @OutData1, Sizeof(Data));
    Cipher.Reset;
    Cipher.SetKey(@Key1, SizeOf(Key1));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData1, SizeOf(Data)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key2, SizeOf(Key2));
    Cipher.InitEncipher;
    Cipher.engineUpdateEncipherBlockECB(@InData2, @Data);
    Result:= CompareMem(@Data, @OutData2, SizeOf(Data)) and Result;
    Cipher.Reset;
    Cipher.SetKey(@Key2, SizeOf(Key2));
    Cipher.InitDecipher;
    Cipher.engineUpdateDecipherBlockECB(@Data, @Data);
    Result:= CompareMem(@Data, @InData2, SizeOf(Data)) and Result;
    if not Result then
      raise Exception.CreateFmt('Test for %s failed...', [Cipher.ClassName]);
  finally
    FreeAndNil(Cipher);
  end;
end;

end.


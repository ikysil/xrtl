unit xrtl_crypt_test_Khazad;

{$INCLUDE xrtl.inc}

interface

procedure TestTXRTLKhazadCipher;

implementation

uses
  SysUtils,
  xrtl_crypt_cipher_Khazad;

procedure TestTXRTLKhazadCipher;
const
  Key1:     array[0 .. 15] of Byte = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  InData1:  array[0 .. 7] of Byte =  ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0 .. 7] of Byte =  ($23,$25,$D0,$0F,$3E,$76,$A2,$2D);

var
  Cipher: TXRTLKhazadCipher;
  Data: array[0 .. 15] of Byte;
  Result: Boolean;
begin
  Cipher:= nil;
  try
//  test using 128 bits key
    Cipher:= TXRTLKhazadCipher.Create;
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
    if not Result then
      raise Exception.CreateFmt('Test for %s failed...', [Cipher.ClassName]);
  finally
    FreeAndNil(Cipher);
  end;
end;

end.

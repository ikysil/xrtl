program CryptTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  xrtl_crypt_test_AES,
  xrtl_crypt_test_Anubis,
  xrtl_crypt_test_Blowfish,
  xrtl_crypt_test_Khazad,
  xrtl_crypt_test_Rijndael,
  xrtl_crypt_test_Twofish,
  xrtl_crypt_test_MD2,
  xrtl_crypt_test_MD4,
  xrtl_crypt_test_MD5,
  xrtl_crypt_test_RIPEMD128,
  xrtl_crypt_test_RIPEMD160,
  xrtl_crypt_test_SHA0,
  xrtl_crypt_test_SHA1;

type
  TTestProc = procedure;
  TTest = record
    Name: string;
    TestProc: TTestProc;
  end;

const
  Tests: array[0 .. 12] of TTest = (
    (Name: 'AES'; TestProc: TestTXRTLAESCipher),
    (Name: 'Anubis'; TestProc: TestTXRTLAnubisCipher),
    (Name: 'Blowfish'; TestProc: TestTXRTLBlowfishCipher),
    (Name: 'Khazad'; TestProc: TestTXRTLKhazadCipher),
    (Name: 'Rijndael (128)'; TestProc: TestTXRTLRijndael128Cipher),
    (Name: 'Twofish'; TestProc: TestTXRTLTwofishCipher),
    (Name: 'MD2'; TestProc: TestTXRTLMD2),
    (Name: 'MD4'; TestProc: TestTXRTLMD4),
    (Name: 'MD5'; TestProc: TestTXRTLMD5),
    (Name: 'RIPEMD128'; TestProc: TestTXRTLRIPEMD128),
    (Name: 'RIPEMD160'; TestProc: TestTXRTLRIPEMD160),
    (Name: 'SHA0'; TestProc: TestTXRTLSHA0),
    (Name: 'SHA1'; TestProc: TestTXRTLSHA1)
  );

var
  I: Integer;
begin
  WriteLn('XRTL Crypt Test');
  for I:= 0 to High(Tests) do
  begin
    Write('Testing ', Tests[I].Name);
    try
      Tests[I].TestProc;
      WriteLn(' succeeded');
    except
      on E: Exception do
        WriteLn(' failed, message: ', E.Message);
    end;
  end;
  WriteLn('Press <Enter> to exit...');
  ReadLn;
end.

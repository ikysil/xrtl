unit xrtl_crypt_test_MD5;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_MD5;

procedure TestTXRTLMD5;

implementation

procedure TestTXRTLMD5;
const
  InData1: string = '';
  OutData1: TXRTLMD5Sum = (
    $D4,$1D,$8C,$D9,$8F,$00,$B2,$04,$E9,$80,$09,$98,$EC,$F8,$42,$7E
  );

  InData2: string = 'a';
  OutData2: TXRTLMD5Sum = (
    $0C,$C1,$75,$B9,$C0,$F1,$B6,$A8,$31,$C3,$99,$E2,$69,$77,$26,$61
  );

  InData3: string = 'abc';
  OutData3: TXRTLMD5Sum = (
    $90,$01,$50,$98,$3C,$D2,$4F,$B0,$D6,$96,$3F,$7D,$28,$E1,$7F,$72
  );

  InData4: string = 'message digest';
  OutData4: TXRTLMD5Sum = (
    $F9,$6B,$69,$7D,$7C,$B7,$93,$8D,$52,$5A,$2F,$31,$AA,$F1,$61,$D0
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLMD5Sum = (
    $C3,$FC,$D3,$D7,$61,$92,$E4,$00,$7D,$FB,$49,$6C,$CA,$67,$E1,$3B
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLMD5Sum = (
    $D1,$74,$AB,$98,$D2,$77,$D9,$F5,$A5,$61,$1C,$2C,$9F,$41,$9D,$9F
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLMD5Sum = (
    $57,$ED,$F4,$A2,$2B,$E3,$C9,$55,$AC,$49,$DA,$2E,$21,$07,$B6,$7A
  );

  function Test(MD: TXRTLMD5; InData: string; OutData: PXRTLMD5Buffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLMD5SumSize);
  end;

var
  MD: TXRTLMD5;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLMD5.Create;
    Result:= Test(MD, InData1, @OutData1);
    Result:= Test(MD, InData2, @OutData2) and Result;
    Result:= Test(MD, InData3, @OutData3) and Result;
    Result:= Test(MD, InData4, @OutData4) and Result;
    Result:= Test(MD, InData5, @OutData5) and Result;
    Result:= Test(MD, InData6, @OutData6) and Result;
    Result:= Test(MD, InData7, @OutData7) and Result;
    if not Result then
      raise Exception.CreateFmt('Test for %s failed...', [MD.ClassName]);
  finally
    FreeAndNil(MD);
  end;
end;

end.

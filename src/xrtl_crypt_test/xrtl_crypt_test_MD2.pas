unit xrtl_crypt_test_MD2;

{$INCLUDE xrtl.inc}

interface

procedure TestTXRTLMD2;

implementation

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest,
  xrtl_crypt_md_MD2;

procedure TestTXRTLMD2;
const
  InData1: string = '';
  OutData1: TXRTLMD2Sum = (
    $83,$50,$E5,$A3,$E2,$4C,$15,$3D,$F2,$27,$5C,$9F,$80,$69,$27,$73
  );

  InData2: string = 'a';
  OutData2: TXRTLMD2Sum = (
    $32,$EC,$01,$EC,$4A,$6D,$AC,$72,$C0,$AB,$96,$FB,$34,$C0,$B5,$D1
  );

  InData3: string = 'abc';
  OutData3: TXRTLMD2Sum = (
    $DA,$85,$3B,$0D,$3F,$88,$D9,$9B,$30,$28,$3A,$69,$E6,$DE,$D6,$BB
  );

  InData4: string = 'message digest';
  OutData4: TXRTLMD2Sum = (
    $AB,$4F,$49,$6B,$FB,$2A,$53,$0B,$21,$9F,$F3,$30,$31,$FE,$06,$B0
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLMD2Sum = (
    $4E,$8D,$DF,$F3,$65,$02,$92,$AB,$5A,$41,$08,$C3,$AA,$47,$94,$0B
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLMD2Sum = (
    $DA,$33,$DE,$F2,$A4,$2D,$F1,$39,$75,$35,$28,$46,$C3,$03,$38,$CD
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLMD2Sum = (
    $D5,$97,$6F,$79,$D8,$3D,$3A,$0D,$C9,$80,$6C,$3C,$66,$F3,$EF,$D8
  );

  function Test(MD: TXRTLMD2; InData: string; OutData: PXRTLMD2Buffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLMD2SumSize);
  end;

var
  MD: TXRTLMD2;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLMD2.Create;
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

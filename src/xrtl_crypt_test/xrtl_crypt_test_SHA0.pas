unit xrtl_crypt_test_SHA0;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_SHA, xrtl_crypt_md_SHA0;

procedure TestTXRTLSHA0;

implementation

procedure TestTXRTLSHA0;
const
  InData1: string = '';
  OutData1: TXRTLSHASum = (
    $F9,$6C,$EA,$19,$8A,$D1,$DD,$56,$17,$AC,$08,$4A,$3D,$92,$C6,$10,$77,$08,$C0,$EF
  );

  InData2: string = 'a';
  OutData2: TXRTLSHASum = (
    $37,$F2,$97,$77,$2F,$AE,$4C,$B1,$BA,$39,$B6,$CF,$9C,$F0,$38,$11,$80,$BD,$62,$F2
  );

  InData3: string = 'abc';
  OutData3: TXRTLSHASum = (
    $01,$64,$B8,$A9,$14,$CD,$2A,$5E,$74,$C4,$F7,$FF,$08,$2C,$4D,$97,$F1,$ED,$F8,$80
  );

  InData4: string = 'message digest';
  OutData4: TXRTLSHASum = (
    $C1,$B0,$F2,$22,$D1,$50,$EB,$B9,$AA,$36,$A4,$0C,$AF,$DC,$8B,$CB,$ED,$83,$0B,$14
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLSHASum = (
    $B4,$0C,$E0,$7A,$43,$0C,$FD,$3C,$03,$30,$39,$B9,$FE,$9A,$FE,$C9,$5D,$C1,$BD,$CD
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLSHASum = (
    $A7,$B7,$E6,$35,$17,$5B,$AD,$43,$FB,$61,$42,$0E,$F2,$A9,$D1,$D9,$13,$C1,$99,$A9
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLSHASum = (
    $4A,$A2,$9D,$14,$D1,$71,$52,$2E,$CE,$47,$BE,$E8,$95,$7E,$35,$A4,$1F,$3E,$9C,$FF
  );

  function Test(MD: TXRTLSHA0; InData: string; OutData: PXRTLSHABuffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLSHASumSize);
  end;

var
  MD: TXRTLSHA0;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLSHA0.Create;
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

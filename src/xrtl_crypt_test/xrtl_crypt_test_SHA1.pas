unit xrtl_crypt_test_SHA1;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_SHA, xrtl_crypt_md_SHA1;

procedure TestTXRTLSHA1;

implementation

procedure TestTXRTLSHA1;
const
  InData1: string = '';
  OutData1: TXRTLSHASum = (
    $DA,$39,$A3,$EE,$5E,$6B,$4B,$0D,$32,$55,$BF,$EF,$95,$60,$18,$90,$AF,$D8,$07,$09
  );

  InData2: string = 'a';
  OutData2: TXRTLSHASum = (
    $86,$F7,$E4,$37,$FA,$A5,$A7,$FC,$E1,$5D,$1D,$DC,$B9,$EA,$EA,$EA,$37,$76,$67,$B8
  );

  InData3: string = 'abc';
  OutData3: TXRTLSHASum = (
    $A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D
  );

  InData4: string = 'message digest';
  OutData4: TXRTLSHASum = (
    $C1,$22,$52,$CE,$DA,$8B,$E8,$99,$4D,$5F,$A0,$29,$0A,$47,$23,$1C,$1D,$16,$AA,$E3
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLSHASum = (
    $32,$D1,$0C,$7B,$8C,$F9,$65,$70,$CA,$04,$CE,$37,$F2,$A1,$9D,$84,$24,$0D,$3A,$89
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLSHASum = (
    $81,$01,$E3,$41,$A1,$16,$98,$40,$5D,$B9,$61,$24,$90,$F8,$DE,$72,$64,$A1,$E7,$42
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLSHASum = (
    $50,$AB,$F5,$70,$6A,$15,$09,$90,$A0,$8B,$2C,$5E,$A4,$0F,$A0,$E5,$85,$55,$47,$32
  );

  function Test(MD: TXRTLSHA1; InData: string; OutData: PXRTLSHABuffer): Boolean;
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
  MD: TXRTLSHA1;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLSHA1.Create;
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

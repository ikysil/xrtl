unit xrtl_crypt_test_MD4;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_MD4;

procedure TestTXRTLMD4;

implementation

procedure TestTXRTLMD4;
const
  InData1: string = '';
  OutData1: TXRTLMD4Sum = (
    $31,$D6,$CF,$E0,$D1,$6A,$E9,$31,$B7,$3C,$59,$D7,$E0,$C0,$89,$C0
  );

  InData2: string = 'a';
  OutData2: TXRTLMD4Sum = (
    $BD,$E5,$2C,$B3,$1D,$E3,$3E,$46,$24,$5E,$05,$FB,$DB,$D6,$FB,$24
  );

  InData3: string = 'abc';
  OutData3: TXRTLMD4Sum = (
    $A4,$48,$01,$7A,$AF,$21,$D8,$52,$5F,$C1,$0A,$E8,$7A,$A6,$72,$9D
  );

  InData4: string = 'message digest';
  OutData4: TXRTLMD4Sum = (
    $D9,$13,$0A,$81,$64,$54,$9F,$E8,$18,$87,$48,$06,$E1,$C7,$01,$4B
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLMD4Sum = (
    $D7,$9E,$1C,$30,$8A,$A5,$BB,$CD,$EE,$A8,$ED,$63,$DF,$41,$2D,$A9
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLMD4Sum = (
    $04,$3F,$85,$82,$F2,$41,$DB,$35,$1C,$E6,$27,$E1,$53,$E7,$F0,$E4
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLMD4Sum = (
    $E3,$3B,$4D,$DC,$9C,$38,$F2,$19,$9C,$3E,$7B,$16,$4F,$CC,$05,$36
  );

  function Test(MD: TXRTLMD4; InData: string; OutData: PXRTLMD4Buffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLMD4SumSize);
  end;

var
  MD: TXRTLMD4;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLMD4.Create;
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

unit xrtl_crypt_test_RIPEMD128;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_RIPEMD128;

procedure TestTXRTLRIPEMD128;

implementation

procedure TestTXRTLRIPEMD128;
const
  InData1: string = '';
  OutData1: TXRTLRIPEMD128Sum = (
    $CD,$F2,$62,$13,$A1,$50,$DC,$3E,$CB,$61,$0F,$18,$F6,$B3,$8B,$46
  );

  InData2: string = 'a';
  OutData2: TXRTLRIPEMD128Sum = (
    $86,$BE,$7A,$FA,$33,$9D,$0F,$C7,$CF,$C7,$85,$E7,$2F,$57,$8D,$33
  );

  InData3: string = 'abc';
  OutData3: TXRTLRIPEMD128Sum = (
    $C1,$4A,$12,$19,$9C,$66,$E4,$BA,$84,$63,$6B,$0F,$69,$14,$4C,$77
  );

  InData4: string = 'message digest';
  OutData4: TXRTLRIPEMD128Sum = (
    $9E,$32,$7B,$3D,$6E,$52,$30,$62,$AF,$C1,$13,$2D,$7D,$F9,$D1,$B8
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLRIPEMD128Sum = (
    $FD,$2A,$A6,$07,$F7,$1D,$C8,$F5,$10,$71,$49,$22,$B3,$71,$83,$4E
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLRIPEMD128Sum = (
    $D1,$E9,$59,$EB,$17,$9C,$91,$1F,$AE,$A4,$62,$4C,$60,$C5,$C7,$02
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLRIPEMD128Sum = (
    $3F,$45,$EF,$19,$47,$32,$C2,$DB,$B2,$C4,$A2,$C7,$69,$79,$5F,$A3
  );

  function Test(MD: TXRTLRIPEMD128; InData: string; OutData: PXRTLRIPEMD128Buffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLRIPEMD128SumSize);
  end;

var
  MD: TXRTLRIPEMD128;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLRIPEMD128.Create;
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

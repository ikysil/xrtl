unit xrtl_crypt_test_RIPEMD160;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_RIPEMD160;

procedure TestTXRTLRIPEMD160;

implementation

procedure TestTXRTLRIPEMD160;
const
  InData1: string = '';
  OutData1: TXRTLRIPEMD160Sum = (
    $9C,$11,$85,$A5,$C5,$E9,$FC,$54,$61,$28,$08,$97,$7E,$E8,$F5,$48,$B2,$25,$8D,$31
  );

  InData2: string = 'a';
  OutData2: TXRTLRIPEMD160Sum = (
    $0B,$DC,$9D,$2D,$25,$6B,$3E,$E9,$DA,$AE,$34,$7B,$E6,$F4,$DC,$83,$5A,$46,$7F,$FE
  );

  InData3: string = 'abc';
  OutData3: TXRTLRIPEMD160Sum = (
    $8E,$B2,$08,$F7,$E0,$5D,$98,$7A,$9B,$04,$4A,$8E,$98,$C6,$B0,$87,$F1,$5A,$0B,$FC
  );

  InData4: string = 'message digest';
  OutData4: TXRTLRIPEMD160Sum = (
    $5D,$06,$89,$EF,$49,$D2,$FA,$E5,$72,$B8,$81,$B1,$23,$A8,$5F,$FA,$21,$59,$5F,$36
  );

  InData5: string = 'abcdefghijklmnopqrstuvwxyz';
  OutData5: TXRTLRIPEMD160Sum = (
    $F7,$1C,$27,$10,$9C,$69,$2C,$1B,$56,$BB,$DC,$EB,$5B,$9D,$28,$65,$B3,$70,$8D,$BC
  );

  InData6: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  OutData6: TXRTLRIPEMD160Sum = (
    $B0,$E2,$0B,$6E,$31,$16,$64,$02,$86,$ED,$3A,$87,$A5,$71,$30,$79,$B2,$1F,$51,$89
  );

  InData7: string = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';
  OutData7: TXRTLRIPEMD160Sum = (
    $9B,$75,$2E,$45,$57,$3D,$4B,$39,$F4,$DB,$D3,$32,$3C,$AB,$82,$BF,$63,$32,$6B,$FB
  );

  function Test(MD: TXRTLRIPEMD160; InData: string; OutData: PXRTLRIPEMD160Buffer): Boolean;
  var
    Data: PByteArray;
    InAvail: Integer;
  begin
    MD.Init;
    Data:= @InData[1];
    InAvail:= Length(InData);
    MD.Update(Data, InAvail, Data, InAvail, spoFinish);
    Result:= XRTLCompareMessageDigest(MD, OutData, XRTLRIPEMD160SumSize);
  end;

var
  MD: TXRTLRIPEMD160;
  Result: Boolean;
begin
  MD:= nil;
  try
    MD:= TXRTLRIPEMD160.Create;
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

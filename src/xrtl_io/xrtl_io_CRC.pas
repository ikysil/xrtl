unit xrtl_io_CRC;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_CPUUtils, xrtl_util_Type, xrtl_util_Compat,
  xrtl_io_StreamProcessor, xrtl_io_CheckSum;

type
  TXRTLCRC16 = class(TXRTLCustomCheckSum16Bit)
  protected
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  end;

  TXRTLCRC32 = class(TXRTLCustomCheckSum32Bit)
  protected
    function   engineUpdateSum(var InBuffer: PByteArray;
                               var InAvail: Integer;
                               const Operation: TXRTLStreamProcessorOperation): Boolean; override;
  end;

implementation

const
  CRC16Table: array[Byte] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780,
    $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1,
    $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, $D801,
    $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40,
    $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680,
    $D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, $F001, $30C0,
    $3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501,
    $35C0, $3480, $F441, $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981,
    $2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
    $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200,
    $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0, $6180, $A141,
    $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480,
    $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0,
    $6B80, $AB41, $6900, $A9C1, $A881, $6840, $7800, $B8C1, $B981, $7940, $BB01,
    $7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381,
    $7340, $B101, $71C0, $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0,
    $5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01,
    $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
    $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81,
    $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, $4400, $84C1,
    $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100,
    $81C1, $8081, $4040
  );

  CRC32Table: array[Byte] of LongWord = (
    $000000000, $077073096, $0EE0E612C, $0990951BA, $0076DC419, $0706AF48F,
    $0E963A535, $09E6495A3, $00EDB8832, $079DCB8A4, $0E0D5E91E, $097D2D988,
    $009B64C2B, $07EB17CBD, $0E7B82D07, $090BF1D91, $01DB71064, $06AB020F2,
    $0F3B97148, $084BE41DE, $01ADAD47D, $06DDDE4EB, $0F4D4B551, $083D385C7,
    $0136C9856, $0646BA8C0, $0FD62F97A, $08A65C9EC, $014015C4F, $063066CD9,
    $0FA0F3D63, $08D080DF5, $03B6E20C8, $04C69105E, $0D56041E4, $0A2677172,
    $03C03E4D1, $04B04D447, $0D20D85FD, $0A50AB56B, $035B5A8FA, $042B2986C,
    $0DBBBC9D6, $0ACBCF940, $032D86CE3, $045DF5C75, $0DCD60DCF, $0ABD13D59,
    $026D930AC, $051DE003A, $0C8D75180, $0BFD06116, $021B4F4B5, $056B3C423,
    $0CFBA9599, $0B8BDA50F, $02802B89E, $05F058808, $0C60CD9B2, $0B10BE924,
    $02F6F7C87, $058684C11, $0C1611DAB, $0B6662D3D, $076DC4190, $001DB7106,
    $098D220BC, $0EFD5102A, $071B18589, $006B6B51F, $09FBFE4A5, $0E8B8D433,
    $07807C9A2, $00F00F934, $09609A88E, $0E10E9818, $07F6A0DBB, $0086D3D2D,
    $091646C97, $0E6635C01, $06B6B51F4, $01C6C6162, $0856530D8, $0F262004E,
    $06C0695ED, $01B01A57B, $08208F4C1, $0F50FC457, $065B0D9C6, $012B7E950,
    $08BBEB8EA, $0FCB9887C, $062DD1DDF, $015DA2D49, $08CD37CF3, $0FBD44C65,
    $04DB26158, $03AB551CE, $0A3BC0074, $0D4BB30E2, $04ADFA541, $03DD895D7,
    $0A4D1C46D, $0D3D6F4FB, $04369E96A, $0346ED9FC, $0AD678846, $0DA60B8D0,
    $044042D73, $033031DE5, $0AA0A4C5F, $0DD0D7CC9, $05005713C, $0270241AA,
    $0BE0B1010, $0C90C2086, $05768B525, $0206F85B3, $0B966D409, $0CE61E49F,
    $05EDEF90E, $029D9C998, $0B0D09822, $0C7D7A8B4, $059B33D17, $02EB40D81,
    $0B7BD5C3B, $0C0BA6CAD, $0EDB88320, $09ABFB3B6, $003B6E20C, $074B1D29A,
    $0EAD54739, $09DD277AF, $004DB2615, $073DC1683, $0E3630B12, $094643B84,
    $00D6D6A3E, $07A6A5AA8, $0E40ECF0B, $09309FF9D, $00A00AE27, $07D079EB1,
    $0F00F9344, $08708A3D2, $01E01F268, $06906C2FE, $0F762575D, $0806567CB,
    $0196C3671, $06E6B06E7, $0FED41B76, $089D32BE0, $010DA7A5A, $067DD4ACC,
    $0F9B9DF6F, $08EBEEFF9, $017B7BE43, $060B08ED5, $0D6D6A3E8, $0A1D1937E,
    $038D8C2C4, $04FDFF252, $0D1BB67F1, $0A6BC5767, $03FB506DD, $048B2364B,
    $0D80D2BDA, $0AF0A1B4C, $036034AF6, $041047A60, $0DF60EFC3, $0A867DF55,
    $0316E8EEF, $04669BE79, $0CB61B38C, $0BC66831A, $0256FD2A0, $05268E236,
    $0CC0C7795, $0BB0B4703, $0220216B9, $05505262F, $0C5BA3BBE, $0B2BD0B28,
    $02BB45A92, $05CB36A04, $0C2D7FFA7, $0B5D0CF31, $02CD99E8B, $05BDEAE1D,
    $09B64C2B0, $0EC63F226, $0756AA39C, $0026D930A, $09C0906A9, $0EB0E363F,
    $072076785, $005005713, $095BF4A82, $0E2B87A14, $07BB12BAE, $00CB61B38,
    $092D28E9B, $0E5D5BE0D, $07CDCEFB7, $00BDBDF21, $086D3D2D4, $0F1D4E242,
    $068DDB3F8, $01FDA836E, $081BE16CD, $0F6B9265B, $06FB077E1, $018B74777,
    $088085AE6, $0FF0F6A70, $066063BCA, $011010B5C, $08F659EFF, $0F862AE69,
    $0616BFFD3, $0166CCF45, $0A00AE278, $0D70DD2EE, $04E048354, $03903B3C2,
    $0A7672661, $0D06016F7, $04969474D, $03E6E77DB, $0AED16A4A, $0D9D65ADC,
    $040DF0B66, $037D83BF0, $0A9BCAE53, $0DEBB9EC5, $047B2CF7F, $030B5FFE9,
    $0BDBDF21C, $0CABAC28A, $053B39330, $024B4A3A6, $0BAD03605, $0CDD70693,
    $054DE5729, $023D967BF, $0B3667A2E, $0C4614AB8, $05D681B02, $02A6F2B94,
    $0B40BBE37, $0C30C8EA1, $05A05DF1B, $02D02EF8D
  );

{ TXRTLCRC16 }

function TXRTLCRC16.engineUpdateSum(var InBuffer: PByteArray;
  var InAvail: Integer; const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  FSum: PWord;
begin
  Result:= True;
  while InAvail > 0 do
  begin
{$R-,Q-}
    FSum:= @FBytes;
    FSum^:= CRC16Table[(InBuffer[0] xor FSum^) and $FF] xor (FSum^ shr 8);
{$R+,Q+}
    InBuffer:= XRTLPointerAdd(InBuffer, SizeOf(Byte));
    Dec(InAvail);
  end;
end;

{ TXRTLCRC32 }

function TXRTLCRC32.engineUpdateSum(var InBuffer: PByteArray;
  var InAvail: Integer; const Operation: TXRTLStreamProcessorOperation): Boolean;
var
  FSum: PCardinal;
begin
  Result:= True;
  FSum:= @FBytes;
  FSum^:= FSum^ xor $FFFFFFFF;
  while InAvail > 0 do
  begin
{$R-,Q-}
    FSum^:= CRC32Table[(InBuffer[0] xor FSum^) and $FF] xor ((FSum^ shr 8) and $FFFFFF);
{$R+,Q+}
    InBuffer:= XRTLPointerAdd(InBuffer, SizeOf(Byte));
    Dec(InAvail);
  end;
  FSum^:= FSum^ xor $FFFFFFFF;
end;

end.

unit xrtl_io_TaggedDataStream;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_Stream, xrtl_io_DataStream, xrtl_io_Exception, xrtl_io_ResourceStrings;

type
  TXRTLDataTag = (dtBoolean, dtByte, dtCardinal, dtCurrency, dtDateTime,
                  dtDouble, dtInt64, dtInteger, dtShortInt, dtSingle, dtSmallInt,
                  dtString, dtUTF8String, dtVariant, dtWideString, dtWordBool);

  TXRTLTaggedDataInputStream = class(TXRTLDataInputStream)
  private
  public
    function   ReadDataTag: TXRTLDataTag;
    procedure  CheckDataTag(DataTag: TXRTLDataTag);
    function   ReadBoolean: Boolean; override;
    function   ReadByte: Byte; override;
    function   ReadCardinal: Cardinal; override;
    function   ReadCurrency: Currency; override;
    function   ReadDateTime: TDateTime; override;
    function   ReadDouble: Double; override;
    function   ReadInt64: Int64; override;
    function   ReadInteger: Integer; override;
    function   ReadShortInt: ShortInt; override;
    function   ReadSingle: Single; override;
    function   ReadSmallInt: SmallInt; override;
    function   ReadString: string; override;
    function   ReadUTF8String: WideString; override;
    function   ReadVariant: Variant; override;
    function   ReadWideString: WideString; override;
    function   ReadWordBool: WordBool; override;
  end;

  TXRTLTaggedDataOutputStream = class(TXRTLDataOutputStream)
  private
  public
    procedure  WriteDataTag(DataTag: TXRTLDataTag);
    procedure  WriteBoolean(AValue: Boolean); override;
    procedure  WriteByte(AValue: Byte); override;
    procedure  WriteCardinal(AValue: Cardinal); override;
    procedure  WriteCurrency(AValue: Currency); override;
    procedure  WriteDateTime(AValue: TDateTime); override;
    procedure  WriteDouble(AValue: Double); override;
    procedure  WriteInt64(AValue: Int64); override;
    procedure  WriteInteger(AValue: Integer); override;
    procedure  WriteShortInt(AValue: ShortInt); override;
    procedure  WriteSingle(AValue: Single); override;
    procedure  WriteSmallInt(AValue: SmallInt); override;
    procedure  WriteString(AValue: string); override;
    procedure  WriteUTF8String(AValue: WideString); override;
    procedure  WriteVariant(AValue: Variant); override;
    procedure  WriteWideString(AValue: WideString); override;
    procedure  WriteWordBool(AValue: WordBool); override;
  end;

implementation

{ TXRTLTaggedDataInputStream }

function TXRTLTaggedDataInputStream.ReadDataTag: TXRTLDataTag;
begin
  Result:= TXRTLDataTag(inherited ReadByte);
end;

procedure TXRTLTaggedDataInputStream.CheckDataTag(DataTag: TXRTLDataTag);
var
  SDataTag: TXRTLDataTag;
begin
  SDataTag:= ReadDataTag;
  if (SDataTag <> DataTag) then
    raise EXRTLIOException.CreateFmt(SXRTLInvalidDataTag, [Byte(SDataTag), Byte(DataTag)]);
end;

function TXRTLTaggedDataInputStream.ReadBoolean: Boolean;
begin
  CheckDataTag(dtBoolean);
  Result:= inherited ReadBoolean;
end;

function TXRTLTaggedDataInputStream.ReadByte: Byte;
begin
  CheckDataTag(dtByte);
  Result:= inherited ReadByte;
end;

function TXRTLTaggedDataInputStream.ReadCardinal: Cardinal;
begin
  CheckDataTag(dtCardinal);
  Result:= inherited ReadCardinal;
end;

function TXRTLTaggedDataInputStream.ReadCurrency: Currency;
begin
  CheckDataTag(dtCurrency);
  Result:= inherited ReadCurrency;
end;

function TXRTLTaggedDataInputStream.ReadDateTime: TDateTime;
begin
  CheckDataTag(dtDateTime);
  Result:= inherited ReadDateTime;
end;

function TXRTLTaggedDataInputStream.ReadDouble: Double;
begin
  CheckDataTag(dtDouble);
  Result:= inherited ReadDouble;
end;

function TXRTLTaggedDataInputStream.ReadInt64: Int64;
begin
  CheckDataTag(dtInt64);
  Result:= inherited ReadInt64;
end;

function TXRTLTaggedDataInputStream.ReadInteger: Integer;
begin
  CheckDataTag(dtInteger);
  Result:= inherited ReadInteger;
end;

function TXRTLTaggedDataInputStream.ReadShortInt: ShortInt;
begin
  CheckDataTag(dtShortInt);
  Result:= inherited ReadShortInt;
end;

function TXRTLTaggedDataInputStream.ReadSingle: Single;
begin
  CheckDataTag(dtSingle);
  Result:= inherited ReadSingle;
end;

function TXRTLTaggedDataInputStream.ReadSmallInt: SmallInt;
begin
  CheckDataTag(dtSmallInt);
  Result:= inherited ReadSmallInt;
end;

function TXRTLTaggedDataInputStream.ReadString: string;
begin
  CheckDataTag(dtString);
  Result:= inherited ReadString;
end;

function TXRTLTaggedDataInputStream.ReadUTF8String: WideString;
begin
  CheckDataTag(dtUTF8String);
  Result:= inherited ReadUTF8String;
end;

function TXRTLTaggedDataInputStream.ReadVariant: Variant;
begin
  CheckDataTag(dtVariant);
  Result:= inherited ReadVariant;
end;

function TXRTLTaggedDataInputStream.ReadWideString: WideString;
begin
  CheckDataTag(dtWideString);
  Result:= inherited ReadWideString;
end;

function TXRTLTaggedDataInputStream.ReadWordBool: WordBool;
begin
  CheckDataTag(dtWordBool);
  Result:= inherited ReadWordBool;
end;

{ TXRTLTaggedDataOutputStream }

procedure TXRTLTaggedDataOutputStream.WriteDataTag(DataTag: TXRTLDataTag);
begin
  inherited WriteByte(Byte(DataTag));
end;

procedure TXRTLTaggedDataOutputStream.WriteBoolean(AValue: Boolean);
begin
  WriteDataTag(dtBoolean);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteByte(AValue: Byte);
begin
  WriteDataTag(dtByte);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteCardinal(AValue: Cardinal);
begin
  WriteDataTag(dtCardinal);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteCurrency(AValue: Currency);
begin
  WriteDataTag(dtCurrency);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteDateTime(AValue: TDateTime);
begin
  WriteDataTag(dtDateTime);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteDouble(AValue: Double);
begin
  WriteDataTag(dtDouble);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteInt64(AValue: Int64);
begin
  WriteDataTag(dtInt64);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteInteger(AValue: Integer);
begin
  WriteDataTag(dtInteger);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteShortInt(AValue: ShortInt);
begin
  WriteDataTag(dtShortInt);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteSingle(AValue: Single);
begin
  WriteDataTag(dtSingle);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteSmallInt(AValue: SmallInt);
begin
  WriteDataTag(dtSmallInt);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteString(AValue: string);
begin
  WriteDataTag(dtString);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteUTF8String(AValue: WideString);
begin
  WriteDataTag(dtUTF8String);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteVariant(AValue: Variant);
begin
  WriteDataTag(dtVariant);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteWideString(AValue: WideString);
begin
  WriteDataTag(dtWideString);
  inherited;
end;

procedure TXRTLTaggedDataOutputStream.WriteWordBool(AValue: WordBool);
begin
  WriteDataTag(dtWordBool);
  inherited;
end;

end.

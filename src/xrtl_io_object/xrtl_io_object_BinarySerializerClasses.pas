unit xrtl_io_object_BinarySerializerClasses;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils,
  xrtl_util_Type, xrtl_util_Exception,
  xrtl_io_Stream, xrtl_io_DataStream,
  xrtl_io_object_Serializer;

type
  EXRTLBinaryObjectWriterException = class(EXRTLSerializerException);
  EXRTLBinaryObjectReaderException = class(EXRTLSerializerException);

  TXRTLBinaryObjectWriter = class(TInterfacedObject, IXRTLObjectWriter)
  private
    FSerializer: IXRTLSerializer;
    FStream: TXRTLDataOutputStream;
    FBinaryDataStream: TXRTLOutputStream;
    procedure  CheckBinaryDataStream;
  public
    constructor Create(const ACoreStream: TXRTLOutputStream; const ASerializer: IXRTLSerializer);
    destructor Destroy; override;
    procedure  WriteBoolean(AValue: Boolean);
    procedure  WriteByte(AValue: Byte);
    procedure  WriteCardinal(AValue: Cardinal);
    procedure  WriteCurrency(AValue: Currency);
    procedure  WriteDateTime(AValue: TDateTime);
    procedure  WriteDouble(AValue: Double);
    procedure  WriteInt64(AValue: Int64);
    procedure  WriteInteger(AValue: Integer);
    procedure  WriteShortInt(AValue: ShortInt);
    procedure  WriteSingle(AValue: Single);
    procedure  WriteSmallInt(AValue: SmallInt);
    procedure  WriteString(AValue: string);
    procedure  WriteUTF8String(AValue: WideString);
    procedure  WriteVariant(AValue: Variant);
    procedure  WriteWideString(AValue: WideString);
    procedure  WriteWordBool(AValue: WordBool);
    procedure  WriteInterface(const Obj: IInterface; const AllowShared: Boolean = True);
    procedure  WriteClass(const Obj: TClass; const AllowShared: Boolean = True);
    procedure  WriteObject(const Obj: TObject; const AllowShared: Boolean = True);
    function   BeginBinaryData: TXRTLOutputStream;
    procedure  EndBinaryData(var Stream: TXRTLOutputStream);
  end;

  TXRTLBinaryObjectReader = class(TInterfacedObject, IXRTLObjectReader)
  private
    FDeserializer: IXRTLDeserializer;
    FStream: TXRTLDataInputStream;
    FBinaryDataStream: TXRTLInputStream;
    procedure  CheckBinaryDataStream;
  public
    constructor Create(const ACoreStream: TXRTLInputStream; const ADeserializer: IXRTLDeserializer);
    destructor Destroy; override;
    function   ReadBoolean: Boolean;
    function   ReadByte: Byte;
    function   ReadCardinal: Cardinal;
    function   ReadCurrency: Currency;
    function   ReadDateTime: TDateTime;
    function   ReadDouble: Double;
    function   ReadInt64: Int64;
    function   ReadInteger: Integer;
    function   ReadShortInt: ShortInt;
    function   ReadSingle: Single;
    function   ReadSmallInt: SmallInt;
    function   ReadString: string;
    function   ReadUTF8String: WideString;
    function   ReadVariant: Variant;
    function   ReadWideString: WideString;
    function   ReadWordBool: WordBool;
    function   ReadInterface(const Obj: IInterface; const AllowShared: Boolean = True): IInterface;
    function   ReadClass(const Obj: TClass; const AllowShared: Boolean = True): TClass;
    function   ReadObject(const Obj: TObject; const AllowShared: Boolean = True): TObject;
    function   BeginBinaryData: TXRTLInputStream;
    procedure  EndBinaryData(var Stream: TXRTLInputStream);
  end;

implementation

uses
  xrtl_io_BlockStream;

{ TXRTLBinaryObjectWriter }

constructor TXRTLBinaryObjectWriter.Create(const ACoreStream: TXRTLOutputStream;
  const ASerializer: IXRTLSerializer);
begin
  inherited Create;
  FStream:= TXRTLDataOutputStream.Create(ACoreStream, False);
  FSerializer:= ASerializer;
  FBinaryDataStream:= nil;
end;

destructor TXRTLBinaryObjectWriter.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TXRTLBinaryObjectWriter.CheckBinaryDataStream;
begin
  if Assigned(FBinaryDataStream) then
    raise EXRTLBinaryObjectWriterException.Create('Binary data stream is not properly closed.');
end;

procedure TXRTLBinaryObjectWriter.WriteBoolean(AValue: Boolean);
begin
  CheckBinaryDataStream;
  FStream.WriteBoolean(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteByte(AValue: Byte);
begin
  CheckBinaryDataStream;
  FStream.WriteByte(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteCardinal(AValue: Cardinal);
begin
  CheckBinaryDataStream;
  FStream.WriteCardinal(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteCurrency(AValue: Currency);
begin
  CheckBinaryDataStream;
  FStream.WriteCurrency(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteDateTime(AValue: TDateTime);
begin
  CheckBinaryDataStream;
  FStream.WriteDateTime(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteDouble(AValue: Double);
begin
  CheckBinaryDataStream;
  FStream.WriteDouble(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteInt64(AValue: Int64);
begin
  CheckBinaryDataStream;
  FStream.WriteInt64(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteInteger(AValue: Integer);
begin
  CheckBinaryDataStream;
  FStream.WriteInteger(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteShortInt(AValue: ShortInt);
begin
  CheckBinaryDataStream;
  FStream.WriteShortInt(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteSingle(AValue: Single);
begin
  CheckBinaryDataStream;
  FStream.WriteSingle(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteSmallInt(AValue: SmallInt);
begin
  CheckBinaryDataStream;
  FStream.WriteSmallInt(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteString(AValue: string);
begin
  CheckBinaryDataStream;
  FStream.WriteString(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteUTF8String(AValue: WideString);
begin
  CheckBinaryDataStream;
  FStream.WriteUTF8String(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteVariant(AValue: Variant);
begin
  CheckBinaryDataStream;
  FStream.WriteVariant(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteWideString(AValue: WideString);
begin
  CheckBinaryDataStream;
  FStream.WriteWideString(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteWordBool(AValue: WordBool);
begin
  CheckBinaryDataStream;
  FStream.WriteWordBool(AValue);
end;

procedure TXRTLBinaryObjectWriter.WriteInterface(const Obj: IInterface; const AllowShared: Boolean = True);
begin
  CheckBinaryDataStream;
  FSerializer.WriteInterface(FStream, Obj, AllowShared);
end;

procedure TXRTLBinaryObjectWriter.WriteClass(const Obj: TClass; const AllowShared: Boolean = True);
begin
  CheckBinaryDataStream;
  FSerializer.WriteClass(FStream, Obj, AllowShared);
end;

procedure TXRTLBinaryObjectWriter.WriteObject(const Obj: TObject; const AllowShared: Boolean = True);
begin
  CheckBinaryDataStream;
  FSerializer.WriteObject(FStream, Obj, AllowShared);
end;

function TXRTLBinaryObjectWriter.BeginBinaryData: TXRTLOutputStream;
begin
  CheckBinaryDataStream;
  FBinaryDataStream:= TXRTLBlockOutputStream.Create(FStream, False);
  Result:= FBinaryDataStream;
end;

procedure TXRTLBinaryObjectWriter.EndBinaryData(var Stream: TXRTLOutputStream);
begin
  if Stream <> FBinaryDataStream then
    raise EXRTLBinaryObjectWriterException.Create('Invalid binary data stream');
  FreeAndNil(Stream);
  FBinaryDataStream:= nil;
end;

{ TXRTLBinaryObjectReader }

constructor TXRTLBinaryObjectReader.Create(const ACoreStream: TXRTLInputStream;
  const ADeserializer: IXRTLDeserializer);
begin
  inherited Create;
  FStream:= TXRTLDataInputStream.Create(ACoreStream, False);
  FDeserializer:= ADeserializer;
end;

destructor TXRTLBinaryObjectReader.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TXRTLBinaryObjectReader.CheckBinaryDataStream;
begin
  if Assigned(FBinaryDataStream) then
    raise EXRTLBinaryObjectReaderException.Create('Binary data stream is not properly closed.');
end;

function TXRTLBinaryObjectReader.ReadBoolean: Boolean;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadBoolean;
end;

function TXRTLBinaryObjectReader.ReadByte: Byte;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadByte;
end;

function TXRTLBinaryObjectReader.ReadCardinal: Cardinal;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadCardinal;
end;

function TXRTLBinaryObjectReader.ReadCurrency: Currency;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadCurrency;
end;

function TXRTLBinaryObjectReader.ReadDateTime: TDateTime;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadDateTime;
end;

function TXRTLBinaryObjectReader.ReadDouble: Double;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadDouble;
end;

function TXRTLBinaryObjectReader.ReadInt64: Int64;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadInt64;
end;

function TXRTLBinaryObjectReader.ReadInteger: Integer;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadInteger;
end;

function TXRTLBinaryObjectReader.ReadShortInt: ShortInt;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadShortInt;
end;

function TXRTLBinaryObjectReader.ReadSingle: Single;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadSingle;
end;

function TXRTLBinaryObjectReader.ReadSmallInt: SmallInt;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadSmallInt;
end;

function TXRTLBinaryObjectReader.ReadString: string;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadString;
end;

function TXRTLBinaryObjectReader.ReadUTF8String: WideString;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadUTF8String;
end;

function TXRTLBinaryObjectReader.ReadVariant: Variant;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadVariant;
end;

function TXRTLBinaryObjectReader.ReadWideString: WideString;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadWideString;
end;

function TXRTLBinaryObjectReader.ReadWordBool: WordBool;
begin
  CheckBinaryDataStream;
  Result:= FStream.ReadWordBool;
end;

function TXRTLBinaryObjectReader.ReadInterface(const Obj: IInterface; const AllowShared: Boolean = True): IInterface;
begin
  CheckBinaryDataStream;
  Result:= FDeserializer.ReadInterface(FStream, Obj, AllowShared);
end;

function TXRTLBinaryObjectReader.ReadClass(const Obj: TClass; const AllowShared: Boolean = True): TClass;
begin
  CheckBinaryDataStream;
  Result:= FDeserializer.ReadClass(FStream, Obj, AllowShared);
end;

function TXRTLBinaryObjectReader.ReadObject(const Obj: TObject; const AllowShared: Boolean = True): TObject;
begin
  CheckBinaryDataStream;
  Result:= FDeserializer.ReadObject(FStream, Obj, AllowShared);
end;

function TXRTLBinaryObjectReader.BeginBinaryData: TXRTLInputStream;
begin
  CheckBinaryDataStream;
  FBinaryDataStream:= TXRTLBlockInputStream.Create(FStream, False);
  Result:= FBinaryDataStream;
end;

procedure TXRTLBinaryObjectReader.EndBinaryData(var Stream: TXRTLInputStream);
begin
  if Stream <> FBinaryDataStream then
    raise EXRTLBinaryObjectReaderException.Create('Invalid binary data stream');
  if Stream is TXRTLBlockInputStream then
    (Stream as TXRTLBlockInputStream).SkipToEndOfStream;
  FreeAndNil(Stream);
  FBinaryDataStream:= nil;
end;

end.

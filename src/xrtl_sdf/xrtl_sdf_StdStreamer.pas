unit xrtl_sdf_StdStreamer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Value, xrtl_util_Compat,
  xrtl_sdf_Serializer;

type
  TXRTLValueStreamer = class(TXRTLObjectStreamer)
  public
    procedure  ReadNoData(const Obj: TObject); override;
  end;

  TXRTLValueCardinalStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueIntegerStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueInt64Streamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueSingleStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueDoubleStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueExtendedStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueInterfaceStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueWideStringStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueObjectStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueVariantStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueCurrencyStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueCompStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueClassStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueGUIDStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueBooleanStreamer = class(TXRTLValueStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueContainerStreamer = class(TXRTLObjectStreamer)
  protected
    procedure  DoAdd(const Obj: TObject; const Value: IXRTLValue); virtual; abstract;
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadNoData(const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLSequentialContainerStreamer = class(TXRTLValueContainerStreamer)
  protected
    procedure  DoAdd(const Obj: TObject; const Value: IXRTLValue); override;
  end;

  TXRTLSetContainerStreamer = class(TXRTLValueContainerStreamer)
  protected
    procedure  DoAdd(const Obj: TObject; const Value: IXRTLValue); override;
  end;

  TXRTLKeyValueContainerStreamer = class(TXRTLObjectStreamer)
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadNoData(const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

procedure XRTLRegisterStdStreamers;

implementation

uses
  xrtl_util_ValueImpl, xrtl_util_Container;

procedure XRTLRegisterStdStreamers;
begin
  XRTLStreamerRegistry.Register(TXRTLValueCardinal,   TXRTLValueCardinalStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueInteger,    TXRTLValueIntegerStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueInt64,      TXRTLValueInt64Streamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueSingle,     TXRTLValueSingleStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueDouble,     TXRTLValueDoubleStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueExtended,   TXRTLValueExtendedStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueInterface,  TXRTLValueInterfaceStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueWideString, TXRTLValueWideStringStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueObject,     TXRTLValueObjectStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueVariant,    TXRTLValueVariantStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueCurrency,   TXRTLValueCurrencyStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueComp,       TXRTLValueCompStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueClass,      TXRTLValueClassStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueGUID,       TXRTLValueGUIDStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLValueBoolean,    TXRTLValueBooleanStreamer.Create);
  
  XRTLStreamerRegistry.Register(TXRTLSequentialContainer, TXRTLSequentialContainerStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLSetContainer,        TXRTLSetContainerStreamer.Create);
  XRTLStreamerRegistry.Register(TXRTLKeyValueContainer,   TXRTLSequentialContainerStreamer.Create);
end;

{ TXRTLValueStreamer }

procedure TXRTLValueStreamer.ReadNoData(const Obj: TObject);
begin
end;

{ TXRTLValueCardinalStreamer }

procedure TXRTLValueCardinalStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteCardinal((Obj as TXRTLValueCardinal).GetValue);
end;

procedure TXRTLValueCardinalStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueCardinal).SetValue(Reader.ReadCardinal);
end;

{ TXRTLValueIntegerStreamer }

procedure TXRTLValueIntegerStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteInteger((Obj as TXRTLValueInteger).GetValue);
end;

procedure TXRTLValueIntegerStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueInteger).SetValue(Reader.ReadInteger);
end;

{ TXRTLValueInt64Streamer }

procedure TXRTLValueInt64Streamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteInt64((Obj as TXRTLValueInt64).GetValue);
end;

procedure TXRTLValueInt64Streamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueInt64).SetValue(Reader.ReadInt64);
end;

{ TXRTLValueSingleStreamer }

procedure TXRTLValueSingleStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteSingle((Obj as TXRTLValueSingle).GetValue);
end;

procedure TXRTLValueSingleStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueSingle).SetValue(Reader.ReadSingle);
end;

{ TXRTLValueDoubleStreamer }

procedure TXRTLValueDoubleStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteDouble((Obj as TXRTLValueDouble).GetValue);
end;

procedure TXRTLValueDoubleStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueDouble).SetValue(Reader.ReadDouble);
end;

{ TXRTLValueExtendedStreamer }

procedure TXRTLValueExtendedStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteExtended((Obj as TXRTLValueExtended).GetValue);
end;

procedure TXRTLValueExtendedStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueExtended).SetValue(Reader.ReadExtended);
end;

{ TXRTLValueInterfaceStreamer }

procedure TXRTLValueInterfaceStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteInterface((Obj as TXRTLValueInterface).GetValue, True);
end;

procedure TXRTLValueInterfaceStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueInterface).SetValue(Reader.ReadInterface((Obj as TXRTLValueInterface).GetValue, True));
end;

{ TXRTLValueWideStringStreamer }

procedure TXRTLValueWideStringStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteWideString((Obj as TXRTLValueWideString).GetValue);
end;

procedure TXRTLValueWideStringStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueWideString).SetValue(Reader.ReadWideString);
end;

{ TXRTLValueObjectStreamer }

procedure TXRTLValueObjectStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteObject((Obj as TXRTLValueObject).GetValue(False), True);
end;

procedure TXRTLValueObjectStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueObject).SetValue(Reader.ReadObject((Obj as TXRTLValueObject).GetValue(False), True));
end;

{ TXRTLValueVariantStreamer }

procedure TXRTLValueVariantStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteVariant((Obj as TXRTLValueVariant).GetValue);
end;

procedure TXRTLValueVariantStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueVariant).SetValue(Reader.ReadVariant);
end;

{ TXRTLValueCurrencyStreamer }

procedure TXRTLValueCurrencyStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteCurrency((Obj as TXRTLValueCurrency).GetValue);
end;

procedure TXRTLValueCurrencyStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueCurrency).SetValue(Reader.ReadCurrency);
end;

{ TXRTLValueCompStreamer }

procedure TXRTLValueCompStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteComp((Obj as TXRTLValueComp).GetValue);
end;

procedure TXRTLValueCompStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueComp).SetValue(Reader.ReadComp);
end;

{ TXRTLValueClassStreamer }

procedure TXRTLValueClassStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteClass((Obj as TXRTLValueClass).GetValue, True);
end;

procedure TXRTLValueClassStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueClass).SetValue(Reader.ReadClass((Obj as TXRTLValueClass).GetValue, True));
end;

{ TXRTLValueGUIDStreamer }

procedure TXRTLValueGUIDStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteWideString(GUIDToString((Obj as TXRTLValueGUID).GetValue));
end;

procedure TXRTLValueGUIDStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueGUID).SetValue(StringToGUID(Reader.ReadWideString));
end;

{ TXRTLValueBooleanStreamer }

procedure TXRTLValueBooleanStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
begin
  Writer.WriteBoolean((Obj as TXRTLValueBoolean).GetValue);
end;

procedure TXRTLValueBooleanStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
begin
  (Obj as TXRTLValueBoolean).SetValue(Reader.ReadBoolean);
end;

{ TXRTLValueContainerStreamer }

procedure TXRTLValueContainerStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
var
  Values: TXRTLValueArray;
  I: Integer;
begin
  Values:= nil;
  try
    Values:= (Obj as TXRTLSequentialContainer).GetValues;
    Writer.WriteInteger(Length(Values));
    for I:= Low(Values) to High(Values) do
      Writer.WriteInterface(Values[I], True);
  finally
    Values:= nil;
  end;
end;

procedure TXRTLValueContainerStreamer.ReadNoData(const Obj: TObject);
begin
  (Obj as TXRTLSequentialContainer).Clear;
end;

procedure TXRTLValueContainerStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
var
  Count, I: Integer;
begin
  (Obj as TXRTLSequentialContainer).Clear;
  Count:= Reader.ReadInteger;
  for I:= 0 to Count - 1 do
  begin
    DoAdd(Obj, Reader.ReadInterface(nil, True) as IXRTLValue);
  end;
end;

{ TXRTLSequentialContainerStreamer }

procedure TXRTLSequentialContainerStreamer.DoAdd(const Obj: TObject; const Value: IXRTLValue);
begin
  (Obj as TXRTLSequentialContainer).Insert(Value);
end;

{ TXRTLSetContainerStreamer }

procedure TXRTLSetContainerStreamer.DoAdd(const Obj: TObject; const Value: IXRTLValue);
begin
  (Obj as TXRTLSetContainer).Add(Value);
end;

{ TXRTLKeyValueContainerStreamer }

procedure TXRTLKeyValueContainerStreamer.WriteObjectData(const Writer: IXRTLObjectWriter;
  const Obj: TObject);
var
  Values, Keys: TXRTLValueArray;
  I: Integer;
begin
  Values:= nil;
  Keys:= nil;
  try
    Values:= (Obj as TXRTLKeyValueContainer).GetValues;
    Keys:=   (Obj as TXRTLKeyValueContainer).GetKeys;
    Assert(Length(Values) = Length(Keys));
    Writer.WriteInteger(Length(Values));
    for I:= Low(Values) to High(Values) do
    begin
      Writer.WriteInterface(Keys[I],   True);
      Writer.WriteInterface(Values[I], True);
    end;
  finally
    Values:= nil;
    Keys:= nil;
  end;
end;

procedure TXRTLKeyValueContainerStreamer.ReadNoData(const Obj: TObject);
begin
  (Obj as TXRTLKeyValueContainer).Clear;
end;

procedure TXRTLKeyValueContainerStreamer.ReadObjectData(const Reader: IXRTLObjectReader;
  const Obj: TObject);
var
  Count, I: Integer;
  Key, Value: IXRTLValue;
begin
  (Obj as TXRTLKeyValueContainer).Clear;
  Count:= Reader.ReadInteger;
  for I:= 0 to Count - 1 do
  begin
    Key:=   Reader.ReadInterface(nil, True) as IXRTLValue;
    Value:= Reader.ReadInterface(nil, True) as IXRTLValue;
    (Obj as TXRTLKeyValueContainer).SetValue(Key, Value);
  end;
end;

end.

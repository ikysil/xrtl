unit xrtl_io_object_StdStreamer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_reflect_ClassDescriptor,
  xrtl_io_object_Serializer;

type
  TXRTLValueStreamer = class(TXRTLObjectStreamer)
  public
    procedure  ReadNoData(const Obj: TObject); override;
  end;

  TXRTLValueCardinalStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueIntegerStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueInt64Streamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueSingleStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueDoubleStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueExtendedStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueInterfaceStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueWideStringStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueObjectStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueVariantStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueCurrencyStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueCompStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueClassStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueGUIDStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TXRTLValueBooleanStreamer = class(TXRTLValueStreamer)
  public
    constructor Create;
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

procedure XRTLRegisterStdStreamers;

implementation

uses
  xrtl_util_ValueImpl;

procedure XRTLRegisterStdStreamers;
begin
  XRTLRegisterStreamer(TXRTLValueCardinalStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueIntegerStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueInt64Streamer.Create);
  XRTLRegisterStreamer(TXRTLValueSingleStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueDoubleStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueExtendedStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueInterfaceStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueWideStringStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueObjectStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueVariantStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueCurrencyStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueCompStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueClassStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueGUIDStreamer.Create);
  XRTLRegisterStreamer(TXRTLValueBooleanStreamer.Create);
end;

{ TXRTLValueStreamer }

procedure TXRTLValueStreamer.ReadNoData(const Obj: TObject);
begin
end;

{ TXRTLValueCardinalStreamer }

constructor TXRTLValueCardinalStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueCardinal));
end;

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

constructor TXRTLValueIntegerStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueInteger));
end;

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

constructor TXRTLValueInt64Streamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueInt64));
end;

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

constructor TXRTLValueSingleStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueSingle));
end;

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

constructor TXRTLValueDoubleStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueDouble));
end;

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

constructor TXRTLValueExtendedStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueExtended));
end;

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

constructor TXRTLValueInterfaceStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueInterface));
end;

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

constructor TXRTLValueWideStringStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueWideString));
end;

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

constructor TXRTLValueObjectStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueObject));
end;

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

constructor TXRTLValueVariantStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueVariant));
end;

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

constructor TXRTLValueCurrencyStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueCurrency));
end;

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

constructor TXRTLValueCompStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueComp));
end;

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

constructor TXRTLValueClassStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueClass));
end;

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

constructor TXRTLValueGUIDStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueGUID));
end;

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

constructor TXRTLValueBooleanStreamer.Create;
begin
  inherited Create(XRTLGetClassDescriptor(TXRTLValueBoolean));
end;

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

end.

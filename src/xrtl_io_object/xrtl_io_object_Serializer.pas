unit xrtl_io_object_Serializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Exception,
  xrtl_reflect_ClassDescriptor,
  xrtl_io_Stream;

type
  EXRTLSerializerException = class(EXRTLException);
  EXRTLObjectStreamerException = class(EXRTLSerializerException);

  IXRTLSerializer = interface
  ['{C255D455-F2C1-44FA-A47F-C37771143BF4}']
    procedure  WriteClass(const Stream: TXRTLOutputStream; const Obj: TClass;
                          const AllowShared: Boolean = True);
    procedure  WriteInterface(const Stream: TXRTLOutputStream; const Obj: IInterface;
                              const AllowShared: Boolean = True);
    procedure  WriteObject(const Stream: TXRTLOutputStream; const Obj: TObject;
                           const AllowShared: Boolean = True);
  end;

  IXRTLDeserializer = interface
  ['{C255D456-F2C1-44FA-A47F-C37771143BF4}']
    function   ReadClass(const Stream: TXRTLInputStream; const Obj: TClass;
                         const AllowShared: Boolean = True): TClass;
    function   ReadInterface(const Stream: TXRTLInputStream; const Obj: IInterface;
                             const AllowShared: Boolean = True): IInterface;
    function   ReadObject(const Stream: TXRTLInputStream; const Obj: TObject;
                          const AllowShared: Boolean = True): TObject;
  end;

  IXRTLObjectWriter = interface
  ['{FBC2460D-B3AF-4447-A246-A6336BA89DED}']
    procedure  WriteBoolean(AValue: Boolean);
    procedure  WriteByte(AValue: Byte);
    procedure  WriteCardinal(AValue: Cardinal);
    procedure  WriteComp(AValue: Comp);
    procedure  WriteCurrency(AValue: Currency);
    procedure  WriteDateTime(AValue: TDateTime);
    procedure  WriteDouble(AValue: Double);
    procedure  WriteExtended(AValue: Extended);
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

  IXRTLObjectReader = interface
  ['{FBC2460E-B3AF-4447-A246-A6336BA89DED}']
    function   ReadBoolean: Boolean;
    function   ReadByte: Byte;
    function   ReadCardinal: Cardinal;
    function   ReadComp: Comp;
    function   ReadCurrency: Currency;
    function   ReadDateTime: TDateTime;
    function   ReadDouble: Double;
    function   ReadExtended: Extended;
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

  IXRTLObjectStreamer = interface
  ['{34F068D3-2136-442C-B11A-69467F90333E}']
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject);
    procedure  ReadNoData(const Obj: TObject);
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject);
  end;

  TXRTLObjectStreamer = class(TInterfacedObject, IXRTLObjectStreamer)
  private
  public
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); virtual; abstract;
    procedure  ReadNoData(const Obj: TObject); virtual; abstract;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); virtual; abstract;
  end;

procedure XRTLRegisterStreamer(const Clazz: TClass; const Streamer: IXRTLObjectStreamer);
function  XRTLFindStreamer(const Clazz: TClass; var Streamer: IXRTLObjectStreamer): Boolean; overload;
function  XRTLFindParentStreamer(const Clazz: TClass; var Streamer: IXRTLObjectStreamer): Boolean;
function  XRTLGetStreamer(const Clazz: TClass): IXRTLObjectStreamer; overload;

implementation

uses
  xrtl_util_Compare, xrtl_util_Container, xrtl_util_Map, xrtl_util_Lock, xrtl_util_Value,
  xrtl_io_object_ResourceStrings, xrtl_io_object_StdClassDescriptor,
  xrtl_io_object_StdStreamer;

var
  FStreamers: TXRTLSynchronizedMap = nil;

procedure XRTLRegisterStreamer(const Clazz: TClass; const Streamer: IXRTLObjectStreamer);
var
  LStreamer: IXRTLObjectStreamer;
begin
  try
    FStreamers.BeginRead;
    if XRTLFindStreamer(Clazz, LStreamer) then
      raise EXRTLObjectStreamerException.CreateFmt(SXRTLStreamerForClassNameRegistered,
                                                    [Clazz.ClassName]);
    FStreamers.SetValue(XRTLValue(Clazz), XRTLValue(Streamer));
  finally
    FStreamers.EndRead;
  end;
end;

function XRTLFindStreamer(const Clazz: TClass; var Streamer: IXRTLObjectStreamer): Boolean;
var
  Value: IXRTLValue;
begin
  Streamer:= nil;
  Value:= FStreamers.GetValue(XRTLValue(Clazz));
  Result:= Assigned(Value);
  if Result then
    Streamer:= XRTLGetAsInterface(Value) as IXRTLObjectStreamer;
end;

function XRTLFindParentStreamer(const Clazz: TClass; var Streamer: IXRTLObjectStreamer): Boolean;
var
  LClass: TClass;
begin
  Result:= False;
  Streamer:= nil;
  LClass:= Clazz.ClassParent;
  while not Result and Assigned(LClass) do
  begin
    Result:= XRTLFindStreamer(LClass, Streamer);
    LClass:= LClass.ClassParent;
  end;
end;

function XRTLGetStreamer(const Clazz: TClass): IXRTLObjectStreamer; overload;
begin
  if not XRTLFindStreamer(Clazz, Result) then
    raise EXRTLObjectStreamerException.CreateFmt(SXRTLNoStreamerForClassNameRegistered, [Clazz.ClassName]);
end;

initialization
begin
  FStreamers:= TXRTLSynchronizedMap.Create(TXRTLArrayMap.Create, True);
  XRTLRegisterStdClassDescriptors;
  XRTLRegisterStdStreamers;
end;

finalization
begin
  FreeAndNil(FStreamers);
end;

end.

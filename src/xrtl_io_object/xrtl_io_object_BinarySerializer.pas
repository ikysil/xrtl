unit xrtl_io_object_BinarySerializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Array,
  xrtl_util_Container, xrtl_util_Value,
  xrtl_reflect_ClassDescriptor, xrtl_reflect_Introspector,
  xrtl_io_Stream,
  xrtl_io_object_Serializer, xrtl_io_object_Reference, xrtl_io_object_ReferenceMap;

type
  EXRTLBinarySerializerException   = class(EXRTLSerializerException);
  EXRTLBinaryDeserializerException = class(EXRTLSerializerException);

  TXRTLBinarySerializer = class(TInterfacedObject, IXRTLSerializer)
  private
    FRefMap: TXRTLReferenceMap;
    procedure  WriteClassDescriptor(const Stream: TXRTLOutputStream;
                                    const Obj: TObject);
    procedure  WriteClassDescriptors(const Stream: TXRTLOutputStream;
                                     const Descriptors: TXRTLSequentialContainer);
    procedure  WriteInstanceData(const Stream: TXRTLOutputStream;
                                 const Descriptors: TXRTLSequentialContainer;
                                 const Obj: TObject);
    procedure  WriteClassInstanceData(const Stream: TXRTLOutputStream;
                                      const Descriptor: IXRTLClassDescriptor;
                                      const Obj: TObject);
    procedure  WritePropertyValue(const Stream: TXRTLOutputStream; const Obj: IXRTLValue);
    procedure  WriteProperty(const Stream: TXRTLOutputStream;
                             const AProperty: IXRTLProperty);
  protected
    function   CheckAndWriteNilReference(const Stream: TXRTLOutputStream; const Obj): Boolean;
    function   WriteReference(const Stream: TXRTLOutputStream; const Obj: TObject;
                              const AllowShared: Boolean): Boolean;
    procedure  WriteObjectData(const Stream: TXRTLOutputStream; const Obj: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  WriteClass(const Stream: TXRTLOutputStream; const Obj: TClass;
                          const AllowShared: Boolean = True);
    procedure  WriteInterface(const Stream: TXRTLOutputStream; const Obj: IInterface;
                              const AllowShared: Boolean = True);
    procedure  WriteObject(const Stream: TXRTLOutputStream; const Obj: TObject;
                           const AllowShared: Boolean = True);
  end;

  TXRTLBinaryDeserializer = class(TInterfacedObject, IXRTLDeserializer)
  private
    FRefMap: TXRTLReferenceMap;
    procedure  ReadObjectData(const Stream: TXRTLInputStream; const AllowShared: Boolean;
                              var Ref: TXRTLInstanceReference; var Obj: TObject);
    function   ReadReference(const Stream: TXRTLInputStream): TXRTLInstanceReference;
    function   GetInstanceFromReference(var Ref: TXRTLInstanceReference;
                                        var Obj: TObject; var AllowShared: Boolean): Boolean;
    procedure  ReadClassDescriptor(const Stream: TXRTLInputStream; var Obj: TObject);
    function   ReadClassDescriptors(const Stream: TXRTLInputStream): TXRTLSequentialContainer;
    procedure  ReadInstanceData(const Stream: TXRTLInputStream;
                                const LDescriptors, SDescriptors: TXRTLSequentialContainer;
                                const Obj: TObject);
    procedure  ReadClassInstanceData(const Stream: TXRTLInputStream;
                                     const Descriptor: IXRTLClassDescriptor;
                                     const Obj: TObject);
    procedure  ReadNoClassInstanceData(const Stream: TXRTLInputStream;
                                       const Descriptor: IXRTLClassDescriptor;
                                       const Obj: TObject);
    procedure  SkipClassInstanceData(const Stream: TXRTLInputStream);
    procedure  ReadPropertyValue(const Stream: TXRTLInputStream; const Obj: IXRTLValue);
    procedure  ReadProperty(const Stream: TXRTLInputStream;
                            const PropList: IXRTLPropertyList);
  public
    constructor Create;
    destructor Destroy; override;
    function   ReadClass(const Stream: TXRTLInputStream; const Obj: TClass;
                         const AllowShared: Boolean = True): TClass;
    function   ReadInterface(const Stream: TXRTLInputStream; const Obj: IInterface;
                             const AllowShared: Boolean = True): IInterface;
    function   ReadObject(const Stream: TXRTLInputStream; const Obj: TObject;
                          const AllowShared: Boolean = True): TObject;
  end;
  
implementation

uses
  xrtl_io_BlockStream, xrtl_io_DataStream,
  xrtl_io_object_BinarySerializerClasses;

{ TXRTLBinarySerializer }

constructor TXRTLBinarySerializer.Create;
begin
  inherited Create;
  FRefMap:= TXRTLReferenceMap.Create;
end;

destructor TXRTLBinarySerializer.Destroy;
begin
  FreeAndNil(FRefMap);
  inherited;
end;

function TXRTLBinarySerializer.WriteReference(const Stream: TXRTLOutputStream;
  const Obj: TObject; const AllowShared: Boolean): Boolean;
var
  Ref: TXRTLInstanceReference;
begin
  Ref:= nil;
  Result:= False;
  try
    if AllowShared then
    begin
//  check for shared instance and return shared instance reference
      Ref:= FRefMap.GetReference(Obj);
      Result:= Assigned(Ref);
      if not Result then
      begin
//  create new instance reference
        Ref:= TXRTLInstanceReference.Create(Obj, AllowShared);
      end;
      WriteObjectData(Stream, Ref);
//  if reference is old (Result = true) or is registered successfully then
//  set Ref to nil to bypass FreeAndNil below
      if Result or FRefMap.RegisterReference(Ref, Obj) then
      begin
        Ref:= nil; // to bypass FreeAndNil below
      end;
    end
    else
    begin
//  create unique instance reference
      Ref:= TXRTLInstanceReference.Create(Obj, AllowShared);
      WriteObjectData(Stream, Ref);
    end;
  finally
    FreeAndNil(Ref);
  end;
end;

function TXRTLBinarySerializer.CheckAndWriteNilReference(const Stream: TXRTLOutputStream;
  const Obj): Boolean;
begin
  Result:= Pointer(Obj) = nil;
  if Result then
  begin
//  write nil reference
    WriteObjectData(Stream, TXRTLInstanceReference.Create(nil, False));
  end;
end;

procedure TXRTLBinarySerializer.WriteClass(const Stream: TXRTLOutputStream;
  const Obj: TClass; const AllowShared: Boolean = True);
var
  LDescriptor: IXRTLClassDescriptor;
begin
  LDescriptor:= nil;
  if Assigned(Obj) then
  begin
    LDescriptor:= XRTLGetClassDescriptor(Obj);
    WriteInterface(Stream, LDescriptor, AllowShared);
  end
  else
  begin
    WriteInterface(Stream, nil, False);
  end;
end;

procedure TXRTLBinarySerializer.WriteInterface(const Stream: TXRTLOutputStream;
  const Obj: IInterface; const AllowShared: Boolean = True);
var
  LObj: IXRTLImplementationObjectProvider;
begin
  if Assigned(Obj) then
  begin
    if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
      raise EXRTLBinarySerializerException.Create('Invalid interface reference, can''t serialize');
    WriteObject(Stream, LObj.GetImplementationObject, AllowShared);
  end
  else
  begin
    WriteObject(Stream, nil, False);
  end;
end;

procedure TXRTLBinarySerializer.WriteObject(const Stream: TXRTLOutputStream;
  const Obj: TObject; const AllowShared: Boolean = True);
var
  LAllowShared: Boolean;
  LStream: TXRTLOutputStream;

  procedure WriteInstanceReference(const Obj: TXRTLInstanceReference);
  var
    Ref: TXRTLInstanceReference;
  begin
    Ref:= nil;
    try
      Ref:= TXRTLInstanceReference.CreateSelfReference(Obj as TXRTLInstanceReference);
      WriteObjectData(LStream, Ref);
    finally
      FreeAndNil(Ref);
    end;
  end;

begin
  LStream:= nil;
  try
    LStream:= TXRTLBlockOutputStream.Create(Stream, False);
    if CheckAndWriteNilReference(LStream, Obj) then
      Exit;
    LAllowShared:= AllowShared;
    if Obj is TXRTLInstanceReference then
    begin
      WriteInstanceReference(Obj as TXRTLInstanceReference);
      Exit;
    end;
    if WriteReference(LStream, Obj, LAllowShared) then
      Exit;
    WriteObjectData(LStream, Obj);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinarySerializer.WriteObjectData(const Stream: TXRTLOutputStream;
  const Obj: TObject);
var
  LDescriptors: TXRTLSequentialContainer;
  CDescriptor: IXRTLClassDescriptor;
begin
  LDescriptors:= nil;
  try
//  get class descriptors in descendant - ancestor order
    LDescriptors:= XRTLFindHierarchyClassDescriptors(Obj.ClassType);
    CDescriptor:= XRTLGetAsInterface(LDescriptors.GetValue(LDescriptors.AtBegin)) as IXRTLClassDescriptor;
    WriteClassDescriptors(Stream, LDescriptors);
    if WideCompareStr(CDescriptor.GetClassId, XRTLClassDescriptorId) = 0 then
    begin
      WriteClassDescriptor(Stream, Obj);
    end
    else
    begin
      WriteInstanceData(Stream, LDescriptors, Obj);
    end;
  finally
    FreeAndNil(LDescriptors);
  end;
end;

procedure TXRTLBinarySerializer.WriteClassDescriptors(const Stream: TXRTLOutputStream;
  const Descriptors: TXRTLSequentialContainer);
var
  I: Integer;
  DStream: TXRTLDataOutputStream;
  Values: TXRTLValueArray;
  LDescriptor: IXRTLClassDescriptor;
  LClassId: WideString;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(TXRTLBlockOutputStream.Create(Stream, False), True);
    Values:= Descriptors.GetValues;
    for I:= Low(Values) to High(Values) do
    begin
      LDescriptor:= XRTLGetAsInterface(Values[I]) as IXRTLClassDescriptor;
      LClassId:= LDescriptor.GetClassId;
      DStream.WriteUTF8String(LClassId);
    end;
  finally
    FreeAndNil(DStream);
    Values:= nil;
  end;
end;

procedure TXRTLBinarySerializer.WriteClassDescriptor(const Stream: TXRTLOutputStream;
  const Obj: TObject);
var
  DStream: TXRTLDataOutputStream;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(TXRTLBlockOutputStream.Create(Stream, False), True);
    DStream.WriteUTF8String((Obj as TXRTLClassDescriptor).GetClassId);
  finally
    FreeAndNil(DStream);
  end;
end;

procedure TXRTLBinarySerializer.WriteInstanceData(const Stream: TXRTLOutputStream;
  const Descriptors: TXRTLSequentialContainer; const Obj: TObject);
var
  I: Integer;
  LStream: TXRTLOutputStream;
  Values: TXRTLValueArray;
  LDescriptor: IXRTLClassDescriptor;
begin
  LStream:= nil;
  try
    LStream:= TXRTLBlockOutputStream.Create(Stream, False);
    Values:= Descriptors.GetValues;
    for I:= High(Values) downto Low(Values) do
    begin
      LDescriptor:= XRTLGetAsInterface(Values[I]) as IXRTLClassDescriptor;
      WriteClassInstanceData(LStream, LDescriptor, Obj);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinarySerializer.WriteClassInstanceData(const Stream: TXRTLOutputStream;
  const Descriptor: IXRTLClassDescriptor; const Obj: TObject);
var
  I: Integer;
  LStream: TXRTLOutputStream;
  LPropList: IXRTLPropertyList;
  LProps: TXRTLArray;
  Values: TXRTLValueArray;
  LStreamer: IXRTLObjectStreamer;
begin
  LStream:= nil;
  LProps:= nil;
  try
    LStream:= TXRTLBlockOutputStream.Create(Stream, False);
    LProps:= TXRTLArray.Create;
    LPropList:= Descriptor.DefineProperties;
    Descriptor.GetValues(Obj, LPropList);
    LPropList.GetProperties(LProps);
    Values:= LProps.GetValues;
    for I:= Low(Values) to High(Values) do
    begin
      WriteProperty(LStream, XRTLGetAsInterface(Values[I]) as IXRTLProperty);
    end;
  finally
    Values:= nil;
    FreeAndNil(LProps);
    FreeAndNil(LStream);
  end;
//  write custom data
  LStream:= nil;
  try
    LStream:= TXRTLBlockOutputStream.Create(Stream, False);
    if XRTLStreamerRegistry.Find(Descriptor.GetClass, LStreamer) then
    begin
      LStreamer.WriteObjectData(TXRTLBinaryObjectWriter.Create(LStream, Self), Obj);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinarySerializer.WritePropertyValue(const Stream: TXRTLOutputStream;
  const Obj: IXRTLValue);
var
  LObj: IXRTLImplementationObjectProvider;
begin
  if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
    raise EXRTLBinarySerializerException.Create('Invalid interface reference, can''t serialize');
  WriteObjectData(Stream, LObj.GetImplementationObject);
end;

procedure TXRTLBinarySerializer.WriteProperty(const Stream: TXRTLOutputStream;
  const AProperty: IXRTLProperty);
var
  DStream: TXRTLDataOutputStream;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(TXRTLBlockOutputStream.Create(Stream, False), True);
    DStream.WriteUTF8String(AProperty.Name);
    WritePropertyValue(DStream, AProperty.Value); 
  finally
    FreeAndNil(DStream);
  end;
end;

{ TXRTLBinaryDeserializer }

constructor TXRTLBinaryDeserializer.Create;
begin
  inherited;
  FRefMap:= TXRTLReferenceMap.Create;
end;

destructor TXRTLBinaryDeserializer.Destroy;
begin
  FreeAndNil(FRefMap);
  inherited;
end;

function TXRTLBinaryDeserializer.ReadClass(const Stream: TXRTLInputStream;
  const Obj: TClass; const AllowShared: Boolean = True): TClass;
var
  RObj: IInterface;
  LDescriptor: IXRTLClassDescriptor;
begin
  Result:= nil;
  LDescriptor:= nil;
  if Assigned(Obj) then
    XRTLFindClassDescriptor(Obj, LDescriptor);
  RObj:= ReadInterface(Stream, LDescriptor, AllowShared);
  if not Assigned(RObj) then Exit;
  LDescriptor:= RObj as IXRTLClassDescriptor;
  Result:= LDescriptor.GetClass;
end;

function TXRTLBinaryDeserializer.ReadInterface(const Stream: TXRTLInputStream;
  const Obj: IInterface; const AllowShared: Boolean = True): IInterface;
var
  LObj: IXRTLImplementationObjectProvider;
  RObj: TObject;
begin
  Result:= nil;
  LObj:= nil;
  if Assigned(Obj) then
  begin
    if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
      raise EXRTLBinaryDeserializerException.Create('Invalid interface reference, can''t deserialize');
  end;
  RObj:= ReadObject(Stream, LObj.GetImplementationObject, AllowShared);
  if not Assigned(RObj) then Exit;
  RObj.GetInterface(IInterface, Result);
end;

function TXRTLBinaryDeserializer.ReadObject(const Stream: TXRTLInputStream;
  const Obj: TObject; const AllowShared: Boolean = True): TObject;
var
  Ref: TXRTLInstanceReference;
  LAllowShared: Boolean;
  LStream: TXRTLInputStream;
begin
  Result:= Obj;
  Ref:= nil;
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    LAllowShared:= AllowShared;
    Ref:= ReadReference(LStream);
    if GetInstanceFromReference(Ref, Result, LAllowShared) then
      Exit;
    ReadObjectData(LStream, LAllowShared, Ref, Result);
  finally
    FreeAndNil(Ref);
    FreeAndNil(LStream);
  end;
end;

function TXRTLBinaryDeserializer.ReadReference(const Stream: TXRTLInputStream): TXRTLInstanceReference;
var
  RObj: TObject;
begin
  RObj:= nil;
  Result:= nil;
  try
    RObj:= TXRTLInstanceReference.Create;
    ReadObjectData(Stream, False, Result, RObj);
    Result:= RObj as TXRTLInstanceReference;
  except
    FreeAndNil(RObj);
    raise;
  end;
end;

function TXRTLBinaryDeserializer.GetInstanceFromReference(var Ref: TXRTLInstanceReference;
  var Obj: TObject; var AllowShared: Boolean): Boolean;
var
  LObj: TObject;
begin
  Result:= False;
  if Ref.IsNil then
  begin
    Obj:= nil;
    AllowShared:= False;
    Result:= True;
    Exit;
  end;
  if Ref.SelfRef then
  begin
    Obj:= Ref;
    Ref:= nil;
    AllowShared:= False;
    Result:= True;
    Exit;
  end;
  if not (AllowShared and Ref.AllowShared) then Exit;
  LObj:= FRefMap.GetObject(Ref);
  Result:= Assigned(LObj);
  if Result then
    Obj:= LObj;
end;

procedure TXRTLBinaryDeserializer.ReadObjectData(const Stream: TXRTLInputStream;
  const AllowShared: Boolean; var Ref: TXRTLInstanceReference; var Obj: TObject);
var
  LDescriptors, SDescriptors: TXRTLSequentialContainer;
  CDescriptor: IXRTLClassDescriptor;
begin
  SDescriptors:= nil;
  LDescriptors:= nil;
  try
    SDescriptors:= ReadClassDescriptors(Stream);
    Assert(not SDescriptors.IsEmpty);
    CDescriptor:= XRTLGetAsInterface(SDescriptors.GetValue(SDescriptors.AtBegin)) as IXRTLClassDescriptor;
    if WideCompareStr(CDescriptor.GetClassId, XRTLClassDescriptorId) = 0 then
    begin
      ReadClassDescriptor(Stream, Obj);
      Exit;
    end;
    if Assigned(Obj) then
    begin
      if Assigned(CDescriptor.GetClass) then
      begin
        if not(Obj is CDescriptor.GetClass) then
          raise EXRTLBinaryDeserializerException.CreateFmt('Can''t deserialize, class ''%s'' expected but ''%s'' instance provided...',
                  [CDescriptor.GetClass.ClassName, Obj.ClassType]);
      end;
//  get class descriptors in descendant - ancestor order
      LDescriptors:= XRTLFindHierarchyClassDescriptors(Obj.ClassType);
    end
    else
    begin
      Obj:= CDescriptor.CreateInstance;
      if not Assigned(Obj) then
        raise EXRTLBinaryDeserializerException.CreateFmt('Can''t create instance for class id ''%s''...', [CDescriptor.GetClassId]);
//  get class descriptors in descendant - ancestor order
      LDescriptors:= XRTLFindHierarchyClassDescriptors(CDescriptor.GetClass);
    end;
//  if AllowShared and reference is registered successfully then
//  set Ref to nil to bypass FreeAndNil below
    if AllowShared and FRefMap.RegisterReference(Ref, Obj) then
      Ref:= nil; // to bypass FreeAndNil below
    ReadInstanceData(Stream, LDescriptors, SDescriptors, Obj);
  finally
    FreeAndNil(SDescriptors);
    FreeAndNil(LDescriptors);
  end;
end;

function TXRTLBinaryDeserializer.ReadClassDescriptors(const Stream: TXRTLInputStream): TXRTLSequentialContainer;
var
  BStream: TXRTLBlockInputStream;
  DStream: TXRTLDataInputStream;
  LDescriptor: IXRTLClassDescriptor;
  LClassId: WideString;
begin
  Result:= TXRTLArray.Create;
  DStream:= nil;
  try
    BStream:= TXRTLBlockInputStream.Create(Stream, False);
    DStream:= TXRTLDataInputStream.Create(BStream, True);
    while not BStream.IsEndOfStream do
    begin
      LClassId:= DStream.ReadUTF8String;
      LDescriptor:= nil;
      if not XRTLFindClassDescriptor(LClassId, LDescriptor) then
        LDescriptor:= TXRTLClassDescriptor.Create(LClassId);
      Result.Insert(XRTLValue(LDescriptor), Result.AtEnd);
    end;
  finally
    FreeAndNil(DStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadClassDescriptor(const Stream: TXRTLInputStream;
  var Obj: TObject);
var
  DStream: TXRTLDataInputStream;
  LDescriptor: IXRTLClassDescriptor;
  LClassId: WideString;
begin
  DStream:= nil;
  LDescriptor:= nil;
  try
    DStream:= TXRTLDataInputStream.Create(TXRTLBlockInputStream.Create(Stream, False), True);
    LClassId:= DStream.ReadUTF8String;
    if XRTLFindClassDescriptor(LClassId, LDescriptor) then
    begin
      Obj:= (LDescriptor as IXRTLImplementationObjectProvider).GetImplementationObject;
    end
    else
    begin
      Obj:= TXRTLClassDescriptor.Create(LClassId);
    end;
  finally
    FreeAndNil(DStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadInstanceData(const Stream: TXRTLInputStream;
  const LDescriptors, SDescriptors: TXRTLSequentialContainer; const Obj: TObject);
var
  I, K, LValuesHigh: Integer;
  LStream: TXRTLInputStream;
  LValues, SValues: TXRTLValueArray;
  LDescriptor, SDescriptor: IXRTLClassDescriptor;

  function HasLocalDescriptor(const Descriptor: IXRTLClassDescriptor): Boolean;
  var
    I: Integer;
    LDescriptor: IXRTLClassDescriptor;
  begin
    Result:= False;
    for I:= LValuesHigh downto Low(LValues) do
    begin
      LDescriptor:= XRTLGetAsInterface(LValues[I]) as IXRTLClassDescriptor;
      Result:= WideCompareStr(Descriptor.GetClassId, LDescriptor.GetClassId) = 0;
      if Result then Exit;
    end;
  end;

begin
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    LValues:= LDescriptors.GetValues;
    SValues:= SDescriptors.GetValues;
    LValuesHigh:= High(LValues);
    for I:= High(SValues) downto Low(SValues) do
    begin
      SDescriptor:= XRTLGetAsInterface(SValues[I]) as IXRTLClassDescriptor;
      if HasLocalDescriptor(SDescriptor) then
      begin
        for K:= LValuesHigh downto Low(LValues) do
        begin
          LDescriptor:= XRTLGetAsInterface(LValues[K]) as IXRTLClassDescriptor;
          Dec(LValuesHigh);
          if WideCompareStr(SDescriptor.GetClassId, LDescriptor.GetClassId) = 0 then
          begin
            ReadClassInstanceData(LStream, SDescriptor, Obj);
            Break;
          end
          else
          begin
            ReadNoClassInstanceData(LStream, LDescriptor, Obj);
          end;
        end;
      end
      else
      begin
        SkipClassInstanceData(LStream);
      end;
    end;
    Assert(LValuesHigh < 0);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadClassInstanceData(const Stream: TXRTLInputStream;
  const Descriptor: IXRTLClassDescriptor; const Obj: TObject);
var
  LStream: TXRTLBlockInputStream;
  LPropList: IXRTLPropertyList;
  LProps: TXRTLArray;
  Values: TXRTLValueArray;
  LStreamer: IXRTLObjectStreamer;
begin
  LStream:= nil;
  LProps:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    LPropList:= Descriptor.DefineProperties;
    while not LStream.IsEndOfStream do
      ReadProperty(LStream, LPropList);
    Descriptor.SetValues(Obj, LPropList);
  finally
    Values:= nil;
    FreeAndNil(LProps);
    FreeAndNil(LStream);
  end;
//  read custom data
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    if XRTLStreamerRegistry.Find(Descriptor.GetClass, LStreamer) then
    begin
      LStreamer.ReadObjectData(TXRTLBinaryObjectReader.Create(LStream, Self), Obj);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadNoClassInstanceData(const Stream: TXRTLInputStream;
  const Descriptor: IXRTLClassDescriptor; const Obj: TObject);
var
  LStream: TXRTLInputStream;
  LPropList: IXRTLPropertyList;
  LProps: TXRTLArray;
  Values: TXRTLValueArray;
  LStreamer: IXRTLObjectStreamer;
begin
  LStream:= nil;
  LProps:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    LPropList:= Descriptor.DefineProperties;
    Descriptor.SetValues(Obj, LPropList);
  finally
    Values:= nil;
    FreeAndNil(LProps);
//  automaticly skips property data
    FreeAndNil(LStream);
  end;
//  read custom data
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
    if XRTLStreamerRegistry.Find(Descriptor.GetClass, LStreamer) then
    begin
      LStreamer.ReadNoData(Obj);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinaryDeserializer.SkipClassInstanceData(const Stream: TXRTLInputStream);
var
  LStream: TXRTLInputStream;
begin
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
  finally
//  automatically skips property data
    FreeAndNil(LStream);
  end;
//  read custom data
  LStream:= nil;
  try
    LStream:= TXRTLBlockInputStream.Create(Stream, False);
  finally
//  automatically skips custom data
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadPropertyValue(const Stream: TXRTLInputStream;
  const Obj: IXRTLValue);
var
  LObj: IXRTLImplementationObjectProvider;
  RObj: TObject;
  Ref: TXRTLInstanceReference;
begin
  LObj:= nil;
  Ref:= nil;
  if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
    raise EXRTLBinaryDeserializerException.Create('Invalid interface reference, can''t deserialize');
  RObj:= LObj.GetImplementationObject;
  ReadObjectData(Stream, False, Ref, RObj);
end;

procedure TXRTLBinaryDeserializer.ReadProperty(const Stream: TXRTLInputStream;
  const PropList: IXRTLPropertyList);
var
  DStream: TXRTLDataInputStream;
  PropertyName: WideString;
  LProperty: IXRTLProperty;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataInputStream.Create(TXRTLBlockInputStream.Create(Stream, False), True);
    PropertyName:= DStream.ReadUTF8String;
    LProperty:= PropList.GetByName(PropertyName);
    if Assigned(LProperty) then
      ReadPropertyValue(DStream, LProperty.Value);
  finally
    FreeAndNil(DStream);
  end;
end;

end.

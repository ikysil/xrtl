unit xrtl_io_object_BinarySerializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Array,
  xrtl_util_Container, xrtl_util_Stack, xrtl_util_Value,
  xrtl_reflect_ClassDescriptor, xrtl_reflect_Property,
  xrtl_io_Stream,
  xrtl_io_object_Serializer, xrtl_io_object_Reference;

type
  EXRTLBinarySerializerException = class(EXRTLSerializerException);
  EXRTLBinaryDeserializerException = class(EXRTLSerializerException);

  TXRTLBinarySerializer = class(TInterfacedObject, IXRTLSerializer)
  private
    procedure  WriteClassDescriptors(const Stream: TXRTLOutputStream;
                                     const Descriptors: TXRTLSequentialContainer);
    procedure  WriteInstanceData(const Stream: TXRTLOutputStream;
                                 const Descriptors: TXRTLSequentialContainer;
                                 const Obj: TObject);
    procedure  WriteClassInstanceData(const Stream: TXRTLOutputStream;
                                      const Descriptor: IXRTLClassDescriptor;
                                      const Obj: TObject);
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
    function   ReadReference(const Stream: TXRTLInputStream): TXRTLInstanceReference;
    function   GetInstanceFromReference(var Ref: TXRTLInstanceReference;
                                        var Obj: TObject; var AllowShared: Boolean): Boolean;
    function   ReadClassDescriptors(const Stream: TXRTLInputStream): TXRTLSequentialContainer;
    procedure  ReadInstanceData(const Stream: TXRTLInputStream;
                                const Obj: TObject; const LDescriptors,
                                SDescriptors: TXRTLSequentialContainer);
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
  xrtl_util_Guard,
  xrtl_io_BlockStream, xrtl_io_DataStream,
  xrtl_io_object_BinarySerializerClasses,
  xrtl_reflect_PropertyList, xrtl_util_Compare;

{ TXRTLBinarySerializer }

constructor TXRTLBinarySerializer.Create;
begin
  inherited Create;
end;

destructor TXRTLBinarySerializer.Destroy;
begin
  inherited;
end;

function TXRTLBinarySerializer.WriteReference(const Stream: TXRTLOutputStream;
  const Obj: TObject; const AllowShared: Boolean): Boolean;
var
  Ref: TXRTLInstanceReference;
begin
  Ref:= nil;
  Result:= False;
  if AllowShared then
  begin
// TO DO: check for shared instance and return shared instance reference
    //Ref:= get shared reference
    Result:= Assigned(Ref);
    if not Result then
    begin
//  create and register new instance reference
      Ref:= TXRTLInstanceReference.Create(Obj, AllowShared);
//  register ref
    end;
    WriteObjectData(Stream, Ref);
  end
  else
  begin
    try
//  create unique instance reference
      Ref:= TXRTLInstanceReference.Create(Obj, AllowShared);
      WriteObjectData(Stream, Ref);
    finally
      FreeAndNil(Ref);
    end;
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
  if CheckAndWriteNilReference(Stream, Obj) then
    Exit;
  LDescriptor:= XRTLGetClassDescriptor(Obj);
  WriteInterface(Stream, LDescriptor, AllowShared);
end;

procedure TXRTLBinarySerializer.WriteInterface(const Stream: TXRTLOutputStream;
  const Obj: IInterface; const AllowShared: Boolean = True);
var
  LObj: IXRTLImplementationObjectProvider;
begin
  if CheckAndWriteNilReference(Stream, Obj) then
    Exit;
  if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
    raise EXRTLBinarySerializerException.Create('Invalid interface reference, can''t serialize');
  WriteObject(Stream, LObj.GetImplementationObject, AllowShared);
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
    if Obj is TXRTLClassDescriptor then
    begin
      LAllowShared:= False;
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
begin
  LDescriptors:= nil;
  try
    LDescriptors:= XRTLFindHierarchyClassDescriptors(Obj.ClassType);
    WriteClassDescriptors(Stream, LDescriptors);
    WriteInstanceData(Stream, LDescriptors, Obj);
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
//  write class descriptors in descendant - ancestor order
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(
                TXRTLBlockOutputStream.Create(Stream, False), True);
    Values:= Descriptors.GetValues;
    for I:= High(Values) to Low(Values) do
    begin
      LDescriptor:= XRTLGetAsInterface(Values[I]) as IXRTLClassDescriptor;
      LClassId:= LDescriptor.GetClassId;
      DStream.WriteUTF8String(LClassId);
    end;
    DStream.WriteUTF8String(XRTLEndOfHierarchyClassId);
  finally
    FreeAndNil(DStream);
    Values:= nil;
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
    for I:= Low(Values) to High(Values) do
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
    Descriptor.Introspector.GetValues(Obj, LPropList);
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
    if XRTLFindStreamer(Descriptor.GetClass, LStreamer) then
    begin
      LStreamer.WriteObjectData(TXRTLBinaryObjectWriter.Create(LStream, Self), Obj);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinarySerializer.WriteProperty(const Stream: TXRTLOutputStream;
  const AProperty: IXRTLProperty);
var
  DStream: TXRTLDataOutputStream;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(
                TXRTLBlockOutputStream.Create(Stream, False), True);
    DStream.WriteUTF8String(AProperty.Name);
    WriteInterface(DStream, AProperty.Value, False); 
  finally
    FreeAndNil(DStream);
  end;
end;

{ TXRTLBinaryDeserializer }

constructor TXRTLBinaryDeserializer.Create;
begin
  inherited;
end;

destructor TXRTLBinaryDeserializer.Destroy;
begin
  inherited;
end;

function TXRTLBinaryDeserializer.ReadClass(const Stream: TXRTLInputStream;
  const Obj: TClass; const AllowShared: Boolean = True): TClass;
var
  LDescriptor: IXRTLClassDescriptor;
begin
  LDescriptor:= nil;
  if Assigned(Obj) then
    XRTLFindClassDescriptor(Obj, LDescriptor);
  LDescriptor:= ReadInterface(Stream, LDescriptor, AllowShared) as IXRTLClassDescriptor;
  Result:= LDescriptor.GetClass;
end;

function TXRTLBinaryDeserializer.ReadInterface(const Stream: TXRTLInputStream;
  const Obj: IInterface; const AllowShared: Boolean = True): IInterface;
var
  LObj: IXRTLImplementationObjectProvider;
  RObj: TObject;
begin
  LObj:= nil;
  if Assigned(Obj) then
  begin
    if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
      raise EXRTLBinaryDeserializerException.Create('Invalid interface reference, can''t deserialize');
  end;
  RObj:= ReadObject(Stream, LObj.GetImplementationObject, AllowShared);
  RObj.GetInterface(IInterface, Result);
end;

function TXRTLBinaryDeserializer.ReadObject(const Stream: TXRTLInputStream;
  const Obj: TObject; const AllowShared: Boolean = True): TObject;
var
  Ref: TXRTLInstanceReference;
  LDescriptors, SDescriptors: TXRTLSequentialContainer;
  CDescriptor: IXRTLClassDescriptor;
  LAllowShared: Boolean;
begin
  Result:= Obj;
  Ref:= nil;
  SDescriptors:= nil;
  LDescriptors:= nil;
  try
    LAllowShared:= AllowShared;
    Ref:= ReadReference(Stream);
    if GetInstanceFromReference(Ref, Result, LAllowShared) then
      Exit;
    SDescriptors:= ReadClassDescriptors(Stream);
    CDescriptor:= XRTLGetAsInterface(SDescriptors.GetValue(SDescriptors.AtBegin)) as IXRTLClassDescriptor;
    if Assigned(Result) then
    begin
      LDescriptors:= XRTLFindHierarchyClassDescriptors(Result.ClassType);
    end
    else
    begin
      if not Assigned(CDescriptor.Factory) then
        raise EXRTLBinaryDeserializerException.CreateFmt('Can''t deserialize, no class descriptor for class id ''%s'' found...', [CDescriptor.GetClassId]);
      LDescriptors:= XRTLFindHierarchyClassDescriptors(CDescriptor.GetClass);
      Result:= CDescriptor.Factory.CreateInstance;
    end;
    ReadInstanceData(Stream, Result, LDescriptors, SDescriptors);
  finally
    FreeAndNil(Ref);
    FreeAndNil(SDescriptors);
    FreeAndNil(LDescriptors);
  end;
end;

function TXRTLBinaryDeserializer.ReadReference(const Stream: TXRTLInputStream): TXRTLInstanceReference;
var
  LDescriptors, SDescriptors: TXRTLSequentialContainer;
begin
  LDescriptors:= nil;
  SDescriptors:= nil;
  try
    LDescriptors:= XRTLFindHierarchyClassDescriptors(TXRTLInstanceReference);
    SDescriptors:= ReadClassDescriptors(Stream);
    Result:= TXRTLInstanceReference.Create;
    ReadInstanceData(Stream, Result, LDescriptors, SDescriptors);
  finally
    FreeAndNil(SDescriptors);
    FreeAndNil(LDescriptors);
  end;
end;

function TXRTLBinaryDeserializer.GetInstanceFromReference(var Ref: TXRTLInstanceReference;
  var Obj: TObject; var AllowShared: Boolean): Boolean;
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
end;

function TXRTLBinaryDeserializer.ReadClassDescriptors(const Stream: TXRTLInputStream): TXRTLSequentialContainer;
var
  DStream: TXRTLDataInputStream;
  LDescriptor: IXRTLClassDescriptor;
  LClassId: WideString;
begin
  Result:= TXRTLArray.Create;
//  read class descriptors in descendant - ancestor order
  DStream:= nil;
  try
    DStream:= TXRTLDataInputStream.Create(
                TXRTLBlockInputStream.Create(Stream, False), True);
    while True do
    begin
      LClassId:= DStream.ReadUTF8String;
      if WideCompareStr(LClassId, XRTLEndOfHierarchyClassId) = 0 then
        Break;
      LDescriptor:= nil;
      if not XRTLFindClassDescriptor(LClassId, LDescriptor) then
        LDescriptor:= TXRTLClassDescriptor.Create(LClassId);
      Result.Insert(XRTLValue(LDescriptor), Result.AtEnd);
    end;
  finally
    FreeAndNil(DStream);
  end;
end;

procedure TXRTLBinaryDeserializer.ReadInstanceData(const Stream: TXRTLInputStream;
  const Obj: TObject; const LDescriptors, SDescriptors: TXRTLSequentialContainer);
begin
end;

end.

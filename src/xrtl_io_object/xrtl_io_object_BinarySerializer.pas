unit xrtl_io_object_BinarySerializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Array,
  xrtl_reflect_ClassDescriptor, xrtl_reflect_Property,
  xrtl_io_Stream,
  xrtl_io_object_Serializer;

type
  EXRTLBinarySerializerException = class(EXRTLSerializerException);

  TXRTLBinarySerializer = class(TInterfacedObject, IXRTLSerializer)
  private
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

implementation

uses
  xrtl_util_Guard, xrtl_util_Container, xrtl_util_Stack, xrtl_util_Value,
  xrtl_io_BlockStream, xrtl_io_DataStream,
  xrtl_io_object_BinarySerializerClasses, xrtl_io_object_Reference,
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

function TXRTLBinarySerializer.WriteReference(const Stream: TXRTLOutputStream; const Obj: TObject;
  const AllowShared: Boolean): Boolean;
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
{
    LDescriptor:= XRTLGetClassDescriptor(Obj.ClassType);
    WriteObject(LStream, (LDescriptor as IXRTLImplementationObjectProvider).GetImplementationObject, False);
    if WideCompareStr(LDescriptor.GetClassId, XRTLClassDescriptorId) = 0 then
    begin
      WriteClassDescriptor(LDescriptor);
      Exit;
    end;
}
    WriteObjectData(LStream, Obj);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TXRTLBinarySerializer.WriteObjectData(const Stream: TXRTLOutputStream;
  const Obj: TObject);
var
  LDescriptors: TXRTLStack;
  LDescriptor: IXRTLClassDescriptor;
  LClass: TClass;
  LStream, OStream: TXRTLOutputStream;
  LProperties: IXRTLPropertyList;
  Iter: IXRTLIterator;
  LProps: TXRTLArray;
  LStreamer: IXRTLObjectStreamer;

  procedure WriteClassDescriptor(const Stream: TXRTLOutputStream; const Descriptor: IXRTLClassDescriptor);
  var
    DStream: TXRTLDataOutputStream;
  begin
    DStream:= nil;
    try
      DStream:= TXRTLDataOutputStream.Create(Stream, False);
      DStream.WriteUTF8String(LDescriptor.GetClassId);
    finally
      FreeAndNil(DStream);
    end;
  end;

begin
  LDescriptors:= nil;
  try
    LDescriptors:= TXRTLArrayStack.Create;
    LClass:= Obj.ClassType;
//  write class descriptors in descendant - ancestor order
    LStream:= nil;
    try
      LStream:= TXRTLBlockOutputStream.Create(Stream);
      while Assigned(LClass) do
      begin
        LDescriptor:= nil;
        if XRTLFindClassDescriptor(LClass, LDescriptor) then
        begin
          LDescriptors.Push(XRTLValue(LDescriptor));
          WriteClassDescriptor(LStream, LDescriptor);
        end;
        LClass:= LClass.ClassParent;
      end;
    finally
      FreeAndNil(LStream);
    end;
//  write object data in ancestor - descendant order
    while not LDescriptors.IsEmpty do
    begin
      OStream:= nil;
      try
        OStream:= TXRTLBlockOutputStream.Create(Stream, False);
        LDescriptor:= XRTLGetAsInterface(LDescriptors.Pop) as IXRTLClassDescriptor;
//  write property data first
        LProps:= nil;
        LStream:= nil;
        try
          LStream:= TXRTLBlockOutputStream.Create(OStream, False);
          LProps:= TXRTLArray.Create;
          LProperties:= LDescriptor.DefineProperties;
          LDescriptor.Introspector.GetValues(Obj, LProperties);
          LProperties.GetProperties(LProps);
          Iter:= LProps.AtBegin;
          while Iter.Compare(LProps.AtEnd) <> XRTLEqualsValue do
          begin
            WriteProperty(LStream, XRTLGetAsInterface(LProps.GetValue(Iter)) as IXRTLProperty);
          end;
        finally
          FreeAndNil(LProps);
          FreeAndNil(LStream);
          LProperties:= nil;
        end;
//  write custom data
        if XRTLFindStreamer(LDescriptor.GetClass, LStreamer) then
        begin
          LStream:= nil;
          try
            LStream:= TXRTLBlockOutputStream.Create(OStream, False);
            LStreamer.WriteObjectData(TXRTLBinaryObjectWriter.Create(LStream, Self), Obj);
          finally
            FreeAndNil(LStream);
          end;
        end;
      finally
        FreeAndNil(OStream);
      end;
    end;
  finally
    FreeAndNil(LDescriptors);
  end;
end;

procedure TXRTLBinarySerializer.WriteProperty(const Stream: TXRTLOutputStream;
  const AProperty: IXRTLProperty);
var
  DStream: TXRTLDataOutputStream;
begin
  DStream:= nil;
  try
    DStream:= TXRTLDataOutputStream.Create(TXRTLBlockOutputStream.Create(Stream, False));
    DStream.WriteUTF8String(AProperty.Name);
    WriteInterface(DStream, AProperty.Value, False); 
  finally
    FreeAndNil(DStream);
  end;
end;

end.

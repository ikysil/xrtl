unit xrtl_io_object_BinarySerializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Exception,
  xrtl_io_Stream,
  xrtl_io_object_Serializer;

type
  EXRTLBinarySerializerException = class(EXRTLSerializerException);

  TXRTLBinarySerializer = class(TInterfacedObject, IXRTLSerializer)
  private
  protected
    function   CheckAndWriteNilReference(const Stream: TXRTLOutputStream; const Obj): Boolean;
    function   WriteReference(const Stream: TXRTLOutputStream; const Obj: TObject;
                              const AllowShared: Boolean): Boolean;
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
  xrtl_util_Guard,
  xrtl_reflect_ClassDescriptor,
  xrtl_io_object_BinarySerializerClasses, xrtl_io_object_Reference;

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
    WriteObject(Stream, Ref, False);
  end
  else
  begin
    try
//  create unique instance reference
      Ref:= TXRTLInstanceReference.Create(Obj, AllowShared);
      WriteObject(Stream, Ref, False);
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
    WriteObject(Stream, TXRTLInstanceReference.Create(nil, False));
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
begin
  if CheckAndWriteNilReference(Stream, Obj) then
    Exit;
// TO DO: check for special cases - TXRTLClassDescriptor and TXRTLInstanceReference instances
end;

end.

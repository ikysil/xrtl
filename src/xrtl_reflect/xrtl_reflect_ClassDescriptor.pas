unit xrtl_reflect_ClassDescriptor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Value, xrtl_util_Exception,
  xrtl_reflect_PropertyList;

type
  EXRTLClassDescriptorException = class(EXRTLException);

  IXRTLClassDescriptor = interface;

  IXRTLIntrospector = interface
  ['{2BD0AC80-EC53-4252-A8A9-4F5325750A89}']
    procedure  DefineProperties(const Descriptor: IXRTLClassDescriptor; const Properties: IXRTLPropertyList);
  end;

  IXRTLClassDescriptor = interface
  ['{C255D459-F2C1-44FA-A47F-C37771143BF4}']
    function   GetClass: TClass;
    function   GetClassId: WideString;
    function   GetProperties: IXRTLPropertyList;
    function   GetIntrospector: IXRTLIntrospector;
    property   Properties: IXRTLPropertyList read GetProperties;
    property   Introspector: IXRTLIntrospector read GetIntrospector;
  end;

  TXRTLClassDescriptor = class(TInterfacedObject, IXRTLImplementationObjectProvider,
                               IXRTLClassDescriptor)
  private
    FClass: TClass;
    FClassId: WideString;
    FProperties: IXRTLPropertyList;
    FIntrospector: IXRTLIntrospector;
  protected
  public
    constructor Create(const AClass: TClass; const AClassId: WideString;
                       const AIntrospector: IXRTLIntrospector);
    destructor Destroy; override;
    function   GetImplementationObject: TObject;
    function   GetClass: TClass;
    function   GetClassId: WideString;
    function   GetProperties: IXRTLPropertyList;
    function   GetIntrospector: IXRTLIntrospector;
  end;

procedure XRTLRegisterClassDescriptor(const Descriptor: IXRTLClassDescriptor);
function  XRTLFindClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean; overload;
function  XRTLFindClassDescriptor(const ClassId: WideString; var Descriptor: IXRTLClassDescriptor): Boolean; overload;
function  XRTLFindParentClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean;
function  XRTLGetClassDescriptor(const Clazz: TClass): IXRTLClassDescriptor; overload;
function  XRTLGetClassDescriptor(const ClassId: WideString): IXRTLClassDescriptor; overload;

implementation

uses
  xrtl_util_Compare, xrtl_util_Container, xrtl_util_Array, xrtl_util_Lock,
  xrtl_reflect_ResourceStrings;

var
  FDescriptorLock: IXRTLExclusiveLock = nil;
  FDescriptorList: TXRTLSequentialContainer = nil;

procedure XRTLRegisterClassDescriptor(const Descriptor: IXRTLClassDescriptor);
var
  LDescriptor: IXRTLClassDescriptor;
  AutoLock: IInterface;
begin
  AutoLock:= XRTLAcquireExclusiveLock(FDescriptorLock);
  if XRTLFindClassDescriptor(Descriptor.GetClass, LDescriptor) then
    raise EXRTLClassDescriptorException.CreateFmt(SXRTLDescriptorForClassNameRegistered,
                                                  [Descriptor.GetClass.ClassName]);
  if XRTLFindClassDescriptor(Descriptor.GetClassId, LDescriptor) then
    raise EXRTLClassDescriptorException.CreateFmt(SXRTLDescriptorForClassIdRegistered,
                                                  [Descriptor.GetClassId]);
  FDescriptorList.Insert(XRTLValue(Descriptor));
end;

function XRTLFindClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean;
var
  Iter: IXRTLIterator;
  AutoLock: IInterface;
begin
  AutoLock:= XRTLAcquireExclusiveLock(FDescriptorLock);
  Result:= False;
  Iter:= FDescriptorList.AtBegin;
  while Iter.Compare(FDescriptorList.AtBegin) <> XRTLEqualsValue do
  begin
    Descriptor:= XRTLGetAsInterface(FDescriptorList.GetValue(Iter)) as IXRTLClassDescriptor;
    if Descriptor.GetClass = Clazz then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Descriptor:= nil;
end;

function XRTLFindClassDescriptor(const ClassId: WideString; var Descriptor: IXRTLClassDescriptor): Boolean;
var
  Iter: IXRTLIterator;
  AutoLock: IInterface;
begin
  AutoLock:= XRTLAcquireExclusiveLock(FDescriptorLock);
  Result:= False;
  Iter:= FDescriptorList.AtBegin;
  while Iter.Compare(FDescriptorList.AtBegin) <> XRTLEqualsValue do
  begin
    Descriptor:= XRTLGetAsInterface(FDescriptorList.GetValue(Iter)) as IXRTLClassDescriptor;
    if WideCompareStr(Descriptor.GetClassId, ClassId) = 0 then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Descriptor:= nil;
end;

function XRTLFindParentClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean;
var
  LClass: TClass;
begin
  Result:= False;
  Descriptor:= nil;
  LClass:= Clazz.ClassParent;
  while not Result and Assigned(LClass) do
  begin
    Result:= XRTLFindClassDescriptor(LClass, Descriptor);
    LClass:= LClass.ClassParent;
  end;
end;

function XRTLGetClassDescriptor(const Clazz: TClass): IXRTLClassDescriptor; overload;
begin
  if not XRTLFindClassDescriptor(Clazz, Result) then
    raise EXRTLClassDescriptorException.CreateFmt(SXRTLNoDescriptorForClassNameRegistered, [Clazz.ClassName]);
end;

function XRTLGetClassDescriptor(const ClassId: WideString): IXRTLClassDescriptor; overload;
begin
  if not XRTLFindClassDescriptor(ClassId, Result) then
    raise EXRTLClassDescriptorException.CreateFmt(SXRTLNoDescriptorForClassIdRegistered, [ClassId]);
end;

{ TXRTLClassDescriptor }

constructor TXRTLClassDescriptor.Create(const AClass: TClass; const AClassId: WideString;
  const AIntrospector: IXRTLIntrospector);
begin
  inherited Create;
  FClass:= AClass;
  FClassId:= AClassId;
  FProperties:= TXRTLPropertyList.Create;
  FIntrospector:= AIntrospector;
end;

destructor TXRTLClassDescriptor.Destroy;
begin
  inherited;
end;

function TXRTLClassDescriptor.GetImplementationObject: TObject;
begin
  Result:= Self;
end;

function TXRTLClassDescriptor.GetClass: TClass;
begin
  Result:= FClass;
end;

function TXRTLClassDescriptor.GetClassId: WideString;
begin
  Result:= FClassId;
end;

function TXRTLClassDescriptor.GetProperties: IXRTLPropertyList;
begin
  if FProperties.IsEmpty then
    FIntrospector.DefineProperties(Self, FProperties);
  Result:= FProperties;
end;

function TXRTLClassDescriptor.GetIntrospector: IXRTLIntrospector;
begin
  Result:= FIntrospector;
end;

initialization
begin
  FDescriptorLock:= XRTLCreateExclusiveLock;
  FDescriptorList:= TXRTLArray.Create;
end;

finalization
begin
  FreeAndNil(FDescriptorList);
end;

end.

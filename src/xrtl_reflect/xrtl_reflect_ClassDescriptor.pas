unit xrtl_reflect_ClassDescriptor;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Value, xrtl_util_Exception, xrtl_util_Compat,
  xrtl_util_Compare, xrtl_util_Container,
  xrtl_reflect_Introspector, xrtl_reflect_Factory;

const
  XRTLClassDescriptorId = 'xrtl::class';

type
  EXRTLClassDescriptorException = class(EXRTLException);

  IXRTLClassDescriptor = interface
  ['{C255D459-F2C1-44FA-A47F-C37771143BF4}']
    function   GetClass: TClass;
    function   GetClassId: WideString;
    function   DefineProperties: IXRTLPropertyList;
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
    function   CreateInstance: TObject;
  end;

  TXRTLClassDescriptor = class(TInterfacedObject, IXRTLImplementationObjectProvider,
                               IXRTLClassDescriptor)
  private
    FClass: TClass;
    FClassId: WideString;
    FIntrospector: IXRTLIntrospector;
    FFactory: IXRTLFactory;
    procedure  CheckFactory;
    procedure  CheckIntrospector;
  protected
    procedure  DoDefineProperties(const Properties: IXRTLPropertyList); virtual;
    procedure  DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual;
    procedure  DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual;
    function   DoCreateInstance: TObject; virtual;
  public
    constructor Create(const AClassId: WideString); overload;
    constructor Create(const AClassId: WideString; const AClass: TClass); overload;
    constructor Create(const AClassId: WideString; const AClass: TClass;
                       const AIntrospector: IXRTLIntrospector); overload;
    constructor Create(const AClassId: WideString; const AClass: TClass;
                       const AFactory: IXRTLFactory;
                       const AIntrospector: IXRTLIntrospector = nil); overload;
    destructor Destroy; override;
    function   GetImplementationObject: TObject;
    function   GetClass: TClass;
    function   GetClassId: WideString;
    function   DefineProperties: IXRTLPropertyList;
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
    function   CreateInstance: TObject;
  end;

  TXRTLClassDescriptorFactory = class(TInterfacedObject, IXRTLFactory)
  public
    function   CreateInstance: TObject;
  end;
  
procedure XRTLRegisterClassDescriptor(const Descriptor: IXRTLClassDescriptor);
function  XRTLFindClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean; overload;
function  XRTLFindClassDescriptor(const ClassId: WideString; var Descriptor: IXRTLClassDescriptor): Boolean; overload;
function  XRTLFindParentClassDescriptor(const Clazz: TClass; var Descriptor: IXRTLClassDescriptor): Boolean;
function  XRTLGetClassDescriptor(const Clazz: TClass): IXRTLClassDescriptor; overload;
function  XRTLGetClassDescriptor(const ClassId: WideString): IXRTLClassDescriptor; overload;
// Returns hierarchy class descriptors in descendant - ancestor order
function  XRTLFindHierarchyClassDescriptors(const Clazz: TClass): TXRTLSequentialContainer;

implementation

uses
  xrtl_util_Array, xrtl_util_Lock,
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
  if XRTLFindClassDescriptor(Descriptor.GetClass, LDescriptor)
     and (LDescriptor <> Descriptor) then
    raise EXRTLClassDescriptorException.CreateFmt(SXRTLDescriptorForClassNameRegistered,
                                                  [Descriptor.GetClass.ClassName]);
  if XRTLFindClassDescriptor(Descriptor.GetClassId, LDescriptor)
     and (LDescriptor <> Descriptor) then
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
  while Iter.Compare(FDescriptorList.AtEnd) <> XRTLEqualsValue do
  begin
    Descriptor:= XRTLGetAsInterface(FDescriptorList.GetValue(Iter)) as IXRTLClassDescriptor;
    if Descriptor.GetClass = Clazz then
    begin
      Result:= True;
      Exit;
    end;
    Iter.Next;
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
  while Iter.Compare(FDescriptorList.AtEnd) <> XRTLEqualsValue do
  begin
    Descriptor:= XRTLGetAsInterface(FDescriptorList.GetValue(Iter)) as IXRTLClassDescriptor;
    if WideCompareStr(Descriptor.GetClassId, ClassId) = 0 then
    begin
      Result:= True;
      Exit;
    end;
    Iter.Next;
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

function XRTLFindHierarchyClassDescriptors(const Clazz: TClass): TXRTLSequentialContainer;
var
  LClass: TClass;
  LDescriptor: IXRTLClassDescriptor;
begin
  Result:= TXRTLArray.Create;
  LClass:= Clazz;
  while Assigned(LClass) do
  begin
    LDescriptor:= nil;
    if XRTLFindClassDescriptor(LClass, LDescriptor) then
    begin
      Result.Insert(XRTLValue(LDescriptor), Result.AtEnd);
    end;
    LClass:= LClass.ClassParent;
  end;
end;

{ TXRTLClassDescriptor }

constructor TXRTLClassDescriptor.Create(const AClassId: WideString);
begin
  inherited Create;
  FClass:= nil;
  FClassId:= AClassId;
  FIntrospector:= nil;
  FFactory:= nil;
end;

constructor TXRTLClassDescriptor.Create(const AClassId: WideString; const AClass: TClass);
begin
  inherited Create;
  FClassId:= AClassId;
  FClass:= AClass;
  FFactory:= nil;
  FIntrospector:= nil;
end;

constructor TXRTLClassDescriptor.Create(const AClassId: WideString;
  const AClass: TClass; const AIntrospector: IXRTLIntrospector);
begin
  inherited Create;
  FClassId:= AClassId;
  FClass:= AClass;
  FFactory:= nil;
  FIntrospector:= AIntrospector;
end;

constructor TXRTLClassDescriptor.Create(const AClassId: WideString; const AClass: TClass;
  const AFactory: IXRTLFactory; const AIntrospector: IXRTLIntrospector = nil);
begin
  inherited Create;
  FClassId:= AClassId;
  FClass:= AClass;
  FFactory:= AFactory;
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

procedure TXRTLClassDescriptor.CheckIntrospector;
begin
  if not Assigned(FIntrospector) and Assigned(FClass) then
    XRTLIntrospectorRegistry.Find(FClass, FIntrospector);
end;

function TXRTLClassDescriptor.DefineProperties: IXRTLPropertyList;
begin
  CheckIntrospector;
  Result:= TXRTLPropertyList.Create;
  if Assigned(FIntrospector) then
    FIntrospector.DefineProperties(Self.GetClass, Result)
  else
    DoDefineProperties(Result);
end;

procedure TXRTLClassDescriptor.GetValues(const Obj: TObject;
  const Properties: IXRTLPropertyList);
begin
  CheckIntrospector;
  if Assigned(FIntrospector) then
    FIntrospector.GetValues(Obj, Properties)
  else
    DoGetValues(Obj, Properties);
end;

procedure TXRTLClassDescriptor.SetValues(const Obj: TObject;
  const Properties: IXRTLPropertyList);
begin
  CheckIntrospector;
  if Assigned(FIntrospector) then
    FIntrospector.SetValues(Obj, Properties)
  else
    DoSetValues(Obj, Properties);
end;

procedure TXRTLClassDescriptor.CheckFactory;
begin
  if not Assigned(FFactory) and Assigned(FClass) then
    XRTLFactoryRegistry.Find(FClass, FFactory);
end;

function TXRTLClassDescriptor.CreateInstance: TObject;
begin
  CheckFactory;
  if Assigned(FFactory) then
    Result:= FFactory.CreateInstance
  else
    Result:= DoCreateInstance;
end;

procedure TXRTLClassDescriptor.DoDefineProperties(const Properties: IXRTLPropertyList);
begin
end;

procedure TXRTLClassDescriptor.DoGetValues(const Obj: TObject;
  const Properties: IXRTLPropertyList);
begin
end;

procedure TXRTLClassDescriptor.DoSetValues(const Obj: TObject;
  const Properties: IXRTLPropertyList);
begin
end;

function TXRTLClassDescriptor.DoCreateInstance: TObject;
begin
  Result:= nil;
end;

{ TXRTLClassDescriptorFactory }

function TXRTLClassDescriptorFactory.CreateInstance: TObject;
begin
  Result:= TXRTLClassDescriptor.Create;
end;

initialization
begin
  FDescriptorLock:= XRTLCreateExclusiveLock;
  FDescriptorList:= TXRTLArray.Create;
  XRTLRegisterClassDescriptor(
    TXRTLClassDescriptor.Create(XRTLClassDescriptorId, TXRTLClassDescriptor, 
                                TXRTLClassDescriptorFactory.Create));
end;

finalization
begin
  FreeAndNil(FDescriptorList);
end;

end.

unit xrtl_reflect_ClassHelper;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Map, xrtl_util_Value, xrtl_util_Exception;

type
  EXRTLClassHelperRegistryException = class(EXRTLException);

  IXRTLClassHelper = interface
  ['{33F88543-E2F6-47DD-BF63-F3AC0E2D7404}']
  end;

  TXRTLClassHelperRegistry = class
  private
    FHelpers: TXRTLSynchronizedMap;
    function   GetHelperCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  Register(const Clazz: TClass; const AHelper: IXRTLClassHelper);
    procedure  Unregister(const Clazz: TClass);
    function   Find(const Clazz: TClass; out AHelper): Boolean;
    function   Get(const Clazz: TClass): IXRTLClassHelper;
    property   Helpers[const Clazz: TClass]: IXRTLClassHelper read Get;
    property   HelperCount: Integer read GetHelperCount;
  end;

implementation

uses
  xrtl_reflect_ResourceStrings;

{ TXRTLClassHelperRegistry }

constructor TXRTLClassHelperRegistry.Create;
begin
  inherited Create;
  FHelpers:= TXRTLSynchronizedMap.Create(TXRTLArrayMap.Create, True);
end;

destructor TXRTLClassHelperRegistry.Destroy;
begin
  FreeAndNil(FHelpers);
  inherited;
end;

procedure TXRTLClassHelperRegistry.Register(const Clazz: TClass;
  const AHelper: IXRTLClassHelper);
var
  LHelper: IXRTLClassHelper;
begin
  try
    FHelpers.BeginRead;
    if Find(Clazz, LHelper) then
      raise EXRTLClassHelperRegistryException.CreateFmt(SXRTLHelperForClassNameRegistered,
                                                        [Clazz.ClassName]);
    FHelpers.SetValue(XRTLValue(Clazz), XRTLValue(AHelper));
  finally
    FHelpers.EndRead;
  end;
end;

procedure TXRTLClassHelperRegistry.Unregister(const Clazz: TClass);
begin
  FHelpers.Remove(XRTLValue(Clazz));
end;

function TXRTLClassHelperRegistry.Find(const Clazz: TClass; out AHelper): Boolean;
var
  Value: IXRTLValue;
begin
  IInterface(AHelper):= nil;
  Value:= FHelpers.GetValue(XRTLValue(Clazz));
  Result:= Assigned(Value);
  if Result then
    IInterface(AHelper):= XRTLGetAsInterface(Value) as IXRTLClassHelper;
end;

function TXRTLClassHelperRegistry.Get(const Clazz: TClass): IXRTLClassHelper;
begin
  if not Find(Clazz, Result) then
    raise EXRTLClassHelperRegistryException.CreateFmt(SXRTLNoHelperForClassNameRegistered, [Clazz.ClassName]);
end;

function TXRTLClassHelperRegistry.GetHelperCount: Integer;
begin
  try
    FHelpers.BeginRead;
    Result:= (FHelpers.CoreMap as TXRTLArrayMap).GetSize;
  finally
    FHelpers.EndRead;
  end;
end;

end.

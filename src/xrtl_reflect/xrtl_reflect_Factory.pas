unit xrtl_reflect_Factory;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Classes,
  xrtl_reflect_ClassHelper;

type
  IXRTLFactory = interface(IXRTLClassHelper)
  ['{2BD0AC81-EC53-4252-A8A9-4F5325750A89}']
    function   CreateInstance: TObject;
  end;

  TXRTLObjectFactory = class(TInterfacedObject, IXRTLFactory)
  private
    FClazz: TClass;
  public
    constructor Create(const AClazz: TClass);
    function   CreateInstance: TObject;
    property   Clazz: TClass read FClazz;
  end;

  TXRTLComponentFactory = class(TXRTLObjectFactory)
  public
    constructor Create(const AClazz: TClass);
    function   CreateInstance: TObject;
  end;

  TXRTLCreateInstanceProc = function: TObject;

  TXRTLDelegatingFactory = class(TInterfacedObject, IXRTLFactory)
  private
    FCreateInstanceProc: TXRTLCreateInstanceProc;
  public
    constructor Create(const ACreateInstanceProc: TXRTLCreateInstanceProc);
    function   CreateInstance: TObject;
  end;

function  XRTLFactoryRegistry: TXRTLClassHelperRegistry;

implementation

uses
  xrtl_util_Exception;

var
  FFactoryRegistry: TXRTLClassHelperRegistry;

function XRTLFactoryRegistry: TXRTLClassHelperRegistry;
begin
  Result:= FFactoryRegistry;
end;

{ TXRTLObjectFactory }

constructor TXRTLObjectFactory.Create(const AClazz: TClass);
begin
  inherited Create;
  FClazz:= AClazz;
end;

function TXRTLObjectFactory.CreateInstance: TObject;
begin
  Result:= FClazz.NewInstance;
end;

{ TXRTLComponentFactory }

constructor TXRTLComponentFactory.Create(const AClazz: TClass);
begin
  inherited Create(AClazz);
end;

function TXRTLComponentFactory.CreateInstance: TObject;
begin
  Result:= TComponentClass(Clazz).Create(nil);
end;

{ TXRTLDelegatingFactory }

constructor TXRTLDelegatingFactory.Create(const ACreateInstanceProc: TXRTLCreateInstanceProc);
begin
  inherited Create;
  FCreateInstanceProc:= ACreateInstanceProc;
end;

function TXRTLDelegatingFactory.CreateInstance: TObject;
begin
  Result:= nil;
  if Assigned(FCreateInstanceProc) then
    Result:= FCreateInstanceProc
  else
    XRTLNotImplemented;
end;

initialization
begin
  FFactoryRegistry:= TXRTLClassHelperRegistry.Create;
end;

finalization
begin
  FreeAndNil(FFactoryRegistry);
end;

end.

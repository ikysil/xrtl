unit xrtl_reflect_Factory;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Classes,
  xrtl_reflect_ClassDescriptor;

type
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
  
implementation

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

end.

unit xrtl_reflect_Factory;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Classes;

type
  IXRTLFactory = interface
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

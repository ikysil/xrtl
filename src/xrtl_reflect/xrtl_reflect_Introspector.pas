unit xrtl_reflect_Introspector;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Exception, xrtl_util_Container,
  xrtl_util_Array, xrtl_util_Type, xrtl_util_Value;

type
  IXRTLProperty = interface
  ['{C255D454-F2C1-44FA-A47F-C37771143BF4}']
    function   GetName: WideString;
    function   GetValue: IXRTLValue;
    procedure  SetValue(const AValue: IXRTLValue);
    property   Name: WideString read GetName;
    property   Value: IXRTLValue read GetValue write SetValue;
  end;

  EXRTLPropertyListException = class(EXRTLException);

  IXRTLPropertyList = interface
  ['{9704797E-0FB9-41B7-8EAD-07B8BD82B69A}']
    procedure  Add(const AProperty: IXRTLProperty);
    function   GetByName(const APropertyName: WideString): IXRTLProperty;
    procedure  Remove(const AProperty: IXRTLProperty); overload;
    procedure  Remove(const APropertyName: WideString); overload;
    procedure  Clear;
    procedure  SetProperties(const Container: TXRTLSequentialContainer);
    procedure  GetProperties(const Container: TXRTLSequentialContainer);
    function   IsEmpty: Boolean;
  end;

  IXRTLIntrospector = interface
  ['{2BD0AC80-EC53-4252-A8A9-4F5325750A89}']
    procedure  DefineProperties(const Clazz: TClass; const Properties: IXRTLPropertyList);
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
  end;

  TXRTLProperty = class(TInterfacedObject, IXRTLImplementationObjectProvider,
                        IXRTLProperty)
  private
    FName: WideString;
    FValue: IXRTLValue;
  public
    constructor Create(const AName: WideString; const AValue: IXRTLValue = nil);
    function   GetImplementationObject: TObject;
    function   GetName: WideString;
    function   GetValue: IXRTLValue;
    procedure  SetValue(const AValue: IXRTLValue);
  end;

  TXRTLPropertyList = class(TInterfacedObject, IXRTLPropertyList)
  private
    FProperties: TXRTLArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  Add(const AProperty: IXRTLProperty);
    function   GetByName(const APropertyName: WideString): IXRTLProperty;
    procedure  Remove(const AProperty: IXRTLProperty); overload;
    procedure  Remove(const APropertyName: WideString); overload;
    procedure  Clear;
    procedure  SetProperties(const Container: TXRTLSequentialContainer);
    procedure  GetProperties(const Container: TXRTLSequentialContainer);
    function   IsEmpty: Boolean;
  end;

  TXRTLIntrospector = class(TInterfacedObject, IXRTLIntrospector)
  public
    procedure  DefineProperties(const Clazz: TClass; const Properties: IXRTLPropertyList); virtual; abstract;
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual; abstract;
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual; abstract;
  end;

implementation

uses
  xrtl_util_Algorithm, xrtl_util_Compare;

{ TXRTLProperty }

constructor TXRTLProperty.Create(const AName: WideString; const AValue: IXRTLValue = nil);
begin
  inherited Create;
  FName:= AName;
  FValue:= AValue;
end;

function TXRTLProperty.GetImplementationObject: TObject;
begin
  Result:= Self;
end;

function TXRTLProperty.GetName: WideString;
begin
  Result:= FName;
end;

function TXRTLProperty.GetValue: IXRTLValue;
begin
  Result:= FValue;
end;

procedure TXRTLProperty.SetValue(const AValue: IXRTLValue);
begin
  FValue:= AValue;
end;

{ TXRTLPropertyList }

constructor TXRTLPropertyList.Create;
begin
  inherited Create;
  FProperties:= TXRTLArray.Create;
end;

destructor TXRTLPropertyList.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

procedure TXRTLPropertyList.Add(const AProperty: IXRTLProperty);
begin
  Remove(AProperty.Name);
  FProperties.Add(XRTLValue(AProperty));
end;

function TXRTLPropertyList.GetByName(const APropertyName: WideString): IXRTLProperty;
var
  Iter: IXRTLIterator;
  LProperty: IXRTLProperty;
begin
  Result:= nil;
  Iter:= FProperties.AtBegin;
  while Iter.Compare(FProperties.AtEnd) <> XRTLEqualsValue do
  begin
    LProperty:= XRTLGetAsInterface(FProperties.GetValue(Iter)) as IXRTLProperty;
    if WideCompareStr(LProperty.Name, APropertyName) = 0 then
    begin
      Result:= LProperty;
      Exit;
    end;
    Iter.Next;
  end;
end;

procedure TXRTLPropertyList.Remove(const AProperty: IXRTLProperty);
begin
  FProperties.Remove(XRTLValue(AProperty));
end;

procedure TXRTLPropertyList.Remove(const APropertyName: WideString);
begin
  Remove(GetByName(APropertyName));
end;

procedure TXRTLPropertyList.Clear;
begin
  FProperties.Clear;
end;

procedure TXRTLPropertyList.SetProperties(const Container: TXRTLSequentialContainer);
begin
  FProperties.Clear;
  XRTLCopy(Container, FProperties);
end;

procedure TXRTLPropertyList.GetProperties(const Container: TXRTLSequentialContainer);
begin
  Container.Clear;
  XRTLCopy(FProperties, Container);
end;

function TXRTLPropertyList.IsEmpty: Boolean;
begin
  Result:= FProperties.IsEmpty;
end;

end.

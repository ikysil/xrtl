unit xrtl_reflect_PropertyList;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Value, xrtl_util_Exception, xrtl_util_Container,
  xrtl_util_Array,
  xrtl_reflect_Property;

type
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

implementation

uses
  xrtl_util_Algorithm, xrtl_util_Compare;

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

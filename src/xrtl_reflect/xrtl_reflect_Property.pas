unit xrtl_reflect_Property;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_util_Type, xrtl_util_Value;

type
  IXRTLProperty = interface
  ['{C255D454-F2C1-44FA-A47F-C37771143BF4}']
    function   GetName: WideString;
    function   GetValue: IXRTLValue;
    procedure  SetValue(const AValue: IXRTLValue);
    property   Name: WideString read GetName;
    property   Value: IXRTLValue read GetValue write SetValue;
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

implementation

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

end.

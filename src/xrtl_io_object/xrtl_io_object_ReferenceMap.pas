unit xrtl_io_object_ReferenceMap;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Array, xrtl_util_Map,
  xrtl_util_Container, xrtl_util_Value,
  xrtl_reflect_ClassDescriptor, xrtl_reflect_PropertyList, xrtl_reflect_Property,
  xrtl_io_object_Serializer, xrtl_io_object_Reference;

type
  TXRTLReferenceMap = class
  private
    FRef2Obj: TXRTLMap;
    FObj2Ref: TXRTLMap;
  public
    constructor Create;
    destructor Destroy; override;
    function   RegisterReference(const Ref: TXRTLInstanceReference; const Obj: TObject): Boolean;
    function   GetObject(const Ref: TXRTLInstanceReference): TObject;
    function   GetReference(const Obj: TObject): TXRTLInstanceReference;
  end;

implementation

{ TXRTLReferenceMap }

constructor TXRTLReferenceMap.Create;
begin
  inherited Create;
  FRef2Obj:= TXRTLArrayMap.Create;
  FObj2Ref:= TXRTLArrayMap.Create;
end;

destructor TXRTLReferenceMap.Destroy;
begin
  FreeAndNil(FObj2Ref);
  FreeAndNil(FRef2Obj);
  inherited;
end;

function TXRTLReferenceMap.RegisterReference(const Ref: TXRTLInstanceReference;
  const Obj: TObject): Boolean;
var
  ObjValue: IXRTLValue;
begin
  Result:= False;
  if Ref = Obj then Exit;
  if not Assigned(Ref) or not Assigned(Obj) then Exit;
  if WideCompareStr(Ref.ReferenceId, '') = 0 then Exit;
  ObjValue:= XRTLValue(Obj, False);
  FRef2Obj.SetValue(XRTLValue(Ref.ReferenceId), ObjValue);
  FObj2Ref.SetValue(ObjValue, XRTLValue(Ref, True));
  Result:= True;
end;

function TXRTLReferenceMap.GetObject(const Ref: TXRTLInstanceReference): TObject;
var
  ObjValue: IXRTLValue;
begin
  Result:= nil;
  if not Assigned(Ref) then Exit;
  if WideCompareStr(Ref.ReferenceId, '') = 0 then Exit;
  ObjValue:= FRef2Obj.GetValue(XRTLValue(Ref.ReferenceId));
  if Assigned(ObjValue) then
    Result:= XRTLGetAsObject(ObjValue, False);
end;

function TXRTLReferenceMap.GetReference(const Obj: TObject): TXRTLInstanceReference;
var
  RefValue: IXRTLValue;
begin
  Result:= nil;
  if not Assigned(Obj) then Exit;
  RefValue:= FObj2Ref.GetValue(XRTLValue(Obj, False));
  if Assigned(RefValue) then
    Result:= XRTLGetAsObject(RefValue, False) as TXRTLInstanceReference;
end;

end.

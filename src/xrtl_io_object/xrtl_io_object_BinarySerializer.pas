unit xrtl_io_object_BinarySerializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Exception,
  xrtl_io_object_Serializer;

type
  TXRTLBinarySerializer = class
  protected
    function   CheckAndWriteNil(const Obj): Boolean;
    procedure  WriteNilReference; virtual; abstract;
    procedure  WriteInterfaceDataBegin; virtual; abstract;
    procedure  WriteInterfaceDataEnd; virtual; abstract;
    procedure  WriteClassDataBegin; virtual; abstract;
    procedure  WriteClassDataEnd; virtual; abstract;
    procedure  WriteObjectDataBegin; virtual; abstract;
    procedure  WriteObjectDataEnd; virtual; abstract;
    procedure  WriteObjectInternal(const Obj: TObject; const Shared: Boolean = True); virtual; abstract;
  public
    procedure  WriteInterface(const Obj: IInterface; const Shared: Boolean = True);
    procedure  WriteClass(const Obj: TClass; const Shared: Boolean = True);
    procedure  WriteObject(const Obj: TObject; const Shared: Boolean = True);
  end;

implementation

uses
  xrtl_util_Guard,
  xrtl_reflect_ClassDescriptor;

{ TXRTLBinarySerializer }

function TXRTLBinarySerializer.CheckAndWriteNil(const Obj): Boolean;
begin
  Result:= not Assigned(Pointer(Obj));
  if Result then
    WriteNilReference;
end;

procedure TXRTLBinarySerializer.WriteInterface(const Obj: IInterface; const Shared: Boolean = True);
var
  LObj: IXRTLImplementationObjectProvider;
begin
  if CheckAndWriteNil(Obj) then
    Exit;
  if not Supports(Obj, IXRTLImplementationObjectProvider, LObj) then
    raise EXRTLSerializerException.CreateFmt('Can''t serialize interface reference 0x%.8x', [Cardinal(Obj)]);
  WriteInterfaceDataBegin;
  WriteObjectInternal(LObj.GetImplementationObject, Shared);
  WriteInterfaceDataEnd;
end;

procedure TXRTLBinarySerializer.WriteClass(const Obj: TClass; const Shared: Boolean = True);
var
  LObj: TXRTLClassDescriptor;
  LGuard: IXRTLGuard;
begin
  if CheckAndWriteNil(Obj) then
    Exit;
//  LGuard:= XRTLGuardObject(TXRTLClassDescriptor.Create{XRTLGetClassDescriptor(Obj)}, LObj);
  if not Assigned(LObj) then
    raise EXRTLSerializerException.CreateFmt('Can''t serialize class %s', [Obj.ClassName]);
  WriteClassDataBegin;
  WriteObjectInternal(LObj, Shared);
  WriteClassDataEnd;
end;

procedure TXRTLBinarySerializer.WriteObject(const Obj: TObject; const Shared: Boolean = True);
begin
  if CheckAndWriteNil(Obj) then
    Exit;
  WriteObjectDataBegin;
  WriteObjectInternal(Obj, Shared);
  WriteObjectDataEnd;
end;

end.

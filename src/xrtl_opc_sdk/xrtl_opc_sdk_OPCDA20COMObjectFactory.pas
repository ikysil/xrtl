unit xrtl_opc_sdk_OPCDA20COMObjectFactory;

{$INCLUDE xrtl.inc}

interface

uses
  COMObj;

type
  TXRTLOPCDA20COMObjectFactory = class(TCOMObjectFactory)
  public
    constructor Create(ComServer: TComServerObject; ComClass: TCOMClass;
      const ClassID: TGUID; const ClassName, Description: string;
      Instancing: TClassInstancing; ThreadingModel: TThreadingModel = tmApartment);
    procedure  UpdateRegistry(Register: Boolean); override;
  end;

implementation

uses
  xrtl_util_COMCat,
  xrtl_opc_DA,
  xrtl_opc_sdk_DA, xrtl_opc_sdk_OPCDA20Group,
  xrtl_opc_sdk_ResourceStrings;

{ TXRTLOPCDA20COMObjectFactory }

constructor TXRTLOPCDA20COMObjectFactory.Create(
  ComServer: TComServerObject; ComClass: TCOMClass;
  const ClassID: TGUID; const ClassName, Description: string;
  Instancing: TClassInstancing;
  ThreadingModel: TThreadingModel = tmApartment);
begin
  inherited;
  TCOMObjectFactory.Create(COMServer, TXRTLOPCDA20Group, CLASS_OPCDA20Group,
                           SOPCDA20Group_ClassName, SOPCDA20Group_Description,
                           ciInternal, tmFree);
end;

procedure TXRTLOPCDA20COMObjectFactory.UpdateRegistry(Register: Boolean);
var
  ProgID: string;
begin
  ProgID:= GetProgID;
  if Register then
  begin
    inherited;
    if ProgID <> '' then
    begin
      CreateRegKey(ProgID + '\OPC', '', '');
    end;
    XRTLCreateComponentCategory(CATID_OPCDAServer20, OPC_CATEGORY_DESCRIPTION_DA20, $409);
    XRTLRegisterCLSIDInCategory(ClassID, CATID_OPCDAServer20);
  end
  else
  begin
    if ProgID <> '' then
    begin
      DeleteRegKey(ProgID + '\OPC');
    end;
    XRTLUnRegisterCLSIDInCategory(ClassID, CATID_OPCDAServer20);
    if XRTLIsCategoryEmpty(CATID_OPCDAServer20) then
      XRTLRemoveComponentCategory(CATID_OPCDAServer20, OPC_CATEGORY_DESCRIPTION_DA20, $409);
    inherited;
  end;
end;

end.

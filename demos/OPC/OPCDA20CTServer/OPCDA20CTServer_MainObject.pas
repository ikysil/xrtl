unit OPCDA20CTServer_MainObject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, COMObj, StdVcl, Variants, SysUtils,
  xrtl_util_StrUtils,
  xrtl_opc_DA, xrtl_opc_Common,
  xrtl_opc_sdk_OPCDA20Server, xrtl_opc_sdk_OPCDA20COMObjectFactory,
  xrtl_opc_sdk_OPCDANameSpace, xrtl_opc_sdk_OPCDANameSpaceItem,
  xrtl_opc_sdk_OPCDA20Group, xrtl_opc_sdk_OPCDADataSourceCache, xrtl_opc_sdk_OPCDADataSource,
  xrtl_opc_sdk_DA,
  OPCDA20CTServer_Device;

const
  CLASS_OPCDA20CTServer: TGUID = '{D7DFE05F-020D-4553-BD2F-B475C3B9AA17}';

type
  TOPCDA20CTServer = class(TXRTLOPCDA20Server)
  private
  public
  end;

implementation

uses ComServ;

const
  ItemCount = 10;
  Root_Arrays: WideString = 'Arrays';
  Root_RO: WideString = 'R/O';
  Root_WO: WideString = 'W/O';
  Root_RW: WideString = 'R/W';

procedure DefineRoots;
var
  Item: IXRTLOPCDANameSpaceItem;
begin
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_RO]), POLEStr(Root_RO), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_WO]), POLEStr(Root_WO), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_RW]), POLEStr(Root_RW), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays]), POLEStr(Root_Arrays), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_RO]), POLEStr(Root_RO), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_WO]), POLEStr(Root_WO), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_RW]), POLEStr(Root_RW), VT_EMPTY, Item);
end;

procedure DefineItem(vtDataType: DWORD; Name: WideString); overload;
var
  Item: IXRTLOPCDANameSpaceItem;
begin
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_RO, Name]), POLEStr(Name + ' (read only)'), vtDataType, Item);
  Item.SetAccessRights(OPC_READABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_WO, Name]), POLEStr(Name + ' (write only)'), vtDataType, Item);
  Item.SetAccessRights(OPC_WRITEABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_RW, Name]), POLEStr(Name), vtDataType, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_RO, Name]), POLEStr(Name + ' array' + ' (read only)'), vtDataType or VT_ARRAY, Item);
  Item.SetAccessRights(OPC_READABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_WO, Name]), POLEStr(Name + ' array' + ' (write only)'), vtDataType or VT_ARRAY, Item);
  Item.SetAccessRights(OPC_WRITEABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Root_Arrays, Root_RW, Name]), POLEStr(Name + ' array'), vtDataType or VT_ARRAY, Item);
end;

procedure InitializeNameSpace;
begin
  XRTLOPCDANameSpace:= TXRTLOPCDANameSpace.Create;
  XRTLOPCDANameSpace.SetOrganization(OPC_NS_HIERARCHIAL);
  DefineRoots;
  DefineItem(VT_BOOL, 'Boolean');
  DefineItem(VT_I1,   'ShortInt');
  DefineItem(VT_UI1,  'Byte');
  DefineItem(VT_I2,   'SmallInt');
  DefineItem(VT_UI2,  'Word');
  DefineItem(VT_I4,   'Integer');
  DefineItem(VT_UI4,  'DWord');
  DefineItem(VT_R4,   'Real4');
  DefineItem(VT_R8,   'Real8');
  DefineItem(VT_DATE, 'Date');
  DefineItem(VT_BSTR, 'String');
  DefineItem(VT_CY,   'Currency');
end;

procedure InitializeDataSource;
begin
  XRTLOPCDADataSourceCache:= TXRTLOPCDADataSourceCache.Create;
  XRTLOPCDADataSourceDevice:= TOPCDA20CTServerDevice.Create;
end;

initialization
begin
  COMServer.UIInteractive:= False;
  InitializeDataSource;
  InitializeNameSpace;
  XRTLOPCDAUpdateCache;
//  XRTLOPCDADataSourceDevice:= XRTLOPCDADataSourceCache;
  TXRTLOPCDA20COMObjectFactory.Create(COMServer, TOPCDA20CTServer, CLASS_OPCDA20CTServer,
                                      'OPCDA20CTServer', 'xrtl OPC DA 2.0 Compliance Test Server',
                                      ciMultiInstance, tmFree);
end;

end.

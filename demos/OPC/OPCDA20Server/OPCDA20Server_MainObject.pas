unit OPCDA20Server_MainObject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, COMObj, StdVcl, Variants, SysUtils,
  xrtl_util_StrUtils,
  xrtl_opc_DA, xrtl_opc_Common,
  xrtl_opc_sdk_OPCDA20Server, xrtl_opc_sdk_OPCDA20COMObjectFactory,
  xrtl_opc_sdk_OPCDANameSpace, xrtl_opc_sdk_OPCDANameSpaceItem,
  xrtl_opc_sdk_OPCDA20Group, xrtl_opc_sdk_OPCDADataSourceCache, xrtl_opc_sdk_OPCDADataSource,
  xrtl_opc_sdk_DA, OPCDA20Server_Device;

const
  CLASS_OPCDA20ServerDemo: TGUID = '{3EDA7514-F446-4927-8152-AA60E7CDB559}';

type
  TOPCDA20ServerDemo = class(TXRTLOPCDA20Server)
  private
  public
  end;

implementation

uses ComServ;

const
  ItemCount = 10;

procedure DefineItem(vtDataType: DWORD; Name: WideString); overload;
var
  Item: IXRTLOPCDANameSpaceItem;
begin
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name]), POLEStr(Name), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name, 'R']), POLEStr(Name + ' (read only)'), vtDataType, Item);
  Item.SetAccessRights(OPC_READABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name, 'W']), POLEStr(Name + ' (write only)'), vtDataType, Item);
  Item.SetAccessRights(OPC_WRITEABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name, 'R/W']), POLEStr(Name), vtDataType, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name + ' array']), POLEStr(Name + ' array'), VT_EMPTY, Item);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name + ' array', 'R']), POLEStr(Name + ' array' + ' (read only)'), vtDataType or VT_ARRAY, Item);
  Item.SetAccessRights(OPC_READABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name + ' array', 'W']), POLEStr(Name + ' array' + ' (write only)'), vtDataType or VT_ARRAY, Item);
  Item.SetAccessRights(OPC_WRITEABLE);
  XRTLOPCDANameSpace.AddItem(VarArrayOf([Name + ' array', 'R/W']), POLEStr(Name + ' array'), vtDataType or VT_ARRAY, Item);
end;

procedure DefineItem(vtDataType: DWORD; Name: WideString; Count: Integer); overload;
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    DefineItem(vtDataType, WideFormat('%s_%d', [Name, I]));
end;

procedure InitializeNameSpace;
begin
  XRTLOPCDANameSpace:= TXRTLOPCDANameSpace.Create;
  XRTLOPCDANameSpace.SetOrganization(OPC_NS_HIERARCHIAL);
  DefineItem(VT_BOOL, 'Boolean',  ItemCount);
  DefineItem(VT_I1,   'ShortInt', ItemCount);
  DefineItem(VT_UI1,  'Byte',     ItemCount);
  DefineItem(VT_I2,   'SmallInt', ItemCount);
  DefineItem(VT_UI2,  'Word',     ItemCount);
  DefineItem(VT_I4,   'Integer',  ItemCount);
  DefineItem(VT_UI4,  'DWord',    ItemCount);
  DefineItem(VT_R4,   'Real4',    ItemCount);
  DefineItem(VT_R8,   'Real8',    ItemCount);
  DefineItem(VT_DATE, 'Date',     ItemCount);
  DefineItem(VT_BSTR, 'String',   ItemCount);
  DefineItem(VT_CY,   'Currency', ItemCount);
end;

procedure InitializeDataSource;
begin
  XRTLOPCDADataSourceCache:= TXRTLOPCDADataSourceCache.Create;
  XRTLOPCDADataSourceDevice:= TOPCDA20ServerDevice.Create;
end;

initialization
begin
  TCOMObjectFactory.Create(COMServer, TXRTLOPCDA20Group, CLASS_OPCDA20Group,
                           'OPCDA20Group', 'xrtl OPC DA 2.0 Group', ciInternal, tmFree);
  COMServer.UIInteractive:= False;
  InitializeDataSource;
  InitializeNameSpace;
  TXRTLOPCDA20COMObjectFactory.Create(COMServer, TOPCDA20ServerDemo, CLASS_OPCDA20ServerDemo,
                                      'OPCDA20ServerDemo', 'xrtl OPC DA 2.0 Server Demo',
                                      ciMultiInstance);
end;

end.

unit xrtl_opc_sdk_OPCDA30Group;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Classes, COMObj, AxCtrls, ActiveX, SyncObjs,
  DateUtils, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_TimeStamp, xrtl_util_MemoryUtils,
  xrtl_util_Map, xrtl_util_Container,
  xrtl_util_COMUtils, xrtl_util_Lock,
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types,
  xrtl_opc_sdk_OPCServer, xrtl_opc_sdk_OPCLocaleManager,
  xrtl_opc_sdk_OPCDAServer, xrtl_opc_sdk_OPCDANameSpace,
  xrtl_opc_sdk_EnumOPCItemAttributes,
  xrtl_opc_sdk_OPCDADataSource,
  xrtl_opc_sdk_DA, xrtl_opc_sdk_OPCDAVariantManager;

type
  TXRTLOPCDA30Group = class(TXRTLOPCDAGroup)
  end;

implementation

end.

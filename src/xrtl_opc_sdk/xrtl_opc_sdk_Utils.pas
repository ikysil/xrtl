unit xrtl_opc_sdk_Utils;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, ActiveX, SysUtils,
  xrtl_opc_DA;

procedure XRTLAddItemProperty(dwPropertyID, vtDataType: DWORD; const Description: WideString = '');
function  XRTLGetItemProperty(dwPropertyID: DWORD; var vtDataType: DWORD; var Description: WideString): Boolean;

implementation

uses
  xrtl_util_Map, xrtl_util_Value;

var
  ItemProperties: TXRTLSynchronizedMap = nil;

type
  TXRTLItemPropertyDef = class
    vtDataType: DWORD;
    Description: WideString;
  end;

procedure XRTLAddItemProperty(dwPropertyID, vtDataType: DWORD; const Description: WideString = '');
var
  Def: TXRTLItemPropertyDef;
  DefValue: IXRTLValue;
begin
  try
    ItemProperties.BeginWrite;
    DefValue:= ItemProperties.GetValue(XRTLValue(dwPropertyID));
    if Assigned(DefValue) then
    begin
      XRTLGetAsObject(DefValue, Def);
    end
    else
    begin
      Def:= TXRTLItemPropertyDef.Create;
      ItemProperties.SetValue(XRTLValue(dwPropertyID), XRTLValue(Def, True));
    end;
    Def.vtDataType:= vtDataType;
    Def.Description:= Description;
  finally
    ItemProperties.EndWrite;
  end;
end;

function XRTLGetItemProperty(dwPropertyID: DWORD; var vtDataType: DWORD; var Description: WideString): Boolean;
var
  Def: TXRTLItemPropertyDef;
begin
  try
    ItemProperties.BeginRead;
    XRTLGetAsObject(ItemProperties.GetValue(XRTLValue(dwPropertyID)), Def);
    Result:= Assigned(Def);
    if Result then
    begin
      vtDataType:= Def.vtDataType;
      Description:= Def.Description;
    end;
  finally
    ItemProperties.EndRead;
  end;
end;

procedure InitializeItemProperties;
begin
//  OPC Specific Properties
  XRTLAddItemProperty(OPC_PROP_CDT,            VT_I2,   'Item Canonical DataType');
  XRTLAddItemProperty(OPC_PROP_VALUE,          VT_NULL, 'Item Value');
  XRTLAddItemProperty(OPC_PROP_QUALITY,        VT_I2,   'Item Quality');
  XRTLAddItemProperty(OPC_PROP_TIME,           VT_DATE, 'Item Timestamp');
  XRTLAddItemProperty(OPC_PROP_RIGHTS,         VT_I4,   'Item Access Rights');
  XRTLAddItemProperty(OPC_PROP_SCANRATE,       VT_R4,   'Server Scan Rate');
//  7-99 reserved for future OPC use
//  Recommended Properties
  XRTLAddItemProperty(OPC_PROP_UNIT,           VT_BSTR, 'EU Units');
  XRTLAddItemProperty(OPC_PROP_DESC,           VT_BSTR, 'Item Description');
  XRTLAddItemProperty(OPC_PROP_HIEU,           VT_R8,   'High EU');
  XRTLAddItemProperty(OPC_PROP_LOEU,           VT_R8,   'Low EU');
  XRTLAddItemProperty(OPC_PROP_HIRANGE,        VT_R8,   'High Instrumental Range');
  XRTLAddItemProperty(OPC_PROP_LORANGE,        VT_R8,   'Low Instrumental Range');
  XRTLAddItemProperty(OPC_PROP_CLOSE,          VT_BSTR, 'Contact Close Label');
  XRTLAddItemProperty(OPC_PROP_OPEN,           VT_BSTR, 'Contact Open Label');
  XRTLAddItemProperty(OPC_PROP_TIMEZONE,       VT_I4,   'Item Timezone');
//  109-199 reserved for future OPC use
  XRTLAddItemProperty(OPC_PROP_DSP,            VT_BSTR, 'Default Display');
  XRTLAddItemProperty(OPC_PROP_FGC,            VT_I4,   'Current Foreground Color');
  XRTLAddItemProperty(OPC_PROP_BGC,            VT_I4,   'Current Background Color');
  XRTLAddItemProperty(OPC_PROP_BLINK,          VT_BOOL, 'Current Blink');
  XRTLAddItemProperty(OPC_PROP_BMP,            VT_BSTR, 'BMP File');
  XRTLAddItemProperty(OPC_PROP_SND,            VT_BSTR, 'Sound File');
  XRTLAddItemProperty(OPC_PROP_HTML,           VT_BSTR, 'HTML File');
  XRTLAddItemProperty(OPC_PROP_AVI,            VT_BSTR, 'AVI File');
//  207-299 reserved for future OPC use
//  Alarm and Condition properties
  XRTLAddItemProperty(OPC_PROP_ALMSTAT,        VT_BSTR, 'Condition Status');
  XRTLAddItemProperty(OPC_PROP_ALMHELP,        VT_BSTR, 'Alarm Quick Help');
  XRTLAddItemProperty(OPC_PROP_ALMAREAS,       VT_BSTR or VT_ARRAY, 'Alarm Area List');
  XRTLAddItemProperty(OPC_PROP_ALMPRIMARYAREA, VT_BSTR, 'Primary Alarm Area');
  XRTLAddItemProperty(OPC_PROP_ALMCONDITION,   VT_BSTR, 'Condition Logic');
  XRTLAddItemProperty(OPC_PROP_ALMLIMIT,       VT_BSTR, 'Limit Exceeded');
  XRTLAddItemProperty(OPC_PROP_ALMDB,          VT_R8,   'Deadband');
  XRTLAddItemProperty(OPC_PROP_ALMHH,          VT_R8,   'HiHi Limit');
  XRTLAddItemProperty(OPC_PROP_ALMH,           VT_R8,   'Hi Limit');
  XRTLAddItemProperty(OPC_PROP_ALML,           VT_R8,   'Lo Limit');
  XRTLAddItemProperty(OPC_PROP_ALMLL,          VT_R8,   'LoLo Limit');
  XRTLAddItemProperty(OPC_PROP_ALMROC,         VT_R8,   'Rate of Change Limit');
  XRTLAddItemProperty(OPC_PROP_ALMDEV,         VT_R8,   'Deviation Limit');
//  313-399 reserved for future OPC Alarms and Events use
//  400-4999 reserved for future OPC use
end;

initialization
begin
  ItemProperties:= TXRTLSynchronizedMap.Create(TXRTLArrayMap.Create);
  InitializeItemProperties;
end;

finalization
begin
  FreeAndNil(ItemProperties);
end;

end.

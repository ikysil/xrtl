unit xrtl_opc_sdk_OPCLocaleManager;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX,
  xrtl_util_COMUtils, 
  xrtl_opc_Common, xrtl_opc_Error;

type
  IXRTLOPCLocaleManager = interface
  ['{58E576EE-EA34-4FDA-87D9-E60245EEB636}']
    function   IsLocaleIDAvailable(dwLCID: TLCID; out pbResult: BOOL): HRESULT; stdcall;
    function   QueryAvailableLocaleIDs(out pdwCount: UINT; out pdwLcid: PLCIDARRAY): HResult; stdcall;
    function   GetErrorString(dwError: HRESULT; dwLocale: TLCID; out ppString: POleStr): HRESULT; stdcall;
  end;

var
  XRTLOPCLocaleManager: IXRTLOPCLocaleManager = nil;

implementation

uses
  xrtl_opc_sdk_ResourceStrings, xrtl_opc_Utils;

type
  TXRTLDefaultOPCLocaleManager = class(TInterfacedObject, IXRTLOPCLocaleManager)
    function   IsLocaleIDAvailable(dwLCID: TLCID; out pbResult: BOOL): HRESULT; stdcall;
    function   QueryAvailableLocaleIDs(out pdwCount: UINT; out pdwLcid: PLCIDARRAY): HRESULT; stdcall;
    function   GetErrorString(dwError: HRESULT; dwLocale: TLCID; out ppString: POleStr): HRESULT; stdcall;
  end;

{ TXRTLDefaultOPCLocaleManager }

function TXRTLDefaultOPCLocaleManager.IsLocaleIDAvailable(dwLCID: TLCID; out pbResult: BOOL): HRESULT;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(pbResult);
    pbResult:= (Languages.IndexOf(dwLCID) >= 0) or (dwLCID = LOCALE_SYSTEM_DEFAULT) or (dwLCID = LOCALE_USER_DEFAULT);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLDefaultOPCLocaleManager.QueryAvailableLocaleIDs(out pdwCount: UINT; out pdwLcid: PLCIDARRAY): HRESULT;
var
  I: Integer;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(pdwCount);
    XRTLCheckOutArgument(pdwLcid);
    try
      pdwCount:= Languages.Count + 2;
      pdwLcid:= PLCIDARRAY(CoTaskMemAlloc(pdwCount * SizeOf(TLCID)));
      if not Assigned(pdwLcid) then
        OLEError(E_OUTOFMEMORY);
      pdwLcid[0]:= LOCALE_SYSTEM_DEFAULT;
      pdwLcid[1]:= LOCALE_USER_DEFAULT;
      for I:= 2 to pdwCount - 1 do
      begin
        pdwLcid[I]:= Languages.LocaleID[I - 2];
      end;
    except
      XRTLFreeLCIDARRAY(pdwLcid);
      pdwCount:= 0;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLDefaultOPCLocaleManager.GetErrorString(dwError: HRESULT; dwLocale: TLCID; out ppString: POleStr): HRESULT;
var
  S: WideString;
  Buffer: array[0 .. 1023] of WideChar;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(ppString);
    try
      ppString:= nil;
      S:='';
      case dwError of
        OPC_E_INVALIDHANDLE:
          S:= SOPC_E_INVALIDHANDLE;
        OPC_E_BADTYPE:
          S:= SOPC_E_BADTYPE;
        OPC_E_PUBLIC:
          S:= SOPC_E_PUBLIC;
        OPC_E_BADRIGHTS:
          S:= SOPC_E_BADRIGHTS;
        OPC_E_UNKNOWNITEMID:
          S:= SOPC_E_UNKNOWNITEMID;
        OPC_E_INVALIDITEMID:
          S:= SOPC_E_INVALIDITEMID;
        OPC_E_INVALIDFILTER:
          S:= SOPC_E_INVALIDFILTER;
        OPC_E_UNKNOWNPATH:
          S:= SOPC_E_UNKNOWNPATH;
        OPC_E_RANGE:
          S:= SOPC_E_RANGE;
        OPC_E_DUPLICATENAME:
          S:= SOPC_E_DUPLICATENAME;
        OPC_S_UNSUPPORTEDRATE:
          S:= SOPC_S_UNSUPPORTEDRATE;
        OPC_S_CLAMP:
          S:= SOPC_S_CLAMP;
        OPC_S_INUSE:
          S:= SOPC_S_INUSE;
        OPC_E_INVALIDCONFIGFILE:
          S:= SOPC_E_INVALIDCONFIGFILE;
        OPC_E_NOTFOUND:
          S:= SOPC_E_NOTFOUND;
        OPC_E_INVALID_PID:
          S:= SOPC_E_INVALID_PID;
        OPC_S_ALREADYACKED:
          S:= SOPC_S_ALREADYACKED;
        OPC_S_INVALIDBUFFERTIME:
          S:= SOPC_S_INVALIDBUFFERTIME;
        OPC_S_INVALIDMAXSIZE:
          S:= SOPC_S_INVALIDMAXSIZE;
//        OPC_E_INVALIDBRANCHNAME:
//          S:= SOPC_E_INVALIDBRANCHNAME;
        OPC_E_INVALIDTIME:
          S:= SOPC_E_INVALIDTIME;
        OPC_E_BUSY:
          S:= SOPC_E_BUSY;
        OPC_E_NOINFO:
          S:= SOPC_E_NOINFO;
      end;
      if Length(S) = 0 then
      begin
        FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
                       nil, LongWord(dwError), dwLocale, Buffer, SizeOf(Buffer), nil);
        S:= WideString(Buffer);
      end;
      ppString:= XRTLAllocOutWideString(S);
    except
      XRTLFreeOutWideString(ppString);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

initialization
begin
  XRTLOPCLocaleManager:= TXRTLDefaultOPCLocaleManager.Create;
end;

finalization
begin
  XRTLOPCLocaleManager:= nil;
end;

end.

unit xrtl_opc_sdk_OPCDA20Classes;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_COMUtils,
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types,
  xrtl_opc_sdk_OPCDANameSpace,
  xrtl_opc_sdk_OPCDAServer,
  xrtl_opc_sdk_DA;

type
  TXRTLOPCDA20_IOPCItemProperties = class(TAggregatedObject, IOPCItemProperties)
  public
    function   QueryAvailableProperties(szItemID: POleStr; out pdwCount: DWORD;
                                        out ppPropertyIDs: PDWORDARRAY; out ppDescriptions: POleStrList;
                                        out ppvtDataTypes: PVarTypeList): HResult; stdcall;
    function   GetItemProperties(szItemID: POleStr; dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                                 out ppvData: POleVariantArray; out ppErrors: PResultList): HResult; stdcall;
    function   LookupItemIDs(szItemID: POleStr; dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                             out ppszNewItemIDs: POleStrList; out ppErrors: PResultList): HResult; stdcall;
  end;

  TXRTLOPCDA20_IOPCBrowseServerAddressSpace = class(TAggregatedObject, IOPCBrowseServerAddressSpace)
  private
    FBrowseRoot: Variant{array of WideString};
  public
    constructor Create(const Controller: IInterface);
    destructor Destroy; override;
    function   QueryOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
    function   ChangeBrowsePosition(dwBrowseDirection: OPCBROWSEDIRECTION;
                                    szString: POleStr): HResult; stdcall;
    function   BrowseOPCItemIDs(dwBrowseFilterType: OPCBROWSETYPE; szFilterCriteria: POleStr;
                                vtDataTypeFilter: TVarType; dwAccessRightsFilter: DWORD;
                                out ppIEnumString: IEnumString): HResult; stdcall;
    function   GetItemID(szItemDataID: POleStr; out szItemID: POleStr): HResult; stdcall;
    function   BrowseAccessPaths(szItemID: POleStr; out ppIEnumString: IEnumString): HResult; stdcall;
  end;

implementation

{ TXRTLOPCDA20_IOPCItemProperties }

function TXRTLOPCDA20_IOPCItemProperties.QueryAvailableProperties(
  szItemID: POleStr; out pdwCount: DWORD; out ppPropertyIDs: PDWORDARRAY;
  out ppDescriptions: POleStrList;
  out ppvtDataTypes: PVarTypeList): HResult;
var
  Path: OleVariant;
  Item: IXRTLOPCDANameSpaceItem;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(pdwCount);
    XRTLCheckOutArgument(ppPropertyIDs);
    XRTLCheckOutArgument(ppDescriptions);
    XRTLCheckOutArgument(ppvtDataTypes);
    pdwCount:= 0;
    ppPropertyIDs:= nil;
    ppDescriptions:= nil;
    ppvtDataTypes:= nil;
    OLECheck(XRTLOPCDANameSpace.SplitItemID(szItemID, Path));
    OLECheck(XRTLOPCDANameSpace.GetItem(Path, Item));
    Result:= Item.QueryAvailableProperties(pdwCount, ppPropertyIDs, ppDescriptions, ppvtDataTypes);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20_IOPCItemProperties.GetItemProperties(
  szItemID: POleStr; dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
  out ppvData: POleVariantArray; out ppErrors: PResultList): HResult;
var
  Path: OleVariant;
  Item: IXRTLOPCDANameSpaceItem;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ppvData);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    ppvData:= nil;
    ppErrors:= nil;
    OLECheck(XRTLOPCDANameSpace.SplitItemID(szItemID, Path));
    OLECheck(XRTLOPCDANameSpace.GetItem(Path, Item));
    Result:= Item.GetItemProperties(dwCount, pdwPropertyIDs, ppvData, ppErrors);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20_IOPCItemProperties.LookupItemIDs(
  szItemID: POLEStr; dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
  out ppszNewItemIDs: POleStrList; out ppErrors: PResultList): HResult;
var
  Path: OleVariant;
  Item: IXRTLOPCDANameSpaceItem;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ppszNewItemIDs);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    ppszNewItemIDs:= nil;
    ppErrors:= nil;
    OLECheck(XRTLOPCDANameSpace.SplitItemID(szItemID, Path));
    OLECheck(XRTLOPCDANameSpace.GetItem(Path, Item));
    Result:= Item.LookupItemIDs(dwCount, pdwPropertyIDs, ppszNewItemIDs, ppErrors);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

{ TXRTLOPCDA20_IOPCBrowseServerAddressSpace }

constructor TXRTLOPCDA20_IOPCBrowseServerAddressSpace.Create(const Controller: IInterface);
begin
  inherited;
  FBrowseRoot:= VarArrayCreate([0, -1], varOleStr);
end;

destructor TXRTLOPCDA20_IOPCBrowseServerAddressSpace.Destroy;
begin
  VarClear(FBrowseRoot);
  inherited;
end;

function TXRTLOPCDA20_IOPCBrowseServerAddressSpace.BrowseAccessPaths(
  szItemID: POleStr; out ppIEnumString: IEnumString): HResult;
begin
  XRTLCheckOPCDAServerState;
  Result:= E_NOTIMPL;
end;

function TXRTLOPCDA20_IOPCBrowseServerAddressSpace.BrowseOPCItemIDs(
  dwBrowseFilterType: OPCBROWSETYPE; szFilterCriteria: POleStr;
  vtDataTypeFilter: TVarType; dwAccessRightsFilter: DWORD;
  out ppIEnumString: IEnumString): HResult;
begin
  try
    XRTLCheckOPCDAServerState;
    Result:= XRTLOPCDANameSpace.CreateItemEnumerator(FBrowseRoot, dwBrowseFilterType, szFilterCriteria,
                                                     vtDataTypeFilter, dwAccessRightsFilter,
                                                     ppIEnumString);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20_IOPCBrowseServerAddressSpace.ChangeBrowsePosition(
  dwBrowseDirection: OPCBROWSEDIRECTION; szString: POleStr): HResult;
var
  pNameSpaceType: OPCNAMESPACETYPE;
  LRoot: OleVariant;
  bResult1, bResult2: BOOL;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState;
    OLECheck(XRTLOPCDANameSpace.GetOrganization(pNameSpaceType));
    if pNameSpaceType = OPC_NS_FLAT then
      OLEError(E_FAIL);
    case dwBrowseDirection of
      OPC_BROWSE_UP:
      begin
        if VarArrayHighBound(FBrowseRoot, 1) < 0 then
          OLEError(E_FAIL);
        VarArrayRedim(FBrowseRoot, VarArrayHighBound(FBrowseRoot, 1) - 1);
      end;
      OPC_BROWSE_DOWN:
      begin
        if not WideSameStr('', szString) then
        begin
          LRoot:= FBrowseRoot;
          VarArrayRedim(LRoot, VarArrayHighBound(LRoot, 1) + 1);
          LRoot[VarArrayHighBound(LRoot, 1)]:= WideString(szString);
          OLECheck(XRTLOPCDANameSpace.HasItem(LRoot, bResult1));
          OLECheck(XRTLOPCDANameSpace.IsBranch(LRoot, bResult2));
          if bResult1 and bResult2 then
            VarCopy(FBrowseRoot, LRoot)
          else
            OLEError(E_INVALIDARG);
        end
        else
          OLEError(E_INVALIDARG);
      end;
      OPC_BROWSE_TO:
      begin
        if WideSameStr('', szString) then
        begin
          FBrowseRoot:= VarArrayCreate([0, -1], varOleStr);
          Exit;
        end;
        OLECheck(XRTLOPCDANameSpace.SplitItemID(szString, LRoot));
        OLECheck(XRTLOPCDANameSpace.HasItem(LRoot, bResult1));
        OLECheck(XRTLOPCDANameSpace.IsBranch(LRoot, bResult2));
        if bResult1 and bResult2 then
          VarCopy(FBrowseRoot, LRoot)
        else
          OLEError(E_INVALIDARG);
      end;
    else
      OLEError(E_INVALIDARG);
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20_IOPCBrowseServerAddressSpace.GetItemID(
  szItemDataID: POLEStr; out szItemID: POLEStr): HResult;
var
  pNameSpaceType: OPCNAMESPACETYPE;
  Separator: WideString;
  POS: POLEStr;
  bResult: BOOL;
  LItemID, LItemDataID: WideString;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(szItemDataID);
    XRTLCheckOutArgument(szItemID);
    try
      OLECheck(XRTLOPCDANameSpace.GetOrganization(pNameSpaceType));
      LItemDataID:= WideString(szItemDataID);
      if (pNameSpaceType = OPC_NS_FLAT) or (VarArrayHighBound(FBrowseRoot, 1) < 0) then
      begin
        LItemID:= LItemDataID;
      end
      else
      begin
        OLECheck(XRTLOPCDANameSpace.GetItemIDSeparator(POS));
        Separator:= WideString(POS);
        OLECheck(XRTLOPCDANameSpace.CombineItemID(FBrowseRoot, POS));
        LItemID:= WideString(POS);
        if not WideSameStr('', LItemDataID) then
          LItemID:= LItemID + Separator + LItemDataID;
      end;
      if not WideSameStr('', LItemID) then
      begin
        OLECheck(XRTLOPCDANameSpace.HasItem(LItemID, bResult));
        if not bResult then
          OLEError(OPC_E_INVALIDITEMID);
      end;
      szItemID:= XRTLAllocOutWideString(LItemID);
    except
      XRTLFreeOutWideString(szItemID);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20_IOPCBrowseServerAddressSpace.QueryOrganization(
  out pNameSpaceType: OPCNAMESPACETYPE): HResult;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(pNameSpaceType);
    if Assigned(XRTLOPCDANameSpace) then
      Result:= XRTLOPCDANameSpace.GetOrganization(pNameSpaceType)
    else
      Result:= OPC_E_NOINFO;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

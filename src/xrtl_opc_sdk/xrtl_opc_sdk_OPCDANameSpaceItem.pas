unit xrtl_opc_sdk_OPCDANameSpaceItem;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_NameSpacePath, xrtl_util_TimeStamp, xrtl_util_COMUtils, 
  xrtl_util_Map, 
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types,
  xrtl_opc_sdk_OPCDADataSource, xrtl_opc_sdk_DA,
  xrtl_opc_sdk_OPCDAVariantManager;

type
  TXRTLOPCDANameSpaceItem = class(TInterfacedObject, IXRTLOPCDANameSpaceItem)
  private
    FProperties: TXRTLMap;
    FDataType: TVarType;
    FDescription: WideString;
    FPath: TXRTLNameSpacePath;
    FAccessRights: DWORD;
    FItemID: WideString;
    function   GetName: WideString;
    procedure  SetPath(const Value: TXRTLNameSpacePath);
  public
    constructor Create(const APath: TXRTLNameSpacePath; const ADataType: TVarType;
                       const ADescription: WideString);
    destructor Destroy; override;
    property   Name: WideString read GetName;
    property   Path: TXRTLNameSpacePath read FPath write SetPath;
    property   DataType: TVarType read FDataType write FDataType;
    property   Description: WideString read FDescription write FDescription;
    property   AccessRights: DWORD read FAccessRights write FAccessRights;
    property   ItemID: WideString read FItemID write FItemID;
    function   IXRTLOPCDANameSpaceItem.QueryAvailableProperties = IXRTLOPCDANameSpaceItem_QueryAvailableProperties;
    function   IXRTLOPCDANameSpaceItem.GetItemProperties        = IXRTLOPCDANameSpaceItem_GetItemProperties;
    function   IXRTLOPCDANameSpaceItem.LookupItemIDs            = IXRTLOPCDANameSpaceItem_LookupItemIDs;
    function   IXRTLOPCDANameSpaceItem.GetDataType              = IXRTLOPCDANameSpaceItem_GetDataType;
    function   IXRTLOPCDANameSpaceItem.SetDataType              = IXRTLOPCDANameSpaceItem_SetDataType;
    function   IXRTLOPCDANameSpaceItem.GetAccessRights          = IXRTLOPCDANameSpaceItem_GetAccessRights;
    function   IXRTLOPCDANameSpaceItem.SetAccessRights          = IXRTLOPCDANameSpaceItem_SetAccessRights;
    function   IXRTLOPCDANameSpaceItem.CanRead                  = IXRTLOPCDANameSpaceItem_CanRead;
    function   IXRTLOPCDANameSpaceItem.CanWrite                 = IXRTLOPCDANameSpaceItem_CanWrite;
    function   IXRTLOPCDANameSpaceItem.GetItemID                = IXRTLOPCDANameSpaceItem_GetItemID;
    function   IXRTLOPCDANameSpaceItem_QueryAvailableProperties(out pdwCount: DWORD;
                 out ppPropertyIDs: PDWORDARRAY; out ppDescriptions: POleStrList;
                 out ppvtDataTypes: PVarTypeList): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_GetItemProperties(dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                 out ppvData: POleVariantArray; out ppErrors: PResultList): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_LookupItemIDs(dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                 out ppszNewItemIDs: POleStrList; out ppErrors: PResultList): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_GetAccessRights(out AAccessRights: DWORD): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_SetAccessRights(AAccessRights: DWORD): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_GetDataType(out ADataType: TVarType): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_SetDataType(ADataType: TVarType): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_CanRead(out bResult: BOOL): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_CanWrite(out bResult: BOOL): HResult; stdcall;
    function   IXRTLOPCDANameSpaceItem_GetItemID(out szItemID: POLEStr): HResult; stdcall;
  end;

implementation

uses
  xrtl_opc_Utils;

{ TXRTLOPCDANameSpaceItem }

constructor TXRTLOPCDANameSpaceItem.Create(const APath: TXRTLNameSpacePath;
  const ADataType: TVarType; const ADescription: WideString);
begin
  inherited Create;
  FProperties:= TXRTLArrayMap.Create;
  Path:= APath;
  FDataType:= ADataType;
  FDescription:= ADescription;
  FAccessRights:= OPC_READABLE or OPC_WRITEABLE;
end;

destructor TXRTLOPCDANameSpaceItem.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

function TXRTLOPCDANameSpaceItem.GetName: WideString;
begin
  Result:= Path[XRTLNameSpacePathGetLength(Path) - 1];
end;

procedure TXRTLOPCDANameSpaceItem.SetPath(const Value: TXRTLNameSpacePath);
begin
  FPath:= Copy(Value);
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_QueryAvailableProperties(
  out pdwCount: DWORD; out ppPropertyIDs: PDWORDARRAY;
  out ppDescriptions: POleStrList;
  out ppvtDataTypes: PVarTypeList): HResult;
var
  Value: OleVariant;
  Quality: Word;
  ReadResult: HResult;
  ftTimeStamp: TFileTime;
  LItemID: POLEStr;
begin
  Result:= S_OK;
  LItemID:= nil;
  try
    XRTLCheckOutArgument(pdwCount);
    XRTLCheckOutArgument(ppPropertyIDs);
    XRTLCheckOutArgument(ppDescriptions);
    XRTLCheckOutArgument(ppvtDataTypes);
    try
      pdwCount:= 6;
      ppPropertyIDs:=  PDWORDARRAY(CoTaskMemAlloc(pdwCount * SizeOf(DWORD)));
      ppDescriptions:= POleStrList(CoTaskMemAlloc(pdwCount * SizeOf(POleStr)));
      ppvtDataTypes:=  PVarTypeList(CoTaskMemAlloc(pdwCount * SizeOf(TVarType)));
      if not Assigned(ppPropertyIDs) or not Assigned(ppDescriptions) or not Assigned(ppvtDataTypes) then
        OLEError(E_OUTOFMEMORY);
      ZeroMemory(ppPropertyIDs, pdwCount * SizeOf(DWORD));
      ZeroMemory(ppDescriptions, pdwCount * SizeOf(POleStr));
      ZeroMemory(ppvtDataTypes, pdwCount * SizeOf(TVarType));
      try
        OLECheck(IXRTLOPCDANameSpaceItem_GetItemID(LItemID));
        ReadResult:= XRTLOPCDADataSourceDevice.Read(LItemID, Value, Quality, ftTimeStamp);
      finally
        XRTLFreeOutWideString(LItemID);
      end;
//  fill in Item Canonical DataType
      ppPropertyIDs[0]:=  OPC_PROP_CDT;
      ppDescriptions[0]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_DATATYPE);
      ppvtDataTypes[0]:=  VT_I2;
//  fill in Item Value
      ppPropertyIDs[1]:=  OPC_PROP_VALUE;
      ppDescriptions[1]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_VALUE);
      if Succeeded(ReadResult) then
        ppvtDataTypes[1]:=  VarType(Value)
      else
        ppvtDataTypes[1]:=  VT_EMPTY;
//  fill in Item Quality
      ppPropertyIDs[2]:=  OPC_PROP_QUALITY;
      ppDescriptions[2]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_QUALITY);
      ppvtDataTypes[2]:=  VT_I2;
//  fill in Item Timestamp
      ppPropertyIDs[3]:=  OPC_PROP_TIME;
      ppDescriptions[3]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_TIMESTAMP);
      ppvtDataTypes[3]:=  VT_DATE;
//  fill in Item Access Rights
      ppPropertyIDs[4]:=  OPC_PROP_RIGHTS;
      ppDescriptions[4]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_ACCESS_RIGHTS);
      ppvtDataTypes[4]:=  VT_I4;
//  fill in Item Access Rights
      ppPropertyIDs[5]:=  OPC_PROP_SCANRATE;
      ppDescriptions[5]:= XRTLAllocOutWideString(OPC_PROPERTY_DESC_SCAN_RATE);
      ppvtDataTypes[5]:=  VT_R4;
    except
      XRTLFreeDWORDARRAY(ppPropertyIDs);
      XRTLFreeOleStrList(ppDescriptions, pdwCount);
      XRTLFreeVarTypeList(ppvtDataTypes);
      pdwCount:= 0;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_GetItemProperties(dwCount: DWORD;
  pdwPropertyIDs: PDWORDARRAY; out ppvData: POleVariantArray;
  out ppErrors: PResultList): HResult;
var
  I: DWORD;
  Data: OleVariant;
  TimeStamp: TXRTLTimeStamp;
  Value: OleVariant;
  Quality: Word;
  ReadResult: HResult;
  ftTimeStamp: TFileTime;
  LItemID: POLEStr;
begin
  Result:= S_OK;
  TimeStamp:= nil;
  try
    XRTLCheckOutArgument(ppvData);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      TimeStamp:= TXRTLTimeStamp.Create;
      try
        ppvData:= POleVariantArray(CoTaskMemAlloc(dwCount * SizeOf(OleVariant)));
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppvData) or not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppvData, dwCount * SizeOf(OleVariant));
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        LItemID:= nil;
        try
          OLECheck(IXRTLOPCDANameSpaceItem_GetItemID(LItemID));
          ReadResult:= XRTLOPCDADataSourceDevice.Read(LItemID, Value, Quality, ftTimeStamp);
        finally
          XRTLFreeOutWideString(LItemID);
        end;
        if Succeeded(ReadResult) then
          TimeStamp.UTCFileTime:= ftTimeStamp;
        for I:= 0 to dwCount - 1 do
        begin
          VarClear(Data);
          ppErrors[I]:= S_OK;
          case pdwPropertyIDs[I] of
            OPC_PROP_CDT:     ppErrors[I]:= VariantChangeType(Data, DataType, 0, VT_I2);
            OPC_PROP_VALUE:
            begin
              if Succeeded(ReadResult) then
                VarCopy(Data, Value)
              else
                VarClear(Data);
            end;
            OPC_PROP_QUALITY:
            begin
              if Succeeded(ReadResult) then
                ppErrors[I]:= VariantChangeType(Data, Quality, 0, VT_I2)
              else
                VarClear(Data);
            end;
            OPC_PROP_TIME:
            begin
              if Succeeded(ReadResult) then
                VarCopy(Data, VarFromDateTime(TimeStamp.UTCDateTime))
              else
                VarClear(Data);
            end;
            OPC_PROP_RIGHTS:  VarCopy(Data, AccessRights);
            OPC_PROP_SCANRATE: ppErrors[I]:= VariantChangeType(Data, 1E3, 0, VT_R4);
          else
            ppErrors[I]:= OPC_E_INVALID_PID;
          end;
          if Failed(ppErrors[I]) then
            Result:= S_FALSE;
          ppvData[I]:= Data;
        end;
      except
        XRTLFreeOleVariantArray(ppvData, dwCount);
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      FreeAndNil(TimeStamp);
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_LookupItemIDs(dwCount: DWORD;
  pdwPropertyIDs: PDWORDARRAY; out ppszNewItemIDs: POleStrList;
  out ppErrors: PResultList): HResult;
var
  I: Integer;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(ppszNewItemIDs);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      ppszNewItemIDs:= POleStrList(CoTaskMemAlloc(dwCount * SizeOf(POleStr)));
      ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
      if not Assigned(ppszNewItemIDs) or not Assigned(ppErrors) then
        OLEError(E_OUTOFMEMORY);
      ZeroMemory(ppszNewItemIDs, dwCount * SizeOf(POleStr));
      ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
      for I:= 0 to dwCount - 1 do
      begin
        if pdwPropertyIDs[I] in [OPC_PROP_VALUE, OPC_PROP_QUALITY, OPC_PROP_TIME] then
        begin
          Result:= S_FALSE;
          ppErrors[I]:= OPC_E_INVALID_PID;
          ppszNewItemIDs[I]:= nil;
        end
        else
        begin
          Result:= S_FALSE;
          ppErrors[I]:= OPC_E_INVALID_PID;
          ppszNewItemIDs[I]:= nil;
        end;
      end;
    except
      XRTLFreeOleStrList(ppszNewItemIDs, dwCount);
      XRTLFreeResultList(ppErrors);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_GetAccessRights(
  out AAccessRights: DWORD): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(AAccessRights);
    AAccessRights:= FAccessRights;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_SetAccessRights(
  AAccessRights: DWORD): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckArgument((AAccessRights and not (OPC_READABLE or OPC_WRITEABLE or $FFFF0000)) = 0);
    FAccessRights:= AAccessRights;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_GetDataType(
  out ADataType: TVarType): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(ADataType);
    ADataType:= FDataType;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_SetDataType(
  ADataType: TVarType): HResult;
var
  bResult: BOOL;
begin
  try
    OLECheck(XRTLOPCDAVariantManager.IsTypeSupported(ADataType, bResult));
    XRTLCheckArgument(bResult);
    FDataType:= ADataType;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_CanRead(out bResult: BOOL): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(bResult);
    bResult:= (FAccessRights and OPC_READABLE) <> 0;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_CanWrite(out bResult: BOOL): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(bResult);
    bResult:= (FAccessRights and OPC_WRITEABLE) <> 0;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpaceItem.IXRTLOPCDANameSpaceItem_GetItemID(out szItemID: POLEStr): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(szItemID);
    szItemID:= XRTLAllocOutWideString(ItemID);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

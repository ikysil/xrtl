unit xrtl_opc_sdk_OPCDA20Group;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Classes, COMObj, AxCtrls, ActiveX,
  {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_TimeStamp, xrtl_util_MemoryUtils, xrtl_util_Compat,
  xrtl_util_Map, xrtl_util_Container, xrtl_util_Value,
  xrtl_util_COMUtils, xrtl_util_Lock,
  xrtl_util_async_Core,
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types,
  xrtl_opc_sdk_OPCLocaleManager,
  xrtl_opc_sdk_OPCDAServer, xrtl_opc_sdk_OPCDANameSpace,
  xrtl_opc_sdk_EnumOPCItemAttributes,
  xrtl_opc_sdk_OPCDADataSource,
  xrtl_opc_sdk_DA, xrtl_opc_sdk_OPCDAVariantManager;

type
  TXRTLOPCDA20Group = class;

  TXRTLOPCDA20GroupASyncOperation = class
  protected
    procedure  DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint); virtual; abstract;
    procedure  DoCancel(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
  public
    Canceled: Boolean;
    TransactionID: DWORD;
    CancelID: DWORD;
    Source: OPCDATASOURCE;
    Group: TXRTLOPCDA20Group;
    procedure  Execute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
  end;

  TXRTLOPCDA20Group = class(TXRTLOPCDAGroup, IUnknown,
                            IConnectionPointContainer, IOPCGroupStateMgt,
                            IOPCItemMgt, IOPCSyncIO, IOPCASyncIO2)
  private
    FClientCount: Integer;
    FASyncQueue: TXRTLMap;
    FLastClientUpdate: TXRTLTimeStamp;
    FLastScheduleTask: IXRTLASyncTask;
  protected
    FConnectionPoints: TConnectionPoints;
    FIOPCDataCallbackConnectionPoint: TConnectionPoint;
    procedure  OPCDataCallbackConnectEvent(const Sink: IUnknown; Connecting: Boolean);
    function   EnqueueASyncRefresh(dwSource: OPCDATASOURCE; dwTransactionID: DWORD;
                                   out pdwCancelID: DWORD; DoOnDataChange: Boolean): HResult; overload;
    function   EnqueueASyncRefresh(UpdateTimeStamp: Boolean): HResult; overload;
    function   EnqueueASyncOnDataChange: HResult;
    procedure  InvokeOperation(const ATask: IXRTLASyncTask);
    procedure  InvokeOnDataChange(const ATask: IXRTLASyncTask);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  BeforeDestruction; override;
    property   LastClientUpdate: TXRTLTimeStamp read FLastClientUpdate;
    property   ConnectionPoints: TConnectionPoints read FConnectionPoints implements IConnectionPointContainer;
    property   IOPCDataCallbackConnectionPoint: TConnectionPoint read FIOPCDataCallbackConnectionPoint;
    function   IOPCGroupStateMgt.SetState   = IOPCGroupStateMgt_SetState;
    function   IOPCGroupStateMgt.GetState   = IOPCGroupStateMgt_GetState;
    function   IOPCGroupStateMgt.SetName    = IOPCGroupStateMgt_SetName;
    function   IOPCGroupStateMgt.CloneGroup = IOPCGroupStateMgt_CloneGroup;
    function   IOPCGroupStateMgt_SetState(pRequestedUpdateRate: PDWORD; out pRevisedUpdateRate: DWORD;
                                          pActive: PBOOL; pTimeBias: PLongint; pPercentDeadband: PSingle;
                                          pLCID: PLCID; phClientGroup: POPCHANDLE): HResult; stdcall;
    function   IOPCGroupStateMgt_GetState(out pUpdateRate: DWORD; out pActive: BOOL;
                                          out ppName: POleStr; out pTimeBias: Longint;
                                          out pPercentDeadband: Single; out pLCID: TLCID;
                                          out phClientGroup: OPCHANDLE; out phServerGroup: OPCHANDLE): HResult; stdcall;
    function   IOPCGroupStateMgt_SetName(szName: POleStr): HResult; stdcall;
    function   IOPCGroupStateMgt_CloneGroup(szName: POleStr; const riid: TIID; out ppUnk: IUnknown): HResult; stdcall;
    function   IOPCItemMgt.AddItems         = IOPCItemMgt_AddItems;
    function   IOPCItemMgt.ValidateItems    = IOPCItemMgt_ValidateItems;
    function   IOPCItemMgt.RemoveItems      = IOPCItemMgt_RemoveItems;
    function   IOPCItemMgt.SetActiveState   = IOPCItemMgt_SetActiveState;
    function   IOPCItemMgt.SetClientHandles = IOPCItemMgt_SetClientHandles;
    function   IOPCItemMgt.SetDatatypes     = IOPCItemMgt_SetDatatypes;
    function   IOPCItemMgt.CreateEnumerator = IOPCItemMgt_CreateEnumerator;
    function   IOPCItemMgt_AddItems(dwCount: DWORD; pItemArray: POPCITEMDEFARRAY;
                                    out ppAddResults: POPCITEMRESULTARRAY;
                                    out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_ValidateItems(dwCount: DWORD; pItemArray: POPCITEMDEFARRAY; bBlobUpdate: BOOL;
                                         out ppValidationResults: POPCITEMRESULTARRAY;
                                         out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_RemoveItems(dwCount: DWORD; phServer: POPCHANDLEARRAY;
                                       out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_SetActiveState(dwCount: DWORD; phServer: POPCHANDLEARRAY; bActive: BOOL;
                                          out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_SetClientHandles(dwCount: DWORD; phServer: POPCHANDLEARRAY; phClient: POPCHANDLEARRAY;
                                            out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_SetDatatypes(dwCount: DWORD; phServer: POPCHANDLEARRAY; pRequestedDatatypes: PVarTypeList;
                                        out ppErrors: PResultList): HResult; stdcall;
    function   IOPCItemMgt_CreateEnumerator(const riid: TIID; out ppUnk: IUnknown): HResult; stdcall;
    function   IOPCSyncIO.Read  = IOPCSyncIO_Read;
    function   IOPCSyncIO.Write = IOPCSyncIO_Write;
    function   IOPCSyncIO_Read(dwSource: OPCDATASOURCE; dwCount: DWORD; phServer: POPCHANDLEARRAY;
                               out ppItemValues: POPCITEMSTATEARRAY;
                               out ppErrors: PResultList): HResult; stdcall;
    function   IOPCSyncIO_Write(dwCount: DWORD; phServer: POPCHANDLEARRAY; pItemValues: POleVariantArray;
                                out ppErrors: PResultList): HResult; stdcall;
    function   IOPCASyncIO2.Read      = IOPCASyncIO2_Read;
    function   IOPCASyncIO2.Write     = IOPCASyncIO2_Write;
    function   IOPCASyncIO2.Refresh2  = IOPCASyncIO2_Refresh2;
    function   IOPCASyncIO2.Cancel2   = IOPCASyncIO2_Cancel2;
    function   IOPCASyncIO2.SetEnable = IOPCASyncIO2_SetEnable;
    function   IOPCASyncIO2.GetEnable = IOPCASyncIO2_GetEnable;
    function   IOPCASyncIO2_Read(dwCount: DWORD; phServer: POPCHANDLEARRAY; dwTransactionID: DWORD;
                                 out pdwCancelID: DWORD;
                                 out ppErrors: PResultList): HResult; stdcall;
    function   IOPCASyncIO2_Write(dwCount: DWORD; phServer: POPCHANDLEARRAY;
                                  pItemValues: POleVariantArray; dwTransactionID: DWORD;
                                  out pdwCancelID: DWORD;
                                  out ppErrors: PResultList): HResult; stdcall;
    function   IOPCASyncIO2_Refresh2(dwSource: OPCDATASOURCE; dwTransactionID: DWORD;
                                     out pdwCancelID: DWORD): HResult; stdcall;
    function   IOPCASyncIO2_Cancel2(dwCancelID: DWORD): HResult; stdcall;
    function   IOPCASyncIO2_SetEnable(bEnable: BOOL): HResult; stdcall;
    function   IOPCASyncIO2_GetEnable(out pbEnable: BOOL): HResult; stdcall;
  end;

implementation

uses
  xrtl_opc_Utils,
  xrtl_opc_sdk_OPCDA20GroupASyncOperation;

var
  FASyncContext: TXRTLASyncContext = nil;
  FOnDataChangeASyncContext: TXRTLASyncContext = nil;

{ TXRTLOPCDA20GroupASyncOperation }

procedure TXRTLOPCDA20GroupASyncOperation.DoCancel(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
var
  I: Integer;
  Client: IOPCDataCallback;
begin
  for I:= 0 to AIOPCDataCallbackConnectionPoint.SinkList.Count - 1 do
  begin
    try
      if Assigned(AIOPCDataCallbackConnectionPoint.SinkList[I]) and
         Supports(IUnknown(AIOPCDataCallbackConnectionPoint.SinkList[I]), IOPCDataCallback, Client) then
      Client.OnCancelComplete(TransactionID, Group.ClientHandle);
    except
    end;
  end;
end;

procedure TXRTLOPCDA20GroupASyncOperation.Execute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
begin
  if Canceled then
    DoCancel(AIOPCDataCallbackConnectionPoint)
  else
    DoExecute(AIOPCDataCallbackConnectionPoint);
end;

{ TXRTLOPCDA20Group }

constructor TXRTLOPCDA20Group.Create;
begin
  inherited;
  Deleted:= False;
  FConnectionPoints:= TConnectionPoints.Create(Self);
  FIOPCDataCallbackConnectionPoint:= FConnectionPoints.CreateConnectionPoint(IID_IOPCDataCallback, ckSingle, OPCDataCallbackConnectEvent);
  FASyncQueue:= TXRTLSynchronizedMap.Create(TXRTLArrayMap.Create);
  FClientCount:= 0;
  FItemUniqueId:= 0;
  FCancelUniqueId:= 0;
  FLastClientUpdate:= TXRTLTimeStamp.Create;
  Server:= nil;
  LocaleManager:= nil;
  NameSpace:= XRTLOPCDANameSpace;
  Cache:=  XRTLOPCDADataSourceCache;
  Device:= XRTLOPCDADataSourceDevice;
end;

destructor TXRTLOPCDA20Group.Destroy;
begin
  Device:= nil;
  Cache:= nil;
  NameSpace:= nil;
  LocaleManager:= nil;
  Server:= nil;
  FreeAndNil(FLastClientUpdate);
  FreeAndNil(FASyncQueue);
  FreeAndNil(FConnectionPoints);
  inherited;
end;

procedure TXRTLOPCDA20Group.BeforeDestruction;
begin
  try
    FLastScheduleTask.Cancel;
  except
  end;
  FLastScheduleTask:= nil;
  inherited;
end;

procedure TXRTLOPCDA20Group.OPCDataCallbackConnectEvent(const Sink: IInterface; Connecting: Boolean);
begin
  try
    BeginWrite;
    if Connecting then
    begin
      InterlockedIncrement(FClientCount);
//      FLastClientUpdate.SetCurrentTime;
      EnqueueASyncRefresh(True);
    end
    else
    begin
      if FClientCount > 0 then
      begin
        InterlockedDecrement(FClientCount);
      end;
    end;
  finally
    EndWrite;
  end;
end;

function TXRTLOPCDA20Group.IOPCGroupStateMgt_SetState(
  pRequestedUpdateRate: PDWORD; out pRevisedUpdateRate: DWORD;
  pActive: PBOOL; pTimeBias: PLongint; pPercentDeadband: PSingle;
  pLCID: PLCID; phClientGroup: POPCHANDLE): HResult;
var
  bResult: BOOL;
  dwLCID: TLCID;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(pRevisedUpdateRate);
    try
      BeginWrite;
      if Assigned(pLCID) then
      begin
        dwLCID:= pLCID^;
        if dwLCID <> 0 then
        begin
          OLECheck(LocaleManager.IsLocaleIDAvailable(dwLCID, bResult));
          XRTLCheckArgument(bResult);
        end;
        FLocaleID:= dwLCID;
      end;
      if Assigned(pRequestedUpdateRate) then
      begin
        FUpdateRate:= pRequestedUpdateRate^;
        if FUpdateRate < 10 then
        begin
          FUpdateRate:= 10;
          Result:= OPC_S_UNSUPPORTEDRATE;
        end;
      end;
      pRevisedUpdateRate:= FUpdateRate;
      if Assigned(pActive) then
      begin
        if FActive <> pActive^ then
        begin
          FActive:= pActive^;
          if FActive then
            EnqueueASyncRefresh(True);
        end;
      end;
      if Assigned(pTimeBias) then
        FTimeBias:= pTimeBias^;
      if Assigned(pPercentDeadband) then
      begin
        XRTLCheckArgument((pPercentDeadband^ >= 0) and (pPercentDeadband^ <= 100));
        FPercentDeadband:= pPercentDeadband^;
      end;
      if Assigned(phClientGroup) then
        FClientHandle:= phClientGroup^;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCGroupStateMgt_GetState(
  out pUpdateRate: DWORD; out pActive: BOOL; out ppName: POleStr;
  out pTimeBias: Integer; out pPercentDeadband: Single; out pLCID: TLCID;
  out phClientGroup, phServerGroup: OPCHANDLE): HResult;
begin
  try
    CheckDeleted;
    XRTLCheckOutArgument(pUpdateRate);
    XRTLCheckOutArgument(pActive);
    XRTLCheckOutArgument(ppName);
    XRTLCheckOutArgument(pTimeBias);
    XRTLCheckOutArgument(pPercentDeadband);
    XRTLCheckOutArgument(pLCID);
    XRTLCheckOutArgument(phClientGroup);
    XRTLCheckOutArgument(phServerGroup);
    try
      BeginRead;
      pUpdateRate:= FUpdateRate;
      pActive:= FActive;
      ppName:= XRTLAllocOutWideString(FName);
      pTimeBias:= FTimeBias;
      pPercentDeadband:= FPercentDeadband;
      pLCID:= FLocaleID;
      phClientGroup:= FClientHandle;
      phServerGroup:= ServerHandle;
      Result:= S_OK;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCGroupStateMgt_SetName(szName: POleStr): HResult;
var
  GrpUnk: IUnknown;
begin
  try
    CheckDeleted;
    XRTLCheckArgument(not WideSameStr('', szName));
    try
      BeginWrite;
      Result:= (Server as IOPCServer).GetGroupByName(szName, IID_IOPCItemMgt, GrpUnk);
      if Succeeded(Result) and Assigned(GrpUnk) then
        OLEError(OPC_E_DUPLICATENAME);
      FName:= WideString(szName);
      Result:= S_OK;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCGroupStateMgt_CloneGroup(szName: POleStr; const riid: TIID; out ppUnk: IInterface): HResult;
var
  LServerHandle: OPCHANDLE;
  LRevisedUpdateRate: DWORD;
  I: Cardinal;
  Group: IOPCItemMgt;
  EnumUnk: IUnknown;
  Enum: IEnumOPCItemAttributes;
  ItemAttributes: POPCITEMATTRIBUTESARRAY;
  ItemDef: OPCITEMDEF;
  ItemResult: POPCITEMRESULTARRAY;
  ItemErrors: PResultList;
begin
  try
    CheckDeleted;
    try
      BeginRead;
      OLECheck((Server as IOPCServer).AddGroup(szName, FALSE, UpdateRate, ClientHandle, @TimeBias,
                                               @PercentDeadband, FLocaleID, LServerHandle, LRevisedUpdateRate, riid, ppUnk));
      OLECheck(IOPCItemMgt_CreateEnumerator(IEnumOPCItemAttributes, EnumUnk));
      Enum:= EnumUnk as IEnumOPCItemAttributes;
      Group:= ppUnk as IOPCItemMgt;
      while Succeeded(Enum.Next(1, ItemAttributes, I)) and (I > 0) do
      begin
        ItemDef.szAccessPath:= ItemAttributes[0].szAccessPath;
        ItemDef.szItemID:=     ItemAttributes[0].szItemID;
        ItemDef.bActive:=      ItemAttributes[0].bActive;
        ItemDef.hClient:=      ItemAttributes[0].hClient;
        ItemDef.dwBlobSize:=   ItemAttributes[0].dwBlobSize;
        if ItemDef.dwBlobSize > 0 then
        begin
          ItemDef.pBlob:= CoTaskMemAlloc(ItemDef.dwBlobSize);
          XRTLMoveMemory(ItemAttributes[0].pBlob, ItemDef.pBlob, ItemDef.dwBlobSize);
        end
        else
          ItemDef.pBlob:= nil;
        ItemDef.vtRequestedDataType:= ItemAttributes[0].vtRequestedDataType;
        ItemDef.wReserved:= 0;
        try
          OLECheck(Group.AddItems(1, @ItemDef, ItemResult, ItemErrors));
        finally
          XRTLFreeOPCITEMRESULTARRAY(ItemResult, 1);
          XRTLFreeResultList(ItemErrors);
          XRTLFreeOPCITEMATTRIBUTESARRAY(ItemAttributes, 1);
        end;
      end;
      Result:= S_OK;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_AddItems(dwCount: DWORD;
  pItemArray: POPCITEMDEFARRAY; out ppAddResults: POPCITEMRESULTARRAY;
  out ppErrors: PResultList): HResult;
var
  I: DWORD;
  Item: IXRTLOPCDANameSpaceItem;
  GroupItem: TXRTLOPCDAGroupItem;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppAddResults);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginWrite;
      NameSpaceLock.BeginRead;
      try
        OLECheck(IOPCItemMgt_ValidateItems(dwCount, pItemArray, False, ppAddResults, ppErrors));
        Result:= S_OK;
        for I:= 0 to dwCount - 1 do
        begin
          GroupItem:= nil;
          try
            OLECheck(ppErrors[I]);
            OLECheck(NameSpace.GetItem(WideString(pItemArray[I].szItemID), Item));
            GroupItem:= TXRTLOPCDAGroupItem.Create;
            GroupItem.Active:= pItemArray[I].bActive;
            GroupItem.ItemID:= pItemArray[I].szItemID;
            GroupItem.ServerHandle:= GetItemUniqueId;
            GroupItem.ClientHandle:= pItemArray[I].hClient;
            OLECheck(Item.GetDataType(GroupItem.CanonicalDataType));
            GroupItem.RequestedDataType:= pItemArray[I].vtRequestedDataType;
            GroupItem.AccessRights:= ppAddResults[I].dwAccessRights;
            ppAddResults[I].hServer:= GroupItem.ServerHandle;
            AddItem(GroupItem);
            ppErrors[I]:= S_OK;
          except
            FreeAndNil(GroupItem);
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeOPCITEMRESULTARRAY(ppAddResults, dwCount);
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_ValidateItems(dwCount: DWORD;
  pItemArray: POPCITEMDEFARRAY; bBlobUpdate: BOOL;
  out ppValidationResults: POPCITEMRESULTARRAY;
  out ppErrors: PResultList): HResult;
var
  I: DWORD;
  Item: IXRTLOPCDANameSpaceItem;
  bResult: BOOL;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppValidationResults);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginRead;
      NameSpaceLock.BeginRead;
      try
        ppValidationResults:= POPCITEMRESULTARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCITEMRESULT)));
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppValidationResults) or not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppValidationResults, dwCount * SizeOf(OPCITEMRESULT));
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        Result:= S_OK;
        for I:= 0 to dwCount - 1 do
        begin
          try
            OLECheck(NameSpace.GetItem(WideString(pItemArray[I].szItemID), Item));
            OLECheck(Item.GetDataType(ppValidationResults[I].vtCanonicalDataType));
            OLECheck(Item.GetAccessRights(ppValidationResults[I].dwAccessRights));
            OLECheck(XRTLOPCDAVariantManager.CanConvert(ppValidationResults[I].vtCanonicalDataType,
                                                        pItemArray[I].vtRequestedDataType, bResult));
            if not bResult then
              OLEError(OPC_E_BADTYPE);
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeOPCITEMRESULTARRAY(ppValidationResults, dwCount);
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_RemoveItems(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; out ppErrors: PResultList): HResult;
var
  I: DWORD;
  GroupItem: TXRTLOPCDAGroupItem;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginWrite;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            RemoveItem(GroupItem);
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_SetActiveState(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; bActive: BOOL;
  out ppErrors: PResultList): HResult;
var
  I: DWORD;
  GroupItem: TXRTLOPCDAGroupItem;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginWrite;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            ActivateItem(GroupItem, bActive);
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_SetClientHandles(dwCount: DWORD;
  phServer, phClient: POPCHANDLEARRAY; out ppErrors: PResultList): HResult;
var
  I: DWORD;
  GroupItem: TXRTLOPCDAGroupItem;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginWrite;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            GroupItem.ClientHandle:= phClient[I];
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_SetDatatypes(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; pRequestedDatatypes: PVarTypeList;
  out ppErrors: PResultList): HResult;
var
  I: DWORD;
  GroupItem: TXRTLOPCDAGroupItem;
  bResult: BOOL;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginWrite;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            OLECheck(XRTLOPCDAVariantManager.CanConvert(GroupItem.CanonicalDataType,
                                                        pRequestedDatatypes[I], bResult));
            if not bResult then
              OLEError(OPC_E_BADTYPE);
            GroupItem.RequestedDataType:= pRequestedDatatypes[I];
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCItemMgt_CreateEnumerator(const riid: TIID;
  out ppUnk: IInterface): HResult;
var
  Enum: TXRTLEnumOPCItemAttributes;
  I: Integer;
  GroupItem: TXRTLOPCDAGroupItem;
  LItems: TXRTLValueArray;
begin
  SetLength(LItems, 0);
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppUnk);
    if not IsEqualIID(riid, IEnumOPCItemAttributes) then
      OLEError(E_NOINTERFACE);
    try
      try
        BeginRead;
        Enum:= TXRTLEnumOPCItemAttributes.Create;
        ppUnk:= Enum;
        LItems:= FItems.GetValues;
        for I:= 0 to Length(LItems) - 1 do
        begin
          XRTLGetAsObject(LItems[I], GroupItem);
          Enum.Add(GroupItem);
        end;
        LItems:= FActiveItems.GetValues;
        for I:= 0 to Length(LItems) - 1 do
        begin
          XRTLGetAsObject(LItems[I], GroupItem);
          Enum.Add(GroupItem);
        end;
        if Enum.Count = 0 then
          Result:= S_FALSE
        else
          Result:= S_OK;
      finally
        EndRead;
      end;
    except
      ppUnk:= nil;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCSyncIO_Read(dwSource: OPCDATASOURCE;
  dwCount: DWORD; phServer: POPCHANDLEARRAY;
  out ppItemValues: POPCITEMSTATEARRAY;
  out ppErrors: PResultList): HResult;
var
  I: Cardinal;
  GroupItem: TXRTLOPCDAGroupItem;
  Item: IXRTLOPCDANameSpaceItem;
  DataSource: IXRTLOPCDADataSource;
  bResult: BOOL;
  Value: OLEVariant;
  vtDataType: DWORD;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppItemValues);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    XRTLCheckArgument(dwSource in [OPC_DS_CACHE, OPC_DS_DEVICE]);
    try
      BeginRead;
      NameSpaceLock.BeginRead;
      try
        if dwSource = OPC_DS_CACHE then
          DataSource:= Cache;
        if dwSource = OPC_DS_DEVICE then
          DataSource:= Device;
        if not Assigned(DataSource) then
          OLEError(E_FAIL);
        ppItemValues:= POPCITEMSTATEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCITEMSTATE)));
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppItemValues) or not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppItemValues, dwCount * SizeOf(OPCITEMSTATE));
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        Result:= S_OK;
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            ppItemValues[I].hClient:= GroupItem.ClientHandle;
            OLECheck(NameSpace.GetItem(GroupItem.ItemID, Item));
            OLECheck(Item.CanRead(bResult));
            if not bResult then
              OLEError(OPC_E_BADRIGHTS);
            VarClear(Value);
            OLECheck(DataSource.Read(PWideChar(GroupItem.ItemID),
                                     Value,
                                     ppItemValues[I].wQuality,
                                     ppItemValues[I].ftTimeStamp));
            try
              if (DataSource <> Cache) and (Cache <> Device) then
              begin
                Cache.Write(PWideChar(GroupItem.ItemID), Value, ppItemValues[I].wQuality, ppItemValues[I].ftTimeStamp);
              end;
            except
            end;
            if ((not Active or not GroupItem.Active) and (dwSource = OPC_DS_CACHE)) or
               (XRTLOPCDAServerState = ssSuspended) or
               (VarType(Value) = VT_EMPTY) then
            begin
              ppItemValues[I].wQuality:= OPC_QUALITY_OUT_OF_SERVICE;
            end
            else
            begin
              vtDataType:= GroupItem.RequestedDataType;
              if vtDataType = VT_EMPTY then
                vtDataType:= GroupItem.CanonicalDataType;
              OLECheck(XRTLOPCDAVariantManager.Convert(ppItemValues[I].vDataValue, vtDataType, Value, FLocaleID));
            end;
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            ppItemValues[I].wQuality:= OPC_QUALITY_BAD;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeOPCITEMSTATEARRAY(ppItemValues, dwCount);
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCSyncIO_Write(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; pItemValues: POleVariantArray;
  out ppErrors: PResultList): HResult;
var
  I: Cardinal;
  GroupItem: TXRTLOPCDAGroupItem;
  Item: IXRTLOPCDANameSpaceItem;
  DataSource: IXRTLOPCDADataSource;
  ftTimeStamp: TFileTime;
  bResult: BOOL;
  Value: OLEVariant;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    try
      BeginRead;
      NameSpaceLock.BeginRead;
      try
        DataSource:= Device;
        if not Assigned(DataSource) then
          OLEError(E_FAIL);
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        Result:= S_OK;
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            OLECheck(NameSpace.GetItem(GroupItem.ItemID, Item));
            OLECheck(Item.CanWrite(bResult));
            if not bResult then
              OLEError(OPC_E_BADRIGHTS);
            Int64(ftTimeStamp):= 0;
            VarClear(Value);
            OLECheck(XRTLOPCDAVariantManager.Convert(Value, GroupItem.CanonicalDataType, pItemValues[I], FLocaleID));
            OLECheck(DataSource.Write(PWideChar(GroupItem.ItemID),
                                      Value, OPC_QUALITY_GOOD, ftTimeStamp));
            ppErrors[I]:= S_OK;
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
      except
        XRTLFreeResultList(ppErrors);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_Read(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; dwTransactionID: DWORD;
  out pdwCancelID: DWORD; out ppErrors: PResultList): HResult;
var
  Operation: TXRTLOPCDA20GroupASyncRead;
  GroupItem: TXRTLOPCDAGroupItem;
  Item: IXRTLOPCDANameSpaceItem;
  I, dwASyncCount: DWORD;
  bResult: BOOL;
  lphServer: POPCHANDLEARRAY;
begin
  Operation:= nil;
  ppErrors:= nil;
  lphServer:= nil;
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(pdwCancelID);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    try
      BeginRead;
      NameSpaceLock.BeginRead;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        Result:= S_OK;
        dwASyncCount:= 0;
        for I:= 0 to dwCount - 1 do
        begin
          try
            ppErrors[I]:= S_OK;
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            OLECheck(NameSpace.GetItem(GroupItem.ItemID, Item));
            OLECheck(Item.CanRead(bResult));
            if not bResult then
              OLEError(OPC_E_BADRIGHTS);
            Inc(dwASyncCount);
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
        if dwASyncCount > 0 then
        begin
          lphServer:= POPCHANDLEARRAY(CoTaskMemAlloc(dwASyncCount * SizeOf(OPCHANDLE)));
          if not Assigned(lphServer) then
            OLEError(E_OUTOFMEMORY);
          ZeroMemory(lphServer, dwASyncCount * SizeOf(OPCHANDLE));
          Operation:= TXRTLOPCDA20GroupASyncRead.Create;
          Operation.phServer:= lphServer;
          Operation.dwCount:= dwASyncCount;
          Operation.Group:= Self;
          Operation.TransactionID:= dwTransactionID;
          Operation.CancelID:= GetCancelUniqueID;
          Operation.Source:= OPC_DS_DEVICE;
          pdwCancelID:= Operation.CancelID;
          dwASyncCount:= 0;
          for I:= 0 to dwCount - 1 do
          begin
            if Succeeded(ppErrors[I]) then
            begin
              Operation.phServer[dwASyncCount]:= phServer[I];
              Inc(dwASyncCount);
            end;
          end;
          FASyncQueue.SetValue(XRTLValue(pdwCancelID), XRTLValue(Operation));
          FASyncContext.Invoke(XRTLASyncInvokeable(InvokeOperation), XRTLValue(Operation));
        end;
      except
        FreeAndNil(Operation);
        XRTLFreeResultList(ppErrors);
        XRTLFreeOPCHANDLEARRAY(lphServer);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_Write(dwCount: DWORD;
  phServer: POPCHANDLEARRAY; pItemValues: POleVariantArray;
  dwTransactionID: DWORD; out pdwCancelID: DWORD;
  out ppErrors: PResultList): HResult;
var
  Operation: TXRTLOPCDA20GroupASyncWrite;
  GroupItem: TXRTLOPCDAGroupItem;
  Item: IXRTLOPCDANameSpaceItem;
  I, dwASyncCount: DWORD;
  bResult: BOOL;
  lphServer: POPCHANDLEARRAY;
  lpValues: POLEVariantArray;
begin
  Operation:= nil;
  ppErrors:= nil;
  lphServer:= nil;
  lpValues:= nil;
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(pdwCancelID);
    XRTLCheckOutArgument(ppErrors);
    XRTLCheckArgument(dwCount > 0);
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    try
      BeginRead;
      NameSpaceLock.BeginRead;
      try
        ppErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
        if not Assigned(ppErrors) then
          OLEError(E_OUTOFMEMORY);
        ZeroMemory(ppErrors, dwCount * SizeOf(HResult));
        Result:= S_OK;
        pdwCancelID:= 0;
        dwASyncCount:= 0;
        for I:= 0 to dwCount - 1 do
        begin
          try
            GroupItem:= GetItem(phServer[I]);
            if not Assigned(GroupItem) then
              OLEError(OPC_E_INVALIDHANDLE);
            OLECheck(NameSpace.GetItem(GroupItem.ItemID, Item));
            OLECheck(Item.CanWrite(bResult));
            if not bResult then
              OLEError(OPC_E_BADRIGHTS);
            Inc(dwASyncCount);
          except
            ppErrors[I]:= XRTLHandleCOMException;
            Result:= S_FALSE;
          end;
        end;
        if dwASyncCount > 0 then
        begin
          lphServer:= POPCHANDLEARRAY(CoTaskMemAlloc(dwASyncCount * SizeOf(OPCHANDLE)));
          lpValues:= POLEVariantARRAY(CoTaskMemAlloc(dwASyncCount * SizeOf(OLEVariant)));
          if not Assigned(lphServer) or not Assigned(lpValues) then
            OLEError(E_OUTOFMEMORY);
          ZeroMemory(lphServer, dwASyncCount * SizeOf(OPCHANDLE));
          ZeroMemory(lpValues, dwASyncCount * SizeOf(OLEVariant));
          Operation:= TXRTLOPCDA20GroupASyncWrite.Create;
          Operation.phServer:= lphServer;
          Operation.pItemValues:= lpValues;
          Operation.dwCount:= dwASyncCount;
          Operation.Group:= Self;
          Operation.TransactionID:= dwTransactionID;
          Operation.CancelID:= GetCancelUniqueID;
          Operation.Source:= OPC_DS_DEVICE;
          pdwCancelID:= Operation.CancelID;
          dwASyncCount:= 0;
          for I:= 0 to dwCount - 1 do
          begin
            if Succeeded(ppErrors[I]) then
            begin
              Operation.phServer[dwASyncCount]:= phServer[I];
              VarCopy(Operation.pItemValues[dwASyncCount], pItemValues[I]);
              Inc(dwASyncCount);
            end;
          end;
          FASyncQueue.SetValue(XRTLValue(pdwCancelID), XRTLValue(Operation));
          FASyncContext.Invoke(XRTLASyncInvokeable(InvokeOperation), XRTLValue(Operation));
        end;
      except
        FreeAndNil(Operation);
        XRTLFreeResultList(ppErrors);
        XRTLFreeOPCHANDLEARRAY(lphServer);
        XRTLFreeOleVariantArray(lpValues, dwCount);
        raise;
      end;
    finally
      NameSpaceLock.EndRead;
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_Refresh2(dwSource: OPCDATASOURCE;
  dwTransactionID: DWORD; out pdwCancelID: DWORD): HResult;
begin
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(pdwCancelID);
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    if not(FActive and HasActiveItems) then
      OLEError(E_FAIL);
    try
      BeginRead;
      OLECheck(EnqueueASyncRefresh(dwSource, dwTransactionID, pdwCancelID, False));
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_Cancel2(dwCancelID: DWORD): HResult;
var
  Operation: TXRTLOPCDA20GroupASyncOperation;
begin
  try
    CheckDeleted;
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    XRTLGetAsObjectDef(FASyncQueue.GetValue(XRTLValue(dwCancelID)), Operation, nil);
    if not Assigned(Operation) then
      OLEError(E_FAIL);
    Operation.Canceled:= True;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_SetEnable(bEnable: BOOL): HResult;
begin
  try
    CheckDeleted;
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    FASyncEnabled:= bEnable;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.IOPCASyncIO2_GetEnable(out pbEnable: BOOL): HResult;
begin
  try
    CheckDeleted;
    XRTLCheckOutArgument(pbEnable);
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    pbEnable:= FASyncEnabled;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.EnqueueASyncRefresh(dwSource: OPCDATASOURCE;
  dwTransactionID: DWORD; out pdwCancelID: DWORD; DoOnDataChange: Boolean): HResult;
var
  Operation: TXRTLOPCDA20GroupASyncRefresh;
begin
  Operation:= nil;
  Result:= S_OK;
  try
    CheckDeleted;
    XRTLCheckOutArgument(pdwCancelID);
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    if not(FActive and HasActiveItems) then
      OLEError(E_FAIL);
    try
      BeginRead;
      try
        Operation:= TXRTLOPCDA20GroupASyncRefresh.Create;
        Operation.Group:= Self;
        Operation.TransactionID:= dwTransactionID;
        Operation.CancelID:= GetCancelUniqueID;
        Operation.Source:= dwSource;
        pdwCancelID:= Operation.CancelID;
        if DoOnDataChange then
        begin
          try
            FLastScheduleTask.Cancel;
          except
          end;
          FLastScheduleTask:= nil;
          FOnDataChangeASyncContext.Invoke(XRTLASyncInvokeable(InvokeOnDataChange), XRTLValue(Operation));
        end
        else
        begin
          FASyncQueue.SetValue(XRTLValue(pdwCancelID), XRTLValue(Operation));
          FASyncContext.Invoke(XRTLASyncInvokeable(InvokeOperation), XRTLValue(Operation));
        end;
      except
        FreeAndNil(Operation);
        raise;
      end;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA20Group.EnqueueASyncRefresh(UpdateTimeStamp: Boolean): HResult;
var
  CancelId: DWORD;
begin
  Result:= EnqueueASyncRefresh(OPC_DS_DEVICE, 0, CancelId, UpdateTimeStamp);
end;

function TXRTLOPCDA20Group.EnqueueASyncOnDataChange: HResult;
var
  Operation: TXRTLOPCDA20GroupASyncDataChange;
  LTimeStamp: TXRTLTimeStamp;
begin
  Operation:= nil;
  Result:= S_OK;
  LTimeStamp:= nil;
  try
    CheckDeleted;
    if FClientCount = 0 then
      OLEError(CONNECT_E_NOCONNECTION);
    if not(FActive) then// and HasActiveItems) then
      OLEError(E_FAIL);
//    if not FASyncEnabled then
//      OLEError(E_FAIL);
    try
      BeginRead;
      LTimeStamp:= TXRTLTimeStamp.Create;
      try
        Operation:= TXRTLOPCDA20GroupASyncDataChange.Create;
        Operation.Group:= Self;
        Operation.TransactionID:= 0;
        Operation.CancelID:= GetCancelUniqueID;
        Operation.Source:= OPC_DS_DEVICE;
        FLastScheduleTask:= FOnDataChangeASyncContext.Schedule(XRTLASyncInvokeable(InvokeOnDataChange),
                              XRTLValue(Operation), XRTLASyncDelays([FUpdateRate]));
      except
        FreeAndNil(Operation);
        raise;
      end;
    finally
      FreeAndNil(LTimeStamp);
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCDA20Group.InvokeOperation(const ATask: IXRTLASyncTask);
var
  Operation: TXRTLOPCDA20GroupASyncOperation;
begin
  Operation:= nil;
  try
    XRTLGetAsObject(ATask.GetData, Operation);
    FASyncQueue.Remove(XRTLValue(Operation.CancelId));
    if not ATask.IsCanceled then
    begin
      Operation.Execute(FIOPCDataCallbackConnectionPoint);
    end;
  finally
    FreeAndNil(Operation);
  end;
end;

procedure TXRTLOPCDA20Group.InvokeOnDataChange(const ATask: IXRTLASyncTask);
var
  Operation: TXRTLOPCDA20GroupASyncOperation;
begin
  FLastScheduleTask:= nil;
  Operation:= nil;
  try
    XRTLGetAsObject(ATask.GetData, Operation);
    if not ATask.IsCanceled then
    begin
      if (FClientCount > 0) and FActive then
      begin
        if HasActiveItems and FASyncEnabled then
        begin
          Operation.Execute(FIOPCDataCallbackConnectionPoint);
//          FLastClientUpdate.SetCurrentTime;
        end;
        EnqueueASyncOnDataChange;
      end;
    end;
  finally
    FreeAndNil(Operation);
  end;
end;

initialization
begin
//  FASyncContext:= XRTLASyncContextManager.DefaultContext;
  FOnDataChangeASyncContext:= XRTLASyncContextManager.CreateSingleThreadContext(True);
//  FOnDataChangeASyncContext.Options:= [aoSync];
  FASyncContext:= FOnDataChangeASyncContext;
end;

end.

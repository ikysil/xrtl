unit xrtl_opc_sdk_OPCDAServer;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  Math,
  xrtl_util_TimeStamp, xrtl_util_Compat, 
  xrtl_util_COMUtils, xrtl_util_Lock,
  xrtl_util_Map, xrtl_util_Container, xrtl_util_Algorithm, xrtl_util_Array,
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types,
  xrtl_opc_sdk_OPCServer, xrtl_opc_sdk_OPCDANameSpace,
  xrtl_opc_sdk_OPCDADataSource,
  xrtl_opc_sdk_DA, xrtl_opc_sdk_OPCLocaleManager;

type
  TXRTLOPCDAServer    = class;
  TXRTLOPCDAGroup     = class;
  TXRTLOPCDAGroupItem = class;

  TXRTLOPCDAServerState = (ssRunning, ssFailed, ssNoConfig, ssSuspended, ssTest);

  TXRTLOPCDAServer = class(TXRTLOPCServer, IOPCServer, IXRTLOPCDAServer)
  private
    FStartTimeStamp: TXRTLTimeStamp;
    FMajorVersion: Integer;
    FBuildVersion: Integer;
    FMinorVersion: Integer;
    FVendorInfo: WideString;
    FPrivateGroups: TXRTLSynchronizedMap;
    function   GetGroupCount: Integer;
  protected
    function   CreateGroup: TXRTLOPCDAGroup; virtual; abstract;
  public
    procedure  Initialize; override;
    destructor Destroy; override;
    procedure  BeforeDestruction; override;
    property   VendorInfo: WideString read FVendorInfo write FVendorInfo;
    property   MajorVersion: Integer read FMajorVersion write FMajorVersion;
    property   MinorVersion: Integer read FMinorVersion write FMinorVersion;
    property   BuildVersion: Integer read FBuildVersion write FBuildVersion;
    property   GroupCount: Integer read GetGroupCount;
    function   IOPCServer.GetStatus             = IOPCServer_GetStatus;
    function   IOPCServer.GetErrorString        = IOPCServer_GetErrorString;
    function   IOPCServer.AddGroup              = IOPCServer_AddGroup;
    function   IOPCServer.GetGroupByName        = IOPCServer_GetGroupByName;
    function   IOPCServer.RemoveGroup           = IOPCServer_RemoveGroup;
    function   IOPCServer.CreateGroupEnumerator = IOPCServer_CreateGroupEnumerator;
    function   IOPCServer_GetStatus(out ppServerStatus: POPCSERVERSTATUS): HResult; stdcall;
    function   IOPCServer_GetErrorString(dwError: HResult; dwLocale: TLCID; out ppString: POleStr): HResult; stdcall;
    function   IOPCServer_AddGroup(szName: POleStr; bActive: BOOL; dwRequestedUpdateRate: DWORD;
                                   hClientGroup: OPCHANDLE; pTimeBias: PLongint;
                                   pPercentDeadband: PSingle; dwLCID: DWORD;
                                   out phServerGroup: OPCHANDLE; out pRevisedUpdateRate: DWORD;
                                   const riid: TIID; out ppUnk: IUnknown): HResult; stdcall;
    function   IOPCServer_GetGroupByName(szName: POleStr; const riid: TIID; out ppUnk: IUnknown): HResult; stdcall;
    function   IOPCServer_RemoveGroup(hServerGroup: OPCHANDLE; bForce: BOOL): HResult; stdcall;
    function   IOPCServer_CreateGroupEnumerator(dwScope: OPCENUMSCOPE; const riid: TIID;
                                                out ppUnk: IUnknown): HResult; stdcall;
    function   IXRTLOPCDAServer.GetNameSpace  = IXRTLOPCDAServer_GetNameSpace;
    function   IXRTLOPCDAServer.SetNameSpace  = IXRTLOPCDAServer_SetNameSpace;
    function   IXRTLOPCDAServer.GetDataSource = IXRTLOPCDAServer_GetDataSource;
    function   IXRTLOPCDAServer.SetDataSource = IXRTLOPCDAServer_SetDataSource;
    function   IXRTLOPCDAServer_GetNameSpace(out ANameSpace: IXRTLOPCDANameSpace): HResult; stdcall;
    function   IXRTLOPCDAServer_SetNameSpace(ANameSpace: IXRTLOPCDANameSpace): HResult; stdcall;
    function   IXRTLOPCDAServer_GetDataSource(dwSource: OPCDATASOURCE; out ADataSource: IXRTLOPCDADataSource): HResult; stdcall;
    function   IXRTLOPCDAServer_SetDataSource(dwSource: OPCDATASOURCE; ADataSource: IXRTLOPCDADataSource): HResult; stdcall;
  end;

  TXRTLOPCDAGroupItem = class
  public
    Active: BOOL;
    ItemID: WideString;
    ServerHandle: OPCHANDLE;
    ClientHandle: OPCHANDLE;
    CanonicalDataType: TVarType;
    RequestedDataType: TVarType;
    AccessRights: DWORD;
    constructor Create;
    destructor Destroy; override;
  end;

  TXRTLOPCDAGroup = class(TCOMObject, IUnknown,
                          IXRTLOPCDAGroup)
  private
    FLock: IXRTLReadWriteLock;
    function   GetNameSpaceLock: IXRTLReadWriteLock;
  protected
    FName: WideString;
    FLocaleID: TLCID;
    FUpdateRate: DWORD;
    FActive: BOOL;
    FTimeBias: LongInt;
    FPercentDeadband: Single;
    FClientHandle: OPCHANDLE;
    FASyncEnabled: BOOL;
    FLastClientUpdate: TXRTLTimeStamp;
    FItems: TXRTLArrayMap;
    FActiveItems: TXRTLArrayMap;
    FItemUniqueId: Integer;
    FCancelUniqueId: Integer;
    procedure  CheckDeleted;
    function   GetItemUniqueId: Integer;
    function   GetCancelUniqueId: Integer;
  public
    Deleted: Boolean;
    Server: TXRTLOPCDAServer;
    ServerHandle: OPCHANDLE;
    LocaleManager: IXRTLOPCLocaleManager;
    NameSpace: IXRTLOPCDANameSpace;
    Cache: IXRTLOPCDADataSource;
    Device: IXRTLOPCDADataSource;
    constructor Create;
    destructor Destroy; override;
    procedure  BeforeDestruction; override;
    procedure  AddItem(GroupItem: TXRTLOPCDAGroupItem);
    procedure  RemoveItem(GroupItem: TXRTLOPCDAGroupItem);
    procedure  ActivateItem(GroupItem: TXRTLOPCDAGroupItem; bActive: BOOL);
    function   GetItem(const ItemServerHandle: OPCHANDLE): TXRTLOPCDAGroupItem;
    function   HasActiveItems: Boolean;
    function   GetActiveItems: TXRTLArray;
    property   Items: TXRTLArrayMap read FItems;
    property   ActiveItems: TXRTLArrayMap read FActiveItems;
    property   Name: WideString read FName write FName;
    property   LocaleID: TLCID read FLocaleID write FLocaleID;
    property   UpdateRate: DWORD read FUpdateRate write FUpdateRate;
    property   Active: BOOL read FActive write FActive;
    property   ASyncEnabled: BOOL read FASyncEnabled write FASyncEnabled;
    property   TimeBias: LongInt read FTimeBias write FTimeBias;
    property   PercentDeadband: Single read FPercentDeadband write FPercentDeadband;
    property   ClientHandle: OPCHANDLE read FClientHandle write FClientHandle;
    property   LastClientUpdate: TXRTLTimeStamp read FLastClientUpdate;
    property   NameSpaceLock: IXRTLReadWriteLock read GetNameSpaceLock;
    procedure  BeginRead;
    procedure  EndRead;
    procedure  BeginWrite;
    procedure  EndWrite;
    function   SetDeleted(bFlag: BOOL): HResult; stdcall;
    function   GetDeleted(out bFLag: BOOL): HResult; stdcall;
    function   IUnknown._AddRef  = _AddRef;
    function   IUnknown._Release = _Release;
    function   _AddRef: LongInt; stdcall;
    function   _Release: LongInt; stdcall;
  end;

procedure XRTLCheckOPCDAServerState;

var
  XRTLOPCDAServerState: TXRTLOPCDAServerState = ssRunning;

implementation

uses
  xrtl_util_Value,
  xrtl_opc_Utils;

var
  GroupUniqueId: Integer  = 0;
  GroupUniqueIdLock: IXRTLReadWriteLock = nil;

function GetGroupServerHandle: OPCHANDLE;
begin
  try
    GroupUniqueIdLock.BeginWrite;
    Inc(GroupUniqueId);
    Result:= GroupUniqueId;
  finally
    GroupUniqueIdLock.EndWrite;
  end;
end;

procedure XRTLCheckOPCDAServerState;
begin
  if XRTLOPCDAServerState = ssFailed then
    OLEError(E_FAIL);
end;

{ TXRTLOPCDAServer }

procedure TXRTLOPCDAServer.Initialize;
begin
  inherited;
  FStartTimeStamp:= TXRTLTimeStamp.Create;
  FVendorInfo:= ClassName;
  FMajorVersion:= 1;
  FMinorVersion:= 0;
  FBuildVersion:= 0;
  FPrivateGroups:= TXRTLSynchronizedMap.Create(TXRTLArrayMap.Create);
end;

destructor TXRTLOPCDAServer.Destroy;
begin
  FreeAndNil(FPrivateGroups);
  FreeAndNil(FStartTimeStamp);
  inherited;
end;

procedure TXRTLOPCDAServer.BeforeDestruction;
begin
  FPrivateGroups.Clear;
  inherited;
end;

function TXRTLOPCDAServer.GetGroupCount: Integer;
begin
  try
    FPrivateGroups.BeginRead;
    Result:= (FPrivateGroups.CoreMap as TXRTLArrayMap).GetSize;
  finally
    FPrivateGroups.EndRead;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_GetStatus(out ppServerStatus: POPCSERVERSTATUS): HResult;
const
  ServerStateMap: array[TXRTLOPCDAServerState] of OPCSERVERSTATE = (
    OPC_STATUS_RUNNING, OPC_STATUS_FAILED, OPC_STATUS_NOCONFIG,
    OPC_STATUS_SUSPENDED, OPC_STATUS_TEST
  );

  function GetLastUpdateTime: TFileTime;
  var
    I: Integer;
    Group: TXRTLOPCDAGroup;
    Groups: TXRTLValueArray;
    LResult: Int64;
  begin
    try
      BeginRead;
      Groups:= FPrivateGroups.GetValues;
      LResult:= -1;
      for I:= 0 to Length(Groups) - 1 do
      begin
        XRTLGetAsObject(Groups[I], Group);
        LResult:= Max(LResult, Int64(Group.LastClientUpdate.UTCFileTime));
      end;
      Result:= TFileTime(LResult);
    finally
      EndRead;
    end;
  end;

begin
  try
    XRTLCheckOutArgument(ppServerStatus);
    try
      ppServerStatus:= POPCSERVERSTATUS(CoTaskMemAlloc(SizeOf(OPCSERVERSTATUS)));
      if ppServerStatus = nil then
        OLEError(E_OUTOFMEMORY);
      ZeroMemory(ppServerStatus, SizeOf(OPCSERVERSTATUS));
      ppServerStatus.ftStartTime:= FStartTimeStamp.UTCFileTime;
      GetSystemTimeAsFileTime(ppServerStatus.ftCurrentTime);
      ppServerStatus.ftLastUpdateTime:= GetLastUpdateTime;
      ppServerStatus.dwServerState:= ServerStateMap[XRTLOPCDAServerState];
      ppServerStatus.dwGroupCount:= GroupCount;
      ppServerStatus.dwBandWidth:= $FFFFFFFF;
      ppServerStatus.wMajorVersion:= FMajorVersion;
      ppServerStatus.wMinorVersion:= FMinorVersion;
      ppServerStatus.wBuildNumber:= FBuildVersion;
      ppServerStatus.szVendorInfo:= XRTLAllocOutWideString(FVendorInfo);
      Result:= S_OK;
    except
      XRTLFreeOPCSERVERSTATUS(ppServerStatus);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_GetErrorString(dwError: HResult; dwLocale: TLCID; out ppString: POleStr): HResult;
begin
  Result:= S_OK;
  try
    OLECheck(FLocaleManager.GetErrorString(dwError, dwLocale, ppString));
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_AddGroup(szName: POleStr; bActive: BOOL;
                                                dwRequestedUpdateRate: DWORD; hClientGroup: OPCHANDLE;
                                                pTimeBias: PLongint; pPercentDeadband: PSingle; dwLCID: DWORD;
                                                out phServerGroup: OPCHANDLE; out pRevisedUpdateRate: DWORD;
                                                const riid: TIID; out ppUnk: IInterface): HResult;
var
  Group: TXRTLOPCDAGroup;
  GroupUnk: IUnknown;
  GroupStateIntf: IOPCGroupStateMgt;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState ;
    XRTLCheckOutArgument(phServerGroup);
    XRTLCheckOutArgument(ppUnk);
    XRTLCheckOutArgument(pRevisedUpdateRate);
    try
      BeginWrite;
      phServerGroup:= 0;
      pRevisedUpdateRate:= 0;
      ppUnk:= nil;
      if Succeeded(IOPCServer_GetGroupByName(szName, RIID, GroupUnk)) and Assigned(GroupUnk) then
        OLEError(OPC_E_DUPLICATENAME);
      try
        Group:= TXRTLOPCDAGroup(CreateGroup);
        ppUnk:= Group as IUnknown;
        OLECheck(ppUnk.QueryInterface(RIID, GroupUnk));
        Group.Server:= Self;
        Group.LocaleManager:= LocaleManager;
        if not WideSameStr('', szName) then
          Group.Name:= szName;
        GroupStateIntf:= Group as IOPCGroupStateMgt;
        Result:= GroupStateIntf.SetState(@dwRequestedUpdateRate, pRevisedUpdateRate,
                                         @bActive, pTimeBias, pPercentDeadband,
                                         @dwLCID, @hClientGroup);
        OLECheck(Result);
        Group._AddRef;
        FPrivateGroups.SetValue(XRTLValue(Group.ServerHandle), XRTLValue(Group, True));
        phServerGroup:= Group.ServerHandle;
      except
        ppUnk:= nil;
        raise;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_GetGroupByName(szName: POleStr;
                                                      const riid: TIID;
                                                      out ppUnk: IInterface): HResult;
var
  I: Integer;
  Group: TXRTLOPCDAGroup;
  LGroups: TXRTLValueArray;
begin
  SetLength(LGroups, 0);
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ppUnk);
    XRTLCheckArgument(not WideSameStr('', szName));
    try
      BeginRead;
      Result:= E_INVALIDARG;
      ppUnk:= nil;
      LGroups:= FPrivateGroups.GetValues;
      for I:= 0 to Length(LGroups) - 1 do
      begin
        XRTLGetAsObject(LGroups[I], Group);
        if Assigned(Group) and WideSameStr(Group.Name, szName) then
        begin
          Result:= (Group as IUnknown).QueryInterface(RIID, ppUnk);
          Exit;
        end;
      end;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_RemoveGroup(hServerGroup: OPCHANDLE; bForce: BOOL): HResult;
var
  Group: TXRTLOPCDAGroup;
  LGroupValue: IXRTLValue;
begin
  try
    XRTLCheckOPCDAServerState;
    try
      BeginWrite;
      LGroupValue:= FPrivateGroups.GetValue(XRTLValue(hServerGroup));
      XRTLCheckArgument(Assigned(LGroupValue));
      XRTLGetAsObject(LGroupValue, Group, True);
      FPrivateGroups.Remove(XRTLValue(hServerGroup));
      if Group.RefCount = 1 then
      begin
        Group._Release;
        Result:= S_OK;
      end
      else
      begin
        if bForce then
        begin
          Group.Free;
          Result:= S_OK;
        end
        else
        begin
          Group.SetDeleted(True);
          Group._Release;
          Result:= OPC_S_INUSE;
        end;
      end;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IOPCServer_CreateGroupEnumerator(dwScope: OPCENUMSCOPE;
                                                             const riid: TIID;
                                                             out ppUnk: IInterface): HResult;

  procedure EnumPublicGroupsAsUnknown(AList: TXRTLEnumUnknown);
  begin
  end;

  procedure EnumPrivateGroupsAsUnknown(AList: TXRTLEnumUnknown);
  var
    I: Integer;
    Group: TXRTLOPCDAGroup;
    Obj: Pointer;
    LGroups: TXRTLValueArray;
  begin
    LGroups:= FPrivateGroups.GetValues;
    for I:= 0 to Length(LGroups) - 1 do
    begin
      Obj:= nil;
      XRTLGetAsObject(LGroups[I], Group);
      if Assigned(Group) and TObject(Group).GetInterface(IUnknown, Obj) then
        AList.Add(Obj);
    end;
    SetLength(LGroups, 0);
  end;

  procedure EnumPublicGroupsAsString(AList: TXRTLEnumString);
  begin
  end;

  procedure EnumPrivateGroupsAsString(AList: TXRTLEnumString);
  var
    I: Integer;
    Group: TXRTLOPCDAGroup;
    LGroups: TXRTLValueArray;
  begin
    LGroups:= FPrivateGroups.GetValues;
    for I:= 0 to Length(LGroups) - 1 do
    begin
      XRTLGetAsObject(LGroups[I], Group);
      if Assigned(Group) then
        AList.Add(Group.Name);
    end;
  end;

var
  EnumString: TXRTLEnumString;
  EnumUnknown: TXRTLEnumUnknown;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ppUnk);
    XRTLCheckArgument(dwScope in [OPC_ENUM_PRIVATE_CONNECTIONS, OPC_ENUM_PRIVATE,
                                  OPC_ENUM_PUBLIC_CONNECTIONS, OPC_ENUM_PUBLIC,
                                  OPC_ENUM_ALL_CONNECTIONS, OPC_ENUM_ALL]);
    try
      if not IsEqualIID(riid, IEnumUnknown) and not IsEqualIID(riid, IEnumString) then
        OLEError(E_NOINTERFACE);
      try
        BeginRead;
        ppUnk:= nil;
        if IsEqualIID(riid, IEnumString) then
        begin
          EnumString:= TXRTLEnumString.Create;
          ppUnk:= EnumString;
          if dwScope in [OPC_ENUM_PRIVATE_CONNECTIONS, OPC_ENUM_PRIVATE,
                         OPC_ENUM_ALL_CONNECTIONS, OPC_ENUM_ALL] then
          begin
            EnumPrivateGroupsAsString(EnumString);
          end;
          if dwScope in [OPC_ENUM_PUBLIC_CONNECTIONS, OPC_ENUM_PUBLIC,
                         OPC_ENUM_ALL_CONNECTIONS, OPC_ENUM_ALL] then
          begin
            EnumPublicGroupsAsString(EnumString);
          end;
          if EnumString.Count > 0 then
            Result:= S_OK
          else
            Result:= S_FALSE;
        end;
        if IsEqualIID(riid, IEnumUnknown) then
        begin
          EnumUnknown:= TXRTLEnumUnknown.Create;
          ppUnk:= EnumUnknown;
          if dwScope in [OPC_ENUM_PRIVATE_CONNECTIONS, OPC_ENUM_PRIVATE,
                         OPC_ENUM_ALL_CONNECTIONS, OPC_ENUM_ALL] then
          begin
            EnumPrivateGroupsAsUnknown(EnumUnknown);
          end;
          if dwScope in [OPC_ENUM_PUBLIC_CONNECTIONS, OPC_ENUM_PUBLIC,
                         OPC_ENUM_ALL_CONNECTIONS, OPC_ENUM_ALL] then
          begin
            EnumPublicGroupsAsUnknown(EnumUnknown);
          end;
          if EnumUnknown.Count > 0 then
            Result:= S_OK
          else
            Result:= S_FALSE;
        end;
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

function TXRTLOPCDAServer.IXRTLOPCDAServer_GetNameSpace(out ANameSpace: IXRTLOPCDANameSpace): HResult;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ANameSpace);
    try
      try
        BeginRead;
        ANameSpace:= XRTLOPCDANameSpace;
        Result:= S_OK;
      finally
        EndRead;
      end;
    except
      ANameSpace:= nil;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IXRTLOPCDAServer_SetNameSpace(ANameSpace: IXRTLOPCDANameSpace): HResult;
begin
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckArgument(Assigned(ANameSpace));
    try
      BeginWrite;
      XRTLOPCDANameSpace:= ANameSpace;
      Result:= S_OK;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IXRTLOPCDAServer_GetDataSource(dwSource: OPCDATASOURCE; out ADataSource: IXRTLOPCDADataSource): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckOutArgument(ADataSource);
    XRTLCheckArgument(dwSource in [OPC_DS_CACHE, OPC_DS_DEVICE]);
    try
      BeginRead;
      if dwSource = OPC_DS_CACHE then
        ADataSource:= XRTLOPCDADataSourceCache;
      if dwSource = OPC_DS_DEVICE then
        ADataSource:= XRTLOPCDADataSourceDevice;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAServer.IXRTLOPCDAServer_SetDataSource(dwSource: OPCDATASOURCE; ADataSource: IXRTLOPCDADataSource): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckOPCDAServerState;
    XRTLCheckArgument(Assigned(ADataSource));
    XRTLCheckArgument(dwSource in [OPC_DS_CACHE, OPC_DS_DEVICE]);
    try
      BeginWrite;
      if dwSource = OPC_DS_CACHE then
        XRTLOPCDADataSourceCache:= ADataSource;
      if dwSource = OPC_DS_DEVICE then
        XRTLOPCDADataSourceDevice:= ADataSource;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

{ TXRTLOPCDAGroupItem }

constructor TXRTLOPCDAGroupItem.Create;
begin
  inherited;
end;

destructor TXRTLOPCDAGroupItem.Destroy;
begin
  inherited;
end;

{ TXRTLOPCDAGroup }

constructor TXRTLOPCDAGroup.Create;
var
  TZI: TTimeZoneInformation;
begin
  inherited;
  Deleted:= False;
  ServerHandle:= GetGroupServerHandle;
  FLock:= XRTLCreateReadWriteLock(XRTLCreateExclusiveLock);
  FItems:= TXRTLArrayMap.Create;
  FActiveItems:= TXRTLArrayMap.Create;
  FName:= ClassName + '_' + IntToHex(ServerHandle, 8);
  FLocaleID:= LOCALE_SYSTEM_DEFAULT;
  FUpdateRate:= 0;
  FActive:= False;
  GetTimeZoneInformation(TZI);
  FTimeBias:= TZI.Bias;
  FPercentDeadband:= 0;
  FClientHandle:= 0;
  FItemUniqueId:= 0;
  FCancelUniqueId:= 0;
  FLastClientUpdate:= TXRTLTimeStamp.Create;
  FASyncEnabled:= True;
  Server:= nil;
  LocaleManager:= nil;
  NameSpace:= XRTLOPCDANameSpace;
  Cache:=  XRTLOPCDADataSourceCache;
  Device:= XRTLOPCDADataSourceDevice;
end;

destructor TXRTLOPCDAGroup.Destroy;
begin
  Device:= nil;
  Cache:= nil;
  NameSpace:= nil;
  LocaleManager:= nil;
  Server:= nil;
  FreeAndNil(FLastClientUpdate);
  FreeAndNil(FActiveItems);
  FreeAndNil(FItems);
  inherited;
end;

procedure TXRTLOPCDAGroup.BeforeDestruction;
begin
  inherited;
end;

procedure TXRTLOPCDAGroup.CheckDeleted;
begin
  if Deleted then
    OLEError(E_FAIL);
end;

function TXRTLOPCDAGroup.GetItemUniqueId: Integer;
begin
  Inc(FItemUniqueId);
  Result:= FItemUniqueId;
end;

function TXRTLOPCDAGroup.GetCancelUniqueId: Integer;
begin
  Inc(FCancelUniqueId);
  Result:= FCancelUniqueId;
end;

procedure TXRTLOPCDAGroup.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TXRTLOPCDAGroup.EndRead;
begin
  FLock.EndRead;
end;

procedure TXRTLOPCDAGroup.BeginWrite;
begin
  FLock.BeginWrite;
end;

procedure TXRTLOPCDAGroup.EndWrite;
begin
  FLock.EndWrite;
end;

function TXRTLOPCDAGroup.GetDeleted(out bFLag: BOOL): HResult;
begin
  try
    try
      BeginRead;
      bFlag:= Deleted;
      Result:= S_OK;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAGroup.SetDeleted(bFlag: BOOL): HResult;
begin
  try
    try
      BeginRead;
      Deleted:= bFlag;
      Result:= S_OK;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAGroup._AddRef: LongInt;
begin
  Result:= inherited _AddRef;
end;

function TXRTLOPCDAGroup._Release: LongInt;
begin
  Result:= inherited _Release;
end;

function TXRTLOPCDAGroup.GetNameSpaceLock: IXRTLReadWriteLock;
begin
  Result:= NameSpace as IXRTLReadWriteLock;
end;

procedure TXRTLOPCDAGroup.AddItem(GroupItem: TXRTLOPCDAGroupItem);
var
  LContainer: TXRTLMap;
begin
  try
    BeginWrite;
    LContainer:= FItems;
    if GroupItem.Active then
      LContainer:= FActiveItems;
    LContainer.SetValue(XRTLValue(GroupItem.ServerHandle), XRTLValue(GroupItem, True));
  finally
    EndWrite;
  end;
end;

procedure TXRTLOPCDAGroup.RemoveItem(GroupItem: TXRTLOPCDAGroupItem);
var
  SHValue, LValue: IXRTLValue;
begin
  try
    BeginWrite;
    SHValue:= XRTLValue(GroupItem.ServerHandle);
    LValue:= FItems.Remove(SHValue);
    if not Assigned(LValue) then
      LValue:= FActiveItems.Remove(SHValue);
  finally
    EndWrite;
  end;
end;

procedure TXRTLOPCDAGroup.ActivateItem(GroupItem: TXRTLOPCDAGroupItem; bActive: BOOL);
var
  SHValue, LValue: IXRTLValue;
begin
  try
    BeginWrite;
    if GroupItem.Active <> bActive then
    begin
      SHValue:= XRTLValue(GroupItem.ServerHandle);
      if GroupItem.Active then
        LValue:= FActiveItems.Remove(SHValue)
      else
        LValue:= FItems.Remove(SHValue);
      GroupItem.Active:= bActive;
      if GroupItem.Active then
        FActiveItems.SetValue(SHValue, LValue)
      else
        FItems.SetValue(SHValue, LValue);
    end;
  finally
    EndWrite;
  end;
end;

function TXRTLOPCDAGroup.GetItem(const ItemServerHandle: OPCHANDLE): TXRTLOPCDAGroupItem;
var
  SHValue, LValue: IXRTLValue;
begin
  Result:= nil;
  try
    BeginRead;
    SHValue:= XRTLValue(ItemServerHandle);
    LValue:= FItems.GetValue(SHValue);
    if not Assigned(LValue) then
      LValue:= FActiveItems.GetValue(SHValue);
    if Assigned(LValue) then
      XRTLGetAsObject(LValue, Result);
  finally
    EndRead;
  end;
end;

function TXRTLOPCDAGroup.HasActiveItems: Boolean;
begin
  Result:= not FActiveItems.IsEmpty;
end;

function TXRTLOPCDAGroup.GetActiveItems: TXRTLArray;
var
  LItems: TXRTLValueArray;
begin
  try
    BeginRead;
    Result:= TXRTLArray.Create;
    LItems:= FActiveItems.GetValues;
    XRTLCopy(LItems, Result);
  finally
    EndRead;
  end;
end;

initialization
begin
  GroupUniqueIdLock:= XRTLCreateReadWriteLock(XRTLCreateExclusiveLock);
end;

end.

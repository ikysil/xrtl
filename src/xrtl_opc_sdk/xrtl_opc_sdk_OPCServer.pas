unit xrtl_opc_sdk_OPCServer;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Classes, COMObj, AxCtrls, ActiveX,
  xrtl_util_COMUtils, xrtl_util_Lock,
  xrtl_opc_Common, xrtl_opc_sdk_OPCLocaleManager;

type
  IXRTLOPCServer = interface
  ['{B0710D79-C715-485C-9482-FF6164D3AD08}']
    function   GetLocaleManager(out ALocaleManager: IXRTLOPCLocaleManager): HResult; stdcall;
    function   SetLocaleManager(ALocaleManager: IXRTLOPCLocaleManager): HResult; stdcall;
  end;

  TXRTLOPCServer = class(TCOMObject, IOPCCommon, IConnectionPointContainer,
                         IXRTLOPCServer)//, IXRTLSynchronizer)
  private
  protected
    FConnectionPoints: TConnectionPoints;
    FIOPCShutdownConnectionPoint: TConnectionPoint;
    FClientName: WideString;
    FLocaleID: TLCID;
    FLocaleManager: IXRTLOPCLocaleManager;
    FLock: IXRTLReadWriteLock;
  public
    procedure  Initialize; override;
    destructor Destroy; override;
    procedure  AfterConstruction; override;
    procedure  BeforeDestruction; override;
    function   IOPCCommon.SetClientName           = IOPCCommon_SetClientName;
    function   IOPCCommon.SetLocaleID             = IOPCCommon_SetLocaleID;
    function   IOPCCommon.GetLocaleID             = IOPCCommon_GetLocaleID;
    function   IOPCCommon.QueryAvailableLocaleIDs = IOPCCommon_QueryAvailableLocaleIDs;
    function   IOPCCommon.GetErrorString          = IOPCCommon_GetErrorString;
    function   IOPCCommon_SetClientName(szName: POleStr): HResult; stdcall;
    function   IOPCCommon_SetLocaleID(dwLCID: TLCID): HResult; stdcall;
    function   IOPCCommon_GetLocaleID(out pdwLCID: TLCID): HResult; stdcall;
    function   IOPCCommon_QueryAvailableLocaleIDs(out pdwCount: UINT; out pdwLcid: PLCIDARRAY): HResult; stdcall;
    function   IOPCCommon_GetErrorString(dwError: HResult; out ppString: POleStr): HResult; stdcall;
    function   IXRTLOPCServer.GetLocaleManager = IXRTLOPCServer_GetLocaleManager;
    function   IXRTLOPCServer.SetLocaleManager = IXRTLOPCServer_SetLocaleManager;
    function   IXRTLOPCServer_GetLocaleManager(out ALocaleManager: IXRTLOPCLocaleManager): HResult; stdcall;
    function   IXRTLOPCServer_SetLocaleManager(ALocaleManager: IXRTLOPCLocaleManager): HResult; stdcall;
    property   ConnectionPoints: TConnectionPoints read FConnectionPoints implements IConnectionPointContainer;
    property   IOPCShutdownConnectionPoint: TConnectionPoint read FIOPCShutdownConnectionPoint;
    property   ClientName: WideString read FClientName write FClientName;
    procedure  SendShutdownNotify(Reason: WideString);
    property   LocaleID: TLCID read FLocaleID write FLocaleID;
    property   LocaleManager: IXRTLOPCLocaleManager read FLocaleManager write FLocaleManager;
    procedure  BeginRead;
    procedure  EndRead;
    procedure  BeginWrite;
    procedure  EndWrite;
  end;

implementation

uses
  xrtl_opc_sdk_OPCServerList;

{ TXRTLOPCServer }

procedure TXRTLOPCServer.Initialize;
begin
  inherited;
  FLock:= XRTLCreateReadWriteLock(XRTLCreateExclusiveLock);
  FClientName:= '';
  FLocaleManager:= XRTLOPCLocaleManager;
  FLocaleID:= LOCALE_SYSTEM_DEFAULT;
  FConnectionPoints:= TConnectionPoints.Create(Self);
  FIOPCShutdownConnectionPoint:= FConnectionPoints.CreateConnectionPoint(IID_IOPCShutdown, ckMulti, nil);
end;

destructor TXRTLOPCServer.Destroy;
begin
  FreeAndNil(FConnectionPoints);
  inherited;
end;

procedure TXRTLOPCServer.AfterConstruction;
begin
  inherited;
  XRTLOPCServerList.Add(Self);
end;

procedure TXRTLOPCServer.BeforeDestruction;
begin
  XRTLOPCServerList.Remove(Self);
  inherited;
end;

function TXRTLOPCServer.IOPCCommon_SetClientName(szName: POleStr): HResult;
begin
  try
    FClientName:= szName;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCServer.SendShutdownNotify(Reason: WideString);
var
  I: Integer;
  Client: IOPCShutdown;
  LReason: POLEStr;
begin
  if Assigned(FIOPCShutdownConnectionPoint) then
  begin
    LReason:= nil;
    try
      LReason:= XRTLAllocOutWideString(Reason);
      for I:= FIOPCShutdownConnectionPoint.SinkList.Count - 1 downto 0 do
      begin
        try
          Client:= IUnknown(FIOPCShutdownConnectionPoint.SinkList[I]) as IOPCShutdown;
          Client.ShutdownRequest(LReason);
        except
        end;
      end;
    finally
      XRTLFreeOutWideString(LReason);
    end;
  end;
end;

function TXRTLOPCServer.IOPCCommon_GetLocaleID(out pdwLCID: TLCID): HResult;
begin
  try
    pdwLCID:= FLocaleID;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCServer.IOPCCommon_SetLocaleID(dwLCID: TLCID): HResult;
var
  bResult: BOOL;
begin
  try
    if dwLCID = 0 then
      dwLCID:= LOCALE_SYSTEM_DEFAULT;
    OleCheck(FLocaleManager.IsLocaleIDAvailable(dwLCID, bResult));
    XRTLCheckArgument(bResult);
    FLocaleID:= dwLCID;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCServer.IOPCCommon_QueryAvailableLocaleIDs(out pdwCount: UINT; out pdwLCID: PLCIDARRAY): HResult;
begin
  try
    Result:= FLocaleManager.QueryAvailableLocaleIDs(pdwCount, pdwLCID);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCServer.IOPCCommon_GetErrorString(dwError: HResult; out ppString: POleStr): HResult;
begin
  try
    Result:= FLocaleManager.GetErrorString(dwError, FLocaleID, ppString);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCServer.IXRTLOPCServer_GetLocaleManager(out ALocaleManager: IXRTLOPCLocaleManager): HResult;
begin
  try
    XRTLCheckOutArgument(ALocaleManager);
    ALocaleManager:= FLocaleManager;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCServer.IXRTLOPCServer_SetLocaleManager(ALocaleManager: IXRTLOPCLocaleManager): HResult;
begin
  try
    XRTLCheckArgument(Assigned(ALocaleManager));
    FLocaleManager:= ALocaleManager;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCServer.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TXRTLOPCServer.EndRead;
begin
  FLock.EndRead;
end;

procedure TXRTLOPCServer.BeginWrite;
begin
  FLock.BeginWrite;
end;

procedure TXRTLOPCServer.EndWrite;
begin
  FLock.EndWrite;
end;

end.

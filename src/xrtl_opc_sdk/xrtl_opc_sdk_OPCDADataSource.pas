unit xrtl_opc_sdk_OPCDADataSource;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, ActiveX, 
  xrtl_util_COMUtils, xrtl_util_Lock,
  xrtl_opc_sdk_DA;

type
  TXRTLOPCDADataSource = class(TInterfacedObject, IXRTLOPCDADataSource)
  private
    FLock: IXRTLReadWriteLock;
  protected
    procedure  DoUpdate(ADataSource: IXRTLOPCDADataSource); virtual;
  public
    constructor Create;
    procedure  BeginRead;
    procedure  EndRead;
    procedure  BeginWrite;
    procedure  EndWrite;
    function   Read(szItemID: POLEStr; out Value: OleVariant; out Quality: Word; out TimeStamp: TFileTime): HResult; virtual; stdcall; abstract;
    function   Write(szItemID: POLEStr; Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult; virtual; stdcall; abstract;
    function   CreateItemEnumerator(out Enum: IEnumString): HResult; virtual; stdcall;
    function   Update(ADataSource: IXRTLOPCDADataSource): HResult; virtual; stdcall;
    function   AddItem(szItemID: POleStr): HResult; virtual; stdcall;
    function   RemoveItem(szItemID: POleStr): HResult; virtual; stdcall;
    function   ClearItems: HResult; virtual; stdcall;
  end;

var
  XRTLOPCDADataSourceCache:  IXRTLOPCDADataSource = nil;
  XRTLOPCDADataSourceDevice: IXRTLOPCDADataSource = nil;

procedure XRTLOPCDAUpdateCache;

implementation

procedure XRTLOPCDAUpdateCache;
begin
  if Assigned(XRTLOPCDADataSourceCache) then
    XRTLOPCDADataSourceCache.Update(XRTLOPCDADataSourceDevice);
end;

{ TXRTLOPCDADataSource }

constructor TXRTLOPCDADataSource.Create;
begin
  inherited;
  FLock:= XRTLCreateReadWriteLock;
end;

procedure TXRTLOPCDADataSource.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TXRTLOPCDADataSource.EndRead;
begin
  FLock.EndRead;
end;

procedure TXRTLOPCDADataSource.BeginWrite;
begin
  FLock.BeginWrite;
end;

procedure TXRTLOPCDADataSource.EndWrite;
begin
  FLock.EndWrite;
end;

function TXRTLOPCDADataSource.CreateItemEnumerator(out Enum: IEnumString): HResult;
begin
  try
    XRTLCheckOutArgument(Enum);
    Enum:= TXRTLEnumString.Create;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSource.Update(ADataSource: IXRTLOPCDADataSource): HResult;
begin
  Result:= S_OK;
  try
    XRTLCheckArgument(Assigned(ADataSource));
    XRTLCheckArgument(ADataSource <> Self as IXRTLOPCDADataSource);
    DoUpdate(ADataSource);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCDADataSource.DoUpdate(ADataSource: IXRTLOPCDADataSource);
begin
end;

function TXRTLOPCDADataSource.AddItem(szItemID: POleStr): HResult;
begin
  try
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSource.RemoveItem(szItemID: POleStr): HResult;
begin
  try
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSource.ClearItems: HResult;
begin
  try
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

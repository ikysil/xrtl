unit xrtl_opc_sdk_OPCServerList;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, 
  xrtl_util_Value, xrtl_util_Set,
  xrtl_opc_sdk_OPCServer;

type
  TXRTLOPCServerList = class
  private
    FList: TXRTLArraySet;
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SendShutdownNotify(Reason: WideString);
    procedure  BeginRead;
    procedure  EndRead;
    procedure  BeginWrite;
    procedure  EndWrite;
    procedure  Add(const OPCServer: TXRTLOPCServer);
    procedure  Remove(const OPCServer: TXRTLOPCServer);
  end;

var
  XRTLOPCServerList: TXRTLOPCServerList = nil;

implementation

{ TXRTLOPCServerList }

constructor TXRTLOPCServerList.Create;
begin
  inherited;
  FLock:= TMultiReadExclusiveWriteSynchronizer.Create;
  FList:= TXRTLArraySet.Create;
end;

destructor TXRTLOPCServerList.Destroy;
begin
  SendShutdownNotify('');
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TXRTLOPCServerList.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TXRTLOPCServerList.EndRead;
begin
  FLock.EndRead;
end;

procedure TXRTLOPCServerList.BeginWrite;
begin
  FLock.BeginWrite;
end;

procedure TXRTLOPCServerList.EndWrite;
begin
  FLock.EndWrite;
end;

procedure TXRTLOPCServerList.SendShutdownNotify(Reason: WideString);
var
  Server: TXRTLOPCServer;
  LServers: TXRTLValueArray;
  I: Integer;
begin
  SetLength(LServers, 0);
  try
    BeginRead;
    LServers:= FList.GetValues;
  finally
    EndRead;
  end;
  for I:= 0 to Length(LServers) - 1 do
  begin
    try
      Server:= XRTLGetAsPointer(LServers[I]);
      Server.SendShutdownNotify(Reason);
    except
    end;
  end;
end;

procedure TXRTLOPCServerList.Add(const OPCServer: TXRTLOPCServer);
begin
  try
    BeginWrite;
    FList.Add(XRTLValue(Pointer(OPCServer)));
  finally
    EndWrite;
  end;
end;

procedure TXRTLOPCServerList.Remove(const OPCServer: TXRTLOPCServer);
begin
  try
    BeginWrite;
    FList.Remove(XRTLValue(Pointer(OPCServer)));
  finally
    EndWrite;
  end;
end;

initialization
begin
  XRTLOPCServerList:= TXRTLOPCServerList.Create;
end;

finalization
begin
  FreeAndNil(XRTLOPCServerList);
end;

end.

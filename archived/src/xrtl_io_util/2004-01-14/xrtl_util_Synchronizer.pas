unit xrtl_util_Synchronizer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, SyncObjs;

type
  IXRTLSynchronizer = interface
  ['{3C206360-1BA9-4537-821E-A9D9002E7463}']
    function   BeginRead: HResult; stdcall;
    function   EndRead: HResult; stdcall;
//Returns: S_OK    if protected memory has not be written to by another thread
//         S_FALSE if another thread may have modified protected memory.
    function   BeginWrite: HResult; stdcall;
    function   EndWrite: HResult; stdcall;
  end;

  TXRTLSynchronizer = class(TInterfacedObject, IXRTLSynchronizer)
  private
  public
    function   BeginRead: HResult; stdcall;
    function   EndRead: HResult; stdcall;
    function   BeginWrite: HResult; stdcall;
    function   EndWrite: HResult; stdcall;
  end;

  TXRTLMultiReadExclusiveWriteSynchronizer = class(TXRTLSynchronizer, IXRTLSynchronizer)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    function   BeginRead: HResult; stdcall;
    function   EndRead: HResult; stdcall;
    function   BeginWrite: HResult; stdcall;
    function   EndWrite: HResult; stdcall;
  end;

  TXRTLCriticalSectionSynchronizer = class(TXRTLSynchronizer, IXRTLSynchronizer)
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function   BeginRead: HResult; stdcall;
    function   EndRead: HResult; stdcall;
    function   BeginWrite: HResult; stdcall;
    function   EndWrite: HResult; stdcall;
  end;

implementation

uses
  xrtl_util_COMUtils;

{ TXRTLSynchronizer }

function TXRTLSynchronizer.BeginRead: HResult;
begin
  Result:= E_NOTIMPL;
end;

function TXRTLSynchronizer.EndRead: HResult;
begin
  Result:= E_NOTIMPL;
end;

function TXRTLSynchronizer.BeginWrite: HResult;
begin
  Result:= E_NOTIMPL;
end;

function TXRTLSynchronizer.EndWrite: HResult;
begin
  Result:= E_NOTIMPL;
end;

{ TXRTLMultiReadExclusiveWriteSynchronizer }

constructor TXRTLMultiReadExclusiveWriteSynchronizer.Create;
begin
  inherited;
  FLock:= TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TXRTLMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TXRTLMultiReadExclusiveWriteSynchronizer.BeginRead: HResult;
begin
  try
    FLock.BeginRead;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLMultiReadExclusiveWriteSynchronizer.EndRead: HResult;
begin
  try
    FLock.EndRead;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLMultiReadExclusiveWriteSynchronizer.BeginWrite: HResult;
begin
  try
    if FLock.BeginWrite then
      Result:= S_OK
    else
      Result:= S_FALSE;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLMultiReadExclusiveWriteSynchronizer.EndWrite: HResult;
begin
  try
    FLock.EndWrite;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

{ TXRTLCriticalSectionSynchronizer }

constructor TXRTLCriticalSectionSynchronizer.Create;
begin
  inherited;
  FLock:= TCriticalSection.Create;
end;

destructor TXRTLCriticalSectionSynchronizer.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TXRTLCriticalSectionSynchronizer.BeginRead: HResult;
begin
  try
    FLock.Enter;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLCriticalSectionSynchronizer.EndRead: HResult;
begin
  try
    FLock.Leave;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLCriticalSectionSynchronizer.BeginWrite: HResult;
begin
  try
    FLock.Enter;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLCriticalSectionSynchronizer.EndWrite: HResult;
begin
  try
    FLock.Leave;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

unit xrtl_util_async_Context;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  Classes,
  xrtl_util_async_Core;

type
  TXRTLASyncSingleThreadContext = class(TXRTLASyncContextBase)
  private
    FThread: TThread;
  protected
    procedure  OnInvoke(ATask: IXRTLASyncTask); override;
    procedure  OnIdle(AThread: TThread); override;
  public
    constructor Create;
  end;

  TXRTLASyncMultiThreadContext = class(TXRTLASyncMultiThreadContextBase)
  private
    FNextCPUIndex: TXRTLCPUIndex;
  protected
    procedure  OnInvoke(ATask: IXRTLASyncTask); override;
    procedure  OnIdle(AThread: TThread); override;
    procedure  OnThreadCreate(AThread: TThread);
    procedure  OnThreadDestroy(AThread: TThread);
    procedure  CheckThreadCountOnInvoke;
    procedure  CheckThreadCountOnIdle(AThread: TThread);
    function   GetNextCPUIndex(var CPUIndex: TXRTLCPUIndex): Boolean;
  public
    constructor Create(const AContextAffinityMask: Cardinal); override;
  end;

implementation

uses
  xrtl_util_Type, xrtl_util_Lock, xrtl_util_Compat,
  xrtl_util_async_Thread;

{ TXRTLASyncMultiThreadContext }

constructor TXRTLASyncMultiThreadContext.Create(const AContextAffinityMask: Cardinal);
begin
  inherited Create(AContextAffinityMask);
  FNextCPUIndex:= 0;
end;

procedure TXRTLASyncMultiThreadContext.OnThreadCreate(AThread: TThread);
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  InterlockedIncrement(FThreadTotalCount);
  if AThread is TXRTLASyncCPUBoundInvokeThread then
    InterlockedIncrement(FThreadPerCPUCount[(AThread as TXRTLASyncCPUBoundInvokeThread).CPUIndex]);
end;

procedure TXRTLASyncMultiThreadContext.OnThreadDestroy(AThread: TThread);
var
  LThread: TXRTLASyncCPUBoundInvokeThread;
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  if XRTLIsAs(AThread, TXRTLASyncCPUBoundInvokeThread, LThread) then
    InterlockedDecrement(FThreadPerCPUCount[LThread.CPUIndex]);
  InterlockedDecrement(FThreadTotalCount);
end;

procedure TXRTLASyncMultiThreadContext.OnInvoke(ATask: IXRTLASyncTask);
begin
  CheckThreadCountOnInvoke;
end;

procedure TXRTLASyncMultiThreadContext.OnIdle(AThread: TThread);
begin
  CheckThreadCountOnIdle(AThread);
end;

function TXRTLASyncMultiThreadContext.GetNextCPUIndex(var CPUIndex: TXRTLCPUIndex): Boolean;
var
  I: TXRTLCPUIndex;
begin
  Result:= False;
  for I:= 0 to XRTLASyncCPUCountMax - 1 do
  begin
    if (((1 shl FNextCPUIndex) and ContextAffinityMask) <> 0) and
      (FThreadPerCPUCount[FNextCPUIndex] < FThreadPerCPUMax) then
    begin
      CPUIndex:= FNextCPUIndex;
      Result:= True;
      Exit;
    end;
    FNextCPUIndex:= (FNextCPUIndex + 1) mod XRTLASyncCPUCountMax;
  end;
end;

procedure TXRTLASyncMultiThreadContext.CheckThreadCountOnInvoke;
var
  LCPUIndex: TXRTLCPUIndex;
begin
  if FThreadTotalCount - FThreadBusyCount <= FTaskQueue.GetSize then
  begin
// start new thread
    if GetNextCPUIndex(LCPUIndex) then
    begin
      OnThreadCreate(TXRTLASyncCPUBoundInvokeThread.Create(Self, LCPUIndex));
    end;
  end;
end;

procedure TXRTLASyncMultiThreadContext.CheckThreadCountOnIdle(AThread: TThread);
var
  LThread: TXRTLASyncCPUBoundInvokeThread;
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
// exit if number of free threads is less or equal than number of tasks in queue
// let the thread run
  if FThreadTotalCount - FThreadBusyCount <= FTaskQueue.GetSize then
    Exit;
  if FTerminating then
  begin
    AThread.Terminate;
    OnThreadDestroy(AThread);
    Exit;
  end;
// terminate thread if number of threads on CPU is greater than ThreadPerCPUMin
  if XRTLIsAs(AThread, TXRTLASyncCPUBoundInvokeThread, LThread) then
  begin
    if FThreadPerCPUCount[LThread.CPUIndex] > FThreadPerCPUMin then
    begin
      AThread.Terminate;
      OnThreadDestroy(AThread);
    end;
  end;
end;

{ TXRTLASyncSingleThreadContext }

constructor TXRTLASyncSingleThreadContext.Create;
begin
  inherited;
  FThread:= nil;
end;

procedure TXRTLASyncSingleThreadContext.OnInvoke(ATask: IXRTLASyncTask);
begin
  if not Assigned(FThread) then
  begin
    FThread:= TXRTLASyncInvokeThread.Create(Self);
    InterlockedIncrement(FThreadTotalCount);
  end;
end;

procedure TXRTLASyncSingleThreadContext.OnIdle(AThread: TThread);
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
// exit if number of free threads is less or equal than number of tasks in queue
// let the thread run
  if FThreadTotalCount - FThreadBusyCount <= FTaskQueue.GetSize then
    Exit;
  if FTerminating then
  begin
    AThread.Terminate;
    InterlockedDecrement(FThreadTotalCount);
    Exit;
  end;
end;

end.

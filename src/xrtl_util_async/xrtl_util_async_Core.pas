unit xrtl_util_async_Core;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils, SyncObjs, Classes,
  xrtl_util_Compare, xrtl_util_Container, xrtl_util_Array, xrtl_util_Value,
  xrtl_util_Lock, xrtl_util_TimeStamp;

const
  XRTLASyncCPUCountMax = 32;

type
  TXRTLCPUIndex = 0 .. XRTLASyncCPUCountMax - 1;
  TXRTLThreadPerCPUCount = array[TXRTLCPUIndex] of Integer;

  TXRTLASyncOption = (aoSync, aoSyncCallback, aoSyncScheduleCallback,
                      aoRepeatable, aoUseContextOptions);
  TXRTLASyncOptions = set of TXRTLASyncOption;

  TXRTLASyncTaskState = (atsScheduled, atsCompleted, atsCanceled);

  IXRTLASyncInvokeable = interface;

  IXRTLASyncTask = interface
  ['{AEFB8101-6FD8-42A9-9C64-3B9968D0D770}']
    function   IsCanceled: Boolean;
    function   IsCompleted: Boolean;
    function   GetWaitableHandle: THandle;
    function   GetFatalException: Exception;
    function   WaitFor(ATimeOut: DWORD = INFINITE): Boolean;
    function   GetData: IXRTLValue;
    procedure  Cancel;
  end;

  IXRTLASyncInvokeable = interface
  ['{AEFB8102-6FD8-42A9-9C64-3B9968D0D770}']
    procedure  Invoke(ATask: IXRTLASyncTask);
  end;

  IXRTLASyncInvokeTask = interface(IXRTLASyncTask)
  ['{AEFB8103-6FD8-42A9-9C64-3B9968D0D770}']
    procedure  Invoke;
    procedure  InvokeCallback;
    function   GetOptions: TXRTLASyncOptions;
    function   GetInvokeable: IXRTLASyncInvokeable;
    function   GetInvokeCallback: IXRTLASyncInvokeable;
    function   GetPriority: TThreadPriority;
    function   GetState: TXRTLASyncTaskState;
    procedure  SetCompleted;
  end;

  IXRTLASyncScheduleTask = interface(IXRTLASyncInvokeTask)
  ['{AEFB8104-6FD8-42A9-9C64-3B9968D0D770}']
    procedure  InvokeScheduleCallback;
    function   GetScheduleCallback: IXRTLASyncInvokeable;
    function   GetCurrentDelay: Cardinal;
    function   NextDelay: Boolean;
    function   GetScheduleTimeStamp: TXRTLTimeStamp;
    procedure  SetLastInvokeTask(ATask: IXRTLASyncInvokeTask);
    function   WaitForLastInvokeTask: Boolean;
  end;

  TXRTLASyncProc   = procedure(const ATask: IXRTLASyncTask);
  TXRTLASyncMethod = procedure(const ATask: IXRTLASyncTask) of object;

  PXRTLASyncDelays = ^TXRTLASyncDelays;
  TXRTLASyncDelays = array of Cardinal;

  TXRTLASyncContextManager = class;

  TXRTLASyncContext = class
  private
  protected
    function   GetScheduleQueueLength: Integer; virtual; abstract;
    function   GetTaskQueueLength: Integer; virtual; abstract;
    function   GetInvokeTask(AThread: TThread): IXRTLASyncTask; virtual; abstract;
    function   GetScheduleTask(AThread: TThread): IXRTLASyncTask; virtual; abstract;
    procedure  ReSchedule(ATask: IXRTLASyncTask); virtual; abstract;
    procedure  OnTaskStart(AThread: TThread); virtual; abstract;
    procedure  OnTaskDone(AThread: TThread); virtual; abstract;
    procedure  OnInvoke(ATask: IXRTLASyncTask); virtual; abstract;
    procedure  OnIdle(AThread: TThread); virtual; abstract;
    function   CreateTask(AInvokeable: IXRTLASyncInvokeable;
                          AInvokeCallback: IXRTLASyncInvokeable;
                          AScheduleCallback: IXRTLASyncInvokeable;
                          AData: IXRTLValue; AOptions: TXRTLASyncOptions;
                          APriority: TThreadPriority;
                          ADelays: PXRTLASyncDelays): IXRTLASyncTask; virtual; abstract;
    function   GetManager: TXRTLASyncContextManager; virtual; abstract;
    procedure  SetManager(AManager: TXRTLASyncContextManager); virtual; abstract;
    function   GetOptions: TXRTLASyncOptions; virtual; abstract;
    procedure  SetOptions(const Value: TXRTLASyncOptions); virtual; abstract;
  public
    function   Invoke(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                      AInvokeCallback: IXRTLASyncInvokeable = nil;
                      AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                      APriority: TThreadPriority = tpNormal): IXRTLASyncTask; virtual; abstract;
    function   Schedule(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                        ADelays: TXRTLASyncDelays;
                        AInvokeCallback: IXRTLASyncInvokeable = nil;
                        AScheduleCallback: IXRTLASyncInvokeable = nil;
                        AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                        APriority: TThreadPriority = tpNormal): IXRTLASyncTask; virtual; abstract;
    procedure  CancelAll; virtual; abstract;
    property   Manager: TXRTLASyncContextManager read GetManager write SetManager;
    property   TaskQueueLength: Integer read GetTaskQueueLength;
    property   ScheduleQueueLength: Integer read GetScheduleQueueLength;
    property   Options: TXRTLASyncOptions read GetOptions write SetOptions;
  end;

  TXRTLASyncContextBase = class(TXRTLASyncContext)
  private
  protected
    FTerminating: Boolean;
    FThreadTotalCount: Integer;
    FThreadBusyCount: Integer;
    FLock: IXRTLReadWriteLock;
    FScheduleThread: TThread;
    FTaskQueue: TXRTLArray;
    FTaskEvent: TEvent;
    FSchedulerQueue: TXRTLArray;
    FSchedulerTaskEvent: TEvent;
    FManager: TXRTLASyncContextManager;
    FOptions: TXRTLASyncOptions;
    function   GetScheduleQueueLength: Integer; override;
    function   GetTaskQueueLength: Integer; override;
    procedure  OnTaskStart(AThread: TThread); override;
    procedure  OnTaskDone(AThread: TThread); override;
    function   CreateTask(AInvokeable: IXRTLASyncInvokeable;
                          AInvokeCallback: IXRTLASyncInvokeable;
                          AScheduleCallback: IXRTLASyncInvokeable;
                          AData: IXRTLValue; AOptions: TXRTLASyncOptions;
                          APriority: TThreadPriority;
                          ADelays: PXRTLASyncDelays): IXRTLASyncTask; override;
    procedure  CheckScheduleThread;
    function   GetInvokeTask(AThread: TThread): IXRTLASyncTask; override;
    function   GetScheduleTask(AThread: TThread): IXRTLASyncTask; override;
    procedure  ReSchedule(ATask: IXRTLASyncTask); override;
    function   GetManager: TXRTLASyncContextManager; override;
    procedure  SetManager(AManager: TXRTLASyncContextManager); override;
    function   GetOptions: TXRTLASyncOptions; override;
    procedure  SetOptions(const Value: TXRTLASyncOptions); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  BeforeDestruction; override;
    function   Invoke(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                      AInvokeCallback: IXRTLASyncInvokeable = nil;
                      AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                      APriority: TThreadPriority = tpNormal): IXRTLASyncTask; override;
    function   Schedule(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                        ADelays: TXRTLASyncDelays;
                        AInvokeCallback: IXRTLASyncInvokeable = nil;
                        AScheduleCallback: IXRTLASyncInvokeable = nil;
                        AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                        APriority: TThreadPriority = tpNormal): IXRTLASyncTask; override;
    procedure  CancelAll; override;
    property   ThreadTotalCount: Integer read FThreadTotalCount;
    property   ThreadBusyCount: Integer read FThreadBusyCount;
  end;

  TXRTLASyncMultiThreadContextBase = class(TXRTLASyncContextBase)
  private
    FContextAffinityMask: Cardinal;
  protected
    FThreadPerCPUMin: Byte;
    FThreadPerCPUMax: Byte;
    FThreadPerCPUCount: TXRTLThreadPerCPUCount;
    property   ContextAffinityMask: Cardinal read FContextAffinityMask;
  public
    constructor Create(const AContextAffinityMask: Cardinal); virtual;
    destructor Destroy; override;
    property   ThreadPerCPUMin: Byte read FThreadPerCPUMin write FThreadPerCPUMin;
    property   ThreadPerCPUMax: Byte read FThreadPerCPUMax write FThreadPerCPUMax;
    property   ThreadPerCPUCount: TXRTLThreadPerCPUCount read FThreadPerCPUCount;
  end;

  TXRTLASyncContextThread = class(TThread)
  private
  protected
    FContext: TXRTLASyncContext;
    function   GetInvokeTask: IXRTLASyncTask; virtual;
    function   GetScheduleTask: IXRTLASyncTask; virtual;
    procedure  ReSchedule(ATask: IXRTLASyncTask); virtual;
    procedure  DoTaskStart; virtual;
    procedure  DoTaskDone; virtual;
    procedure  DoIdle; virtual;
  public
    constructor Create(AContext: TXRTLASyncContext);
    procedure  AfterConstruction; override;
  end;

  TXRTLASyncContextManager = class
  private
  protected
    FDefaultThreadPerCPUMin: Byte;
    FDefaultThreadPerCPUMax: Byte;
    FDefaultContext: TXRTLASyncMultiThreadContextBase;
  public
    function   Invoke(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                      AInvokeCallback: IXRTLASyncInvokeable = nil;
                      AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                      APriority: TThreadPriority = tpNormal): IXRTLASyncTask; virtual; abstract;
    function   Schedule(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
                        ADelays: TXRTLASyncDelays;
                        AInvokeCallback: IXRTLASyncInvokeable = nil;
                        AScheduleCallback: IXRTLASyncInvokeable = nil;
                        AOptions: TXRTLASyncOptions = [aoUseContextOptions];
                        APriority: TThreadPriority = tpNormal): IXRTLASyncTask; virtual; abstract;
    procedure  CancelAll; virtual; abstract;
    procedure  AttachContext(AContext: TXRTLASyncContext); virtual; abstract;
    procedure  DetachContext(AContext: TXRTLASyncContext); virtual; abstract;
    function   CreateMultiThreadContext(AAttachContext: Boolean = True;
                                        AThreadPerCPUMin: Byte = 0;
                                        AThreadPerCPUMax: Byte = 0;
                                        AAffinityMask: Cardinal = $FFFFFFFF): TXRTLASyncMultiThreadContextBase; virtual; abstract;
    function   CreateSingleThreadContext(AAttachContext: Boolean = True): TXRTLASyncContext; virtual; abstract;
    property   DefaultContext: TXRTLASyncMultiThreadContextBase read FDefaultContext;
    property   DefaultThreadPerCPUMin: Byte read FDefaultThreadPerCPUMin
                                            write FDefaultThreadPerCPUMin;
    property   DefaultThreadPerCPUMax: Byte read FDefaultThreadPerCPUMax
                                            write FDefaultThreadPerCPUMax;
  end;
  
function XRTLASyncInvokeable(AProc: TXRTLASyncProc): IXRTLASyncInvokeable; overload;
function XRTLASyncInvokeable(AMethod: TXRTLASyncMethod): IXRTLASyncInvokeable; overload;
function XRTLASyncDelays(ADelays: array of Cardinal): TXRTLASyncDelays;

var
  XRTLASyncContextManager: TXRTLASyncContextManager = nil;

implementation

uses
  xrtl_util_async_Task, xrtl_util_async_Thread, xrtl_util_async_ContextManager;

function XRTLASyncDelays(ADelays: array of Cardinal): TXRTLASyncDelays;
var
  I: Integer;
begin
  SetLength(Result, Length(ADelays));
  for I:= 0 to Length(ADelays) - 1 do
    Result[I]:= ADelays[I];
end;

type
  TXRTLASyncInvokeable = class(TInterfacedObject, IXRTLASyncInvokeable)
  protected
    procedure  DoInvoke(ATask: IXRTLASyncTask); virtual; abstract;
  public
    procedure  Invoke(ATask: IXRTLASyncTask);
  end;

  TXRTLASyncInvokeableProcedure = class(TXRTLASyncInvokeable)
  private
    FProc: TXRTLASyncProc;
  protected
    procedure  DoInvoke(ATask: IXRTLASyncTask); override;
  public
    constructor Create(AProc: TXRTLASyncProc);
  end;

  TXRTLASyncInvokeableMethod = class(TXRTLASyncInvokeable)
  private
    FMethod: TXRTLASyncMethod;
  protected
    procedure  DoInvoke(ATask: IXRTLASyncTask); override;
  public
    constructor Create(AMethod: TXRTLASyncMethod);
  end;

{ TXRTLASyncInvokeable }

procedure TXRTLASyncInvokeable.Invoke(ATask: IXRTLASyncTask);
begin
  DoInvoke(ATask);
end;

{ TXRTLASyncInvokeableProcedure }

constructor TXRTLASyncInvokeableProcedure.Create(AProc: TXRTLASyncProc);
begin
  inherited Create;
  FProc:= AProc;
end;

procedure TXRTLASyncInvokeableProcedure.DoInvoke(ATask: IXRTLASyncTask);
begin
  if Assigned(FProc) then
    FProc(ATask);
end;

{ TXRTLASyncInvokeableMethod }

constructor TXRTLASyncInvokeableMethod.Create(AMethod: TXRTLASyncMethod);
begin
  inherited Create;
  FMethod:= AMethod;
end;

procedure TXRTLASyncInvokeableMethod.DoInvoke(ATask: IXRTLASyncTask);
begin
  if Assigned(FMethod) then
    FMethod(ATask);
end;

function XRTLASyncInvokeable(AProc: TXRTLASyncProc): IXRTLASyncInvokeable;
begin
  Result:= TXRTLASyncInvokeableProcedure.Create(AProc);
end;

function XRTLASyncInvokeable(AMethod: TXRTLASyncMethod): IXRTLASyncInvokeable;
begin
  Result:= TXRTLASyncInvokeableMethod.Create(AMethod);
end;

{ TXRTLASyncContextThread }

constructor TXRTLASyncContextThread.Create(AContext: TXRTLASyncContext);
begin
  inherited Create(False);
  FContext:= AContext;
end;

procedure TXRTLASyncContextThread.AfterConstruction;
begin
  inherited;
  Resume;
end;

procedure TXRTLASyncContextThread.DoTaskStart;
begin
  FContext.OnTaskStart(Self);
end;

procedure TXRTLASyncContextThread.DoTaskDone;
begin
  FContext.OnTaskDone(Self);
end;

procedure TXRTLASyncContextThread.DoIdle;
begin
  FContext.OnIdle(Self);
end;

function TXRTLASyncContextThread.GetInvokeTask: IXRTLASyncTask;
begin
  Result:= FContext.GetInvokeTask(Self);
end;

function TXRTLASyncContextThread.GetScheduleTask: IXRTLASyncTask;
begin
  Result:= FContext.GetScheduleTask(Self);
end;

procedure TXRTLASyncContextThread.ReSchedule(ATask: IXRTLASyncTask);
begin
  FContext.ReSchedule(ATask);
end;

{ TXRTLASyncContextBase }

constructor TXRTLASyncContextBase.Create;
begin
  inherited Create;
  FTerminating:= False;
  FThreadTotalCount:= 0;
  FThreadBusyCount:= 0;
  FOptions:= [];
  FLock:= XRTLCreateReadWriteLock;
  FTaskQueue:= TXRTLArray.Create;
  FTaskEvent:= TEvent.Create(nil, False, False, '');
  FSchedulerQueue:= TXRTLArray.Create;
  FSchedulerQueue.Sorted:= True;
  FSchedulerQueue.Duplicates:= dupAllow;
  FSchedulerTaskEvent:= TEvent.Create(nil, False, False, '');
end;

destructor TXRTLASyncContextBase.Destroy;
begin
  FreeAndNil(FScheduleThread);
  FreeAndNil(FSchedulerTaskEvent);
  FreeAndNil(FSchedulerQueue);
  FreeAndNil(FTaskEvent);
  FreeAndNil(FTaskQueue);
  inherited;
end;

procedure TXRTLASyncContextBase.BeforeDestruction;
begin
  inherited;
  if Assigned(FManager) then
    FManager.DetachContext(Self);
  FTerminating:= True;
  CancelAll;
  while FThreadTotalCount > 0 do
    Sleep(100);
end;

function TXRTLASyncContextBase.CreateTask(AInvokeable: IXRTLASyncInvokeable;
  AInvokeCallback: IXRTLASyncInvokeable; AScheduleCallback: IXRTLASyncInvokeable;
  AData: IXRTLValue; AOptions: TXRTLASyncOptions; APriority: TThreadPriority;
  ADelays: PXRTLASyncDelays): IXRTLASyncTask;
begin
  Result:= TXRTLASyncTask.Create(AInvokeable, AInvokeCallback, AScheduleCallback,
                                 AData, AOptions, APriority, ADelays);
end;

procedure TXRTLASyncContextBase.OnTaskStart(AThread: TThread);
begin
  InterlockedIncrement(FThreadBusyCount);
end;

procedure TXRTLASyncContextBase.OnTaskDone(AThread: TThread);
begin
  InterlockedDecrement(FThreadBusyCount);
end;

procedure TXRTLASyncContextBase.CheckScheduleThread;
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  if not Assigned(FScheduleThread) then
    FScheduleThread:= TXRTLASyncScheduleThread.Create(Self);
end;

function TXRTLASyncContextBase.GetInvokeTask(AThread: TThread): IXRTLASyncTask;
begin
  Result:= nil;
  try
    FLock.BeginRead;
    if FTaskQueue.IsEmpty then
    begin
      if FTaskEvent.WaitFor(50) <> wrSignaled then Exit;
    end;
    try
      FLock.BeginWrite;
      if not FTaskQueue.IsEmpty then
      begin
        XRTLGetAsInterface(FTaskQueue.GetValue(0), Result);
        FTaskQueue.Remove(0);
        if FTerminating then
          Result.Cancel;
      end;
    finally
      FLock.EndWrite;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TXRTLASyncContextBase.GetScheduleTask(AThread: TThread): IXRTLASyncTask;
var
  LTimeStamp: TXRTLTimeStamp;
begin
  Result:= nil;
  LTimeStamp:= nil;
  try
    FLock.BeginRead;
    LTimeStamp:= TXRTLTimeStamp.Create;
    if FSchedulerQueue.IsEmpty then
    begin
      if FSchedulerTaskEvent.WaitFor(10) <> wrSignaled then Exit;
    end;
    try
      FLock.BeginWrite;
      if not FSchedulerQueue.IsEmpty then
      begin
        XRTLGetAsInterface(FSchedulerQueue.GetValue(0), Result);
        if (LTimeStamp.Compare((Result as IXRTLASyncScheduleTask).GetScheduleTimeStamp) = XRTLGreaterThanValue) or
           Result.IsCanceled or Result.IsCompleted or FTerminating then
          FSchedulerQueue.Remove(0)
        else
          Result:= nil;
        FSchedulerTaskEvent.SetEvent;
      end;
    finally
      FLock.EndWrite;
    end;
  finally
    FreeAndNil(LTimeStamp);
    FLock.EndRead;
  end;
end;

procedure TXRTLASyncContextBase.ReSchedule(ATask: IXRTLASyncTask);
var
  LSheduleTimeStamp: Int64;
  LTask: IXRTLASyncScheduleTask;
begin
  try
    FLock.BeginWrite;
    LTask:= ATask as IXRTLASyncScheduleTask;
    if LTask.NextDelay then
    begin
// FILETIME is represented in 100-nanosecond intervals
// Delay is represented in milliseconds
      LSheduleTimeStamp:= Int64(LTask.GetScheduleTimeStamp.UTCFileTime) +
                          LTask.GetCurrentDelay * 10000;
      LTask.GetScheduleTimeStamp.UTCFileTime:= TFileTime(LSheduleTimeStamp);
    end;
    if not LTask.IsCompleted and FTerminating then
    begin
      LTask.Cancel;
      LTask.GetScheduleTimeStamp.SetCurrentTime;
    end;
    FSchedulerQueue.Add(XRTLValue(ATask));
    FSchedulerTaskEvent.SetEvent;
    if not FSchedulerQueue.IsEmpty then
      CheckScheduleThread;
  finally
    FLock.EndWrite;
  end;
end;

function TXRTLASyncContextBase.Invoke(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
  AInvokeCallback: IXRTLASyncInvokeable = nil; AOptions: TXRTLASyncOptions = [aoUseContextOptions];
  APriority: TThreadPriority = tpNormal): IXRTLASyncTask;
begin
  try
    try
      FLock.BeginWrite;
      if aoUseContextOptions in AOptions then
        AOptions:= FOptions;
      Result:= CreateTask(AInvokeable, AInvokeCallback, nil, AData, AOptions, APriority, nil);
      FTaskQueue.Add(XRTLValue(Result));
      FTaskEvent.SetEvent;
      OnInvoke(Result);
    finally
      FLock.EndWrite;
    end;
  except
    Result:= nil;
    raise;
  end;
end;

function TXRTLASyncContextBase.Schedule(AInvokeable: IXRTLASyncInvokeable; AData: IXRTLValue;
  ADelays: TXRTLASyncDelays; AInvokeCallback: IXRTLASyncInvokeable = nil;
  AScheduleCallback: IXRTLASyncInvokeable = nil; AOptions: TXRTLASyncOptions = [aoUseContextOptions];
  APriority: TThreadPriority = tpNormal): IXRTLASyncTask;
begin
  if aoUseContextOptions in AOptions then
    AOptions:= FOptions;
  Result:= CreateTask(AInvokeable, AInvokeCallback, AScheduleCallback,
                      AData, AOptions, APriority, @ADelays);
  ReSchedule(Result);
end;

procedure TXRTLASyncContextBase.CancelAll;
var
  LTask: IXRTLASyncTask;
  I: Integer;
begin
  try
    FLock.BeginWrite;
    for I:= 0 to FTaskQueue.GetSize - 1 do
    begin
      XRTLGetAsInterface(FTaskQueue.GetValue(I), LTask);
      LTask.Cancel;
    end;
    for I:= 0 to FSchedulerQueue.GetSize - 1 do
    begin
      XRTLGetAsInterface(FSchedulerQueue.GetValue(I), LTask);
      LTask.Cancel;
    end;
  finally
    FTaskEvent.SetEvent;
    FSchedulerTaskEvent.SetEvent;
    FLock.EndWrite;
  end;
end;

function TXRTLASyncContextBase.GetManager: TXRTLASyncContextManager;
begin
  Result:= FManager;
end;

procedure TXRTLASyncContextBase.SetManager(AManager: TXRTLASyncContextManager);
begin
  FManager:= AManager;
end;

function TXRTLASyncContextBase.GetOptions: TXRTLASyncOptions;
begin
  Result:= FOptions;
end;

procedure TXRTLASyncContextBase.SetOptions(const Value: TXRTLASyncOptions);
begin
  FOptions:= Value;
end;

function TXRTLASyncContextBase.GetTaskQueueLength: Integer;
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginReadLock(FLock);
  Result:= FTaskQueue.GetSize;
end;

function TXRTLASyncContextBase.GetScheduleQueueLength: Integer;
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginReadLock(FLock);
  Result:= FSchedulerQueue.GetSize;
end;

{ TXRTLASyncMultiThreadContextBase }

constructor TXRTLASyncMultiThreadContextBase.Create(const AContextAffinityMask: Cardinal);
begin
  inherited Create;
  ZeroMemory(@FThreadPerCPUCount, SizeOf(FThreadPerCPUCount));
  FThreadPerCPUMin:= 8;
  FThreadPerCPUMax:= 32;
  FContextAffinityMask:= AContextAffinityMask;
end;

destructor TXRTLASyncMultiThreadContextBase.Destroy;
begin

  inherited;
end;

initialization
begin
  XRTLASyncContextManager:= TXRTLASyncDefaultContextManager.Create;
end;

finalization
begin
  FreeAndNil(XRTLASyncContextManager);
end;

end.


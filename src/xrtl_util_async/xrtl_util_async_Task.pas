unit xrtl_util_async_Task;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils, Classes, SyncObjs,
  xrtl_util_TimeStamp, xrtl_util_Value, xrtl_util_Compare,
  xrtl_util_async_Core;

type
  TXRTLASyncTask = class(TInterfacedObject,
                         IXRTLASyncTask, IXRTLASyncInvokeTask, IXRTLASyncScheduleTask,
                         IXRTLComparable)
  private
    FEvent: TEvent;
    FFatalException: TObject;
    FInvokeable: IXRTLASyncInvokeable;
    FInvokeCallback: IXRTLASyncInvokeable;
    FScheduleCallback: IXRTLASyncInvokeable;
    FData: IXRTLValue;
    FOptions: TXRTLASyncOptions;
    FPriority: TThreadPriority;
    FState: TXRTLASyncTaskState;
    FDelays: TXRTLASyncDelays;
    FDelayIndex: Integer;
    FScheduleTimeStamp: TXRTLTimeStamp;
    FLastInvokeTask: IXRTLASyncInvokeTask;
  protected
  public
    constructor Create(AInvokeable: IXRTLASyncInvokeable;
                       AInvokeCallback: IXRTLASyncInvokeable;
                       AScheduleCallback: IXRTLASyncInvokeable;
                       AData: IXRTLValue; AOptions: TXRTLASyncOptions;
                       APriority: TThreadPriority; ADelays: PXRTLASyncDelays);
    destructor Destroy; override;
    function   IsCanceled: Boolean;
    function   IsCompleted: Boolean;
    function   GetWaitableHandle: THandle;
    function   GetFatalException: Exception;
    function   WaitFor(ATimeOut: DWORD = INFINITE): Boolean;
    procedure  Invoke;
    procedure  InvokeCallback;
    procedure  InvokeScheduleCallback;
    function   GetData: IXRTLValue;
    procedure  Cancel;
    function   GetOptions: TXRTLASyncOptions;
    function   GetInvokeable: IXRTLASyncInvokeable;
    function   GetInvokeCallback: IXRTLASyncInvokeable;
    function   GetScheduleCallback: IXRTLASyncInvokeable;
    function   GetPriority: TThreadPriority;
    function   GetState: TXRTLASyncTaskState;
    function   GetCurrentDelay: Cardinal;
    function   NextDelay: Boolean;
    function   GetScheduleTimeStamp: TXRTLTimeStamp;
    procedure  SetLastInvokeTask(ATask: IXRTLASyncInvokeTask);
    function   WaitForLastInvokeTask: Boolean;
    procedure  SetCompleted;
    function   Compare(const IValue: IInterface): TXRTLValueRelationship;
  end;

implementation

{ TXRTLASyncTask }

constructor TXRTLASyncTask.Create(AInvokeable: IXRTLASyncInvokeable;
  AInvokeCallback: IXRTLASyncInvokeable; AScheduleCallback: IXRTLASyncInvokeable;
  AData: IXRTLValue; AOptions: TXRTLASyncOptions; APriority: TThreadPriority;
  ADelays: PXRTLASyncDelays);
begin
  inherited Create;
  FState:= atsScheduled;
  FFatalException:= nil;
  FEvent:= TEvent.Create(nil, True, False, '');
  FInvokeable:= AInvokeable;
  FInvokeCallback:= AInvokeCallback;
  FScheduleCallback:= AScheduleCallback;
  FData:= AData;
  FPriority:= APriority;
  if Assigned(ADelays) then
    FDelays:= Copy(ADelays^)
  else
    SetLength(FDelays, 0);
  FDelayIndex:= -1;
  if Length(FDelays) < 1 then
    AOptions:= AOptions - [aoRepeatable];
  FOptions:= AOptions;
  FScheduleTimeStamp:= TXRTLTimeStamp.Create;
  FLastInvokeTask:= nil;
end;

destructor TXRTLASyncTask.Destroy;
begin
  FreeAndNil(FScheduleTimeStamp);
  FreeAndNil(FFatalException);
  FreeAndNil(FEvent);
  inherited;
end;

function TXRTLASyncTask.IsCanceled: Boolean;
begin
  Result:= FState = atsCanceled;
end;

function TXRTLASyncTask.IsCompleted: Boolean;
begin
  Result:= FState = atsCompleted;
end;

function TXRTLASyncTask.GetWaitableHandle: THandle;
begin
  Result:= FEvent.Handle;
end;

function TXRTLASyncTask.GetFatalException: Exception;
begin
  Result:= Exception(FFatalException);
end;

procedure TXRTLASyncTask.Invoke;
begin
  FreeAndNil(FFatalException);
  try
    if Assigned(FInvokeable) then
      FInvokeable.Invoke(Self as IXRTLASyncTask);
  except
    FFatalException:= AcquireExceptionObject;
  end;
end;

procedure TXRTLASyncTask.InvokeCallback;
begin
  try
    if Assigned(FInvokeCallback) then
      FInvokeCallback.Invoke(Self as IXRTLASyncTask);
  except
  end;
end;

procedure TXRTLASyncTask.InvokeScheduleCallback;
begin
  try
    if Assigned(FScheduleCallback) then
      FScheduleCallback.Invoke(Self as IXRTLASyncTask);
  except
  end;
end;

function TXRTLASyncTask.WaitFor(ATimeOut: DWORD = INFINITE): Boolean;
begin
  Result:= FEvent.WaitFor(ATimeOut) = wrSignaled;
end;

function TXRTLASyncTask.GetData: IXRTLValue;
begin
  Result:= FData;
end;

procedure TXRTLASyncTask.Cancel;
begin
  case FState of
    atsScheduled:
      FState:= atsCanceled;
  end;
end;

function TXRTLASyncTask.GetOptions: TXRTLASyncOptions;
begin
  Result:= FOptions;
end;

function TXRTLASyncTask.GetInvokeable: IXRTLASyncInvokeable;
begin
  Result:= FInvokeable;
end;

function TXRTLASyncTask.GetInvokeCallback: IXRTLASyncInvokeable;
begin
  Result:= FInvokeCallback;
end;

function TXRTLASyncTask.GetScheduleCallback: IXRTLASyncInvokeable;
begin
  Result:= FScheduleCallback;
end;

function TXRTLASyncTask.GetPriority: TThreadPriority;
begin
  Result:= FPriority;
end;

function TXRTLASyncTask.GetState: TXRTLASyncTaskState;
begin
  Result:= FState;
end;

function TXRTLASyncTask.GetCurrentDelay: Cardinal;
begin
  Result:= 0;
  if Length(FDelays) = 0 then
    Exit;
  if FDelayIndex < Length(FDelays) then
    Result:= FDelays[FDelayIndex];
end;

function TXRTLASyncTask.NextDelay: Boolean;
begin
  Result:= False;
  try
// return False if there is no delays specified
    if Length(FDelays) = 0 then
      Exit;
    Result:= True;
// return True if last delay value is selected and task is Repeatable
    if (FDelayIndex = Length(FDelays) - 1) and (aoRepeatable in FOptions) then
      Exit;
    Result:= FDelayIndex < Length(FDelays) - 1;
    if Result then
      InterlockedIncrement(FDelayIndex);
  finally
    if not Result then
      FState:= atsCompleted;
  end;
end;

function TXRTLASyncTask.GetScheduleTimeStamp: TXRTLTimeStamp;
begin
  Result:= FScheduleTimeStamp;
end;

procedure TXRTLASyncTask.SetLastInvokeTask(ATask: IXRTLASyncInvokeTask);
begin
  FLastInvokeTask:= ATask;
end;

function TXRTLASyncTask.WaitForLastInvokeTask: Boolean;
begin
  if not Assigned(FLastInvokeTask) then
  begin
    Result:= True;
    Exit;
  end;
  Result:= FLastInvokeTask.WaitFor;
  if Result then
    FLastInvokeTask:= nil;
end;

procedure TXRTLASyncTask.SetCompleted;
begin
  if (FState <> atsCanceled) and not Assigned(FFatalException) then
  begin
    FState:= atsCompleted;
  end;
  FEvent.SetEvent;
end;

function TXRTLASyncTask.Compare(const IValue: IInterface): TXRTLValueRelationship;
var
  RTask: IXRTLASyncScheduleTask;
begin
  if Supports(IValue, IXRTLASyncScheduleTask, RTask) then
    Result:= GetScheduleTimeStamp.Compare(RTask.GetScheduleTimeStamp)
  else
    Result:= XRTLLessThanValue;
end;

end.

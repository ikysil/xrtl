unit xrtl_util_async_Thread;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils, Classes,
  xrtl_util_Value,
  xrtl_util_async_Core;

type
  TXRTLASyncInvokeThread = class(TXRTLASyncContextThread)
  private
    FTask: IXRTLASyncInvokeTask;
    procedure  DoInvoke;
  protected
    procedure  Execute; override;
  public
    constructor Create(AContext: TXRTLASyncContext);
  end;

  TXRTLASyncCPUBoundInvokeThread = class(TXRTLASyncInvokeThread)
  private
    FCPUIndex: TXRTLCPUIndex;
  public
    constructor Create(AContext: TXRTLASyncContext; ACPUIndex: TXRTLCPUIndex);
    property   CPUIndex: TXRTLCPUIndex read FCPUIndex;
  end;

  TXRTLASyncScheduleThread = class(TXRTLASyncContextThread)
  private
    FTask: IXRTLASyncScheduleTask;
    procedure  InvokeScheduleCallback(const ATask: IXRTLASyncTask);
  protected
    procedure  Execute; override;
  public
  end;

implementation

{ TXRTLASyncInvokeThread }

constructor TXRTLASyncInvokeThread.Create(AContext: TXRTLASyncContext);
begin
  inherited Create(AContext);
  FreeOnTerminate:= True;
end;

procedure TXRTLASyncInvokeThread.DoInvoke;
begin
  if Assigned(FTask) then
    FTask.Invoke;
end;

procedure XRTLASyncInvokeThread_InvokeCallback(const ATask: IXRTLASyncTask);
var
  LTask: IXRTLASyncInvokeTask;
begin
  XRTLGetAsInterface(ATask.GetData, LTask);
  LTask.InvokeCallback;
  LTask.SetCompleted;
end;

procedure TXRTLASyncInvokeThread.Execute;
var
  LCallbackOptions: TXRTLASyncOptions;
  LPriority: TThreadPriority;
begin
  while not Terminated do
  begin
    try
      FTask:= GetInvokeTask as IXRTLASyncInvokeTask;
      if Assigned(FTask) then
      begin
        LPriority:= Priority;
        try
          Priority:= FTask.GetPriority;
          DoTaskStart;
          if aoSync in FTask.GetOptions then
            Synchronize(DoInvoke)
          else
            DoInvoke;
          if Assigned(FTask.GetInvokeCallback) then
          begin
            LCallbackOptions:= [];
            if aoSyncCallback in FTask.GetOptions then
              Include(LCallbackOptions, aoSync);
            FContext.Invoke(XRTLASyncInvokeable(XRTLASyncInvokeThread_InvokeCallback),
                            XRTLValue(FTask), nil, LCallbackOptions, FTask.GetPriority);
          end
          else
            FTask.SetCompleted;
        finally
          Priority:= LPriority;
          DoTaskDone;
          FTask:= nil;
        end;
      end;
    finally
      DoIdle;
    end;
  end;
end;

{ TXRTLASyncCPUBoundInvokeThread }

constructor TXRTLASyncCPUBoundInvokeThread.Create(AContext: TXRTLASyncContext; ACPUIndex: TXRTLCPUIndex);
begin
  inherited Create(AContext);
  FCPUIndex:= ACPUIndex;
  SetThreadAffinityMask(Handle, 1 shl FCPUIndex);
end;

{ TXRTLASyncScheduleThread }

procedure TXRTLASyncScheduleThread.InvokeScheduleCallback(const ATask: IXRTLASyncTask);
var
  LTask: IXRTLASyncScheduleTask;
  LCallbackOptions: TXRTLASyncOptions;
begin
  XRTLGetAsInterface(ATask.GetData, LTask);
  while not (LTask.WaitForLastInvokeTask or LTask.IsCanceled or ATask.IsCanceled) do
    Sleep(100);
  LCallbackOptions:= [];
  if aoSyncScheduleCallback in LTask.GetOptions then
    Include(LCallbackOptions, aoSync);
  FContext.Invoke(LTask.GetScheduleCallback, LTask.GetData, nil,
                  LCallbackOptions, LTask.GetPriority);
end;

procedure TXRTLASyncScheduleThread.Execute;
var
  LTask: IXRTLASyncTask;
begin
  while not Terminated do
  begin
    try
      FTask:= GetScheduleTask as IXRTLASyncScheduleTask;
      if Assigned(FTask) then
      begin
// if IsCanceled - invoke schedule callback
// if IsCompleted - invoke schedule callback
// else - invoke task, invoke callback
        if FTask.IsCanceled or FTask.IsCompleted then
        begin
          if Assigned(FTask.GetScheduleCallback) then
          begin
            FContext.Invoke(XRTLASyncInvokeable(InvokeScheduleCallback), XRTLValue(FTask));
          end;
        end
        else
        begin
          LTask:= FContext.Invoke(FTask.GetInvokeable, FTask.GetData,
                                  FTask.GetInvokeCallback, FTask.GetOptions, FTask.GetPriority);
          FTask.SetLastInvokeTask(LTask as IXRTLASyncInvokeTask);
          ReSchedule(FTask);
        end;
      end;
    except
    end;
  end;
end;

end.

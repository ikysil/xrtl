unit xrtl_util_async_ContextManager;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils, Classes,
  xrtl_util_Container, xrtl_util_Array, xrtl_util_Lock,
  xrtl_util_Value,
  xrtl_util_async_Core;

type
  TXRTLASyncDefaultContextManager = class(TXRTLASyncContextManager)
  private
    FLock: IXRTLReadWriteLock;
    FContextList: TXRTLArray;
    FDefaultAffinityMask: Cardinal;
  protected
    procedure  InitContext(AContext: TXRTLASyncContext);
  public
    constructor Create;
    destructor Destroy; override;
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
    procedure  AttachContext(AContext: TXRTLASyncContext); override;
    procedure  DetachContext(AContext: TXRTLASyncContext); override;
    function   CreateMultiThreadContext(AAttachContext: Boolean = True;
                                        AThreadPerCPUMin: Byte = 0;
                                        AThreadPerCPUMax: Byte = 0;
                                        AAffinityMask: Cardinal = $FFFFFFFF): TXRTLASyncMultiThreadContextBase; override;
    function   CreateSingleThreadContext(AAttachContext: Boolean = True): TXRTLASyncContext; override;
  end;

implementation

uses
  xrtl_util_async_Context;

{ TXRTLASyncDefaultContextManager }

constructor TXRTLASyncDefaultContextManager.Create;
var
  LSystemAffinityMask: Cardinal;
begin
  inherited Create;
  FDefaultThreadPerCPUMin:= 8;
  FDefaultThreadPerCPUMax:= 32;
  if not GetProcessAffinityMask(GetCurrentProcess, FDefaultAffinityMask, LSystemAffinityMask) then
    FDefaultAffinityMask:= 1;
  FLock:= XRTLCreateReadWriteLock;
  FDefaultContext:= TXRTLASyncMultiThreadContext.Create(FDefaultAffinityMask);
  InitContext(FDefaultContext);
  FContextList:= TXRTLArray.Create;
end;

destructor TXRTLASyncDefaultContextManager.Destroy;
begin
  FContextList.Clear;
  FreeAndNil(FContextList);
  FreeAndNil(FDefaultContext);
  inherited;
end;

procedure TXRTLASyncDefaultContextManager.InitContext(AContext: TXRTLASyncContext);
var
  LContext: TXRTLASyncMultiThreadContextBase;
begin
  if AContext is TXRTLASyncMultiThreadContextBase then
  begin
    LContext:= AContext as TXRTLASyncMultiThreadContextBase;
    LContext.ThreadPerCPUMin:= DefaultThreadPerCPUMin;
    LContext.ThreadPerCPUMax:= DefaultThreadPerCPUMax;
  end;
end;

function TXRTLASyncDefaultContextManager.Invoke(AInvokeable: IXRTLASyncInvokeable;
  AData: IXRTLValue; AInvokeCallback: IXRTLASyncInvokeable = nil;
  AOptions: TXRTLASyncOptions = [aoUseContextOptions]; APriority: TThreadPriority = tpNormal): IXRTLASyncTask;
begin
  Result:= FDefaultContext.Invoke(AInvokeable, AData, AInvokeCallback, AOptions, APriority);
end;

function TXRTLASyncDefaultContextManager.Schedule(AInvokeable: IXRTLASyncInvokeable;
  AData: IXRTLValue; ADelays: TXRTLASyncDelays;
  AInvokeCallback: IXRTLASyncInvokeable = nil; AScheduleCallback: IXRTLASyncInvokeable = nil;
  AOptions: TXRTLASyncOptions = [aoUseContextOptions]; APriority: TThreadPriority = tpNormal): IXRTLASyncTask;
begin
  Result:= FDefaultContext.Schedule(AInvokeable, AData, ADelays,
                                    AInvokeCallback, AScheduleCallback,
                                    AOptions, APriority);
end;

procedure TXRTLASyncDefaultContextManager.CancelAll;
var
  I: Integer;
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  FDefaultContext.CancelAll;
  for I:= 0 to FContextList.GetSize - 1 do
  begin
    (XRTLGetAsObject(FContextList.GetValue(I)) as TXRTLASyncContextBase).CancelAll;
  end;
end;

procedure TXRTLASyncDefaultContextManager.AttachContext(AContext: TXRTLASyncContext);
var
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  if AContext.Manager <> Self then
  begin
    if Assigned(AContext.Manager) then
      AContext.Manager.DetachContext(AContext);
    FContextList.Add(XRTLValue(AContext, True));
    AContext.Manager:= Self;
  end;
end;

procedure TXRTLASyncDefaultContextManager.DetachContext(AContext: TXRTLASyncContext);
var
  LValue: IXRTLValue;
  LIterator: IXRTLIterator;
  FAutoLock: IInterface;
begin
  FAutoLock:= XRTLBeginWriteLock(FLock);
  if AContext.Manager = Self then
  begin
    LValue:= XRTLValue(AContext);
    if FContextList.Find(LValue, LIterator) then
    begin
      XRTLGetAsObject(FContextList.GetValue(LIterator), True);
      FContextList.Remove(LIterator);
    end;
    AContext.Manager:= nil;
  end;
end;

function TXRTLASyncDefaultContextManager.CreateMultiThreadContext(
  AAttachContext: Boolean = True; AThreadPerCPUMin: Byte = 0;
  AThreadPerCPUMax: Byte = 0; AAffinityMask: Cardinal = $FFFFFFFF): TXRTLASyncMultiThreadContextBase;
begin
  if AThreadPerCPUMin = 0 then
    AThreadPerCPUMin:= FDefaultThreadPerCPUMin;
  if AThreadPerCPUMax = 0 then
    AThreadPerCPUMax:= FDefaultThreadPerCPUMax;
  AAffinityMask:= AAffinityMask and FDefaultAffinityMask;
  if AAffinityMask = 0 then
    AAffinityMask:= FDefaultAffinityMask;
  Result:= TXRTLASyncMultiThreadContext.Create(AAffinityMask);
  Result.ThreadPerCPUMin:= AThreadPerCPUMin;
  Result.ThreadPerCPUMax:= AThreadPerCPUMax;
  if AAttachContext then
    AttachContext(Result);
end;

function TXRTLASyncDefaultContextManager.CreateSingleThreadContext(AAttachContext: Boolean = True): TXRTLASyncContext;
begin
  Result:= TXRTLASyncSingleThreadContext.Create;
  if AAttachContext then
    AttachContext(Result);
end;

end.


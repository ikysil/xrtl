program ASyncDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  xrtl_util_Value,
  xrtl_util_async_Core;

var
  Start: TDateTime;

procedure ASyncProc(const ATask: IXRTLASyncTask);
var
  S: string;
begin
  S:= '';
  if ATask.IsCanceled then
    S:= 'canceled';
  WriteLn(Format('%s %.3d Task %s', [FormatDateTime('nn:ss.zzz', Now - Start), XRTLGetAsInteger(ATask.GetData), S]));
//  XRTLASyncManager.CancelAll;
  Sleep(2000);
  XRTLASyncContextManager.Schedule(XRTLASyncInvokeable(ASyncProc),
                                   XRTLValue(Integer(25)),
                                   XRTLASyncDelays([1000]));
end;

procedure ASyncProcCallback(const ATask: IXRTLASyncTask);
var
  S: string;
begin
  S:= '';
  if ATask.IsCanceled then
    S:= 'canceled';
  WriteLn(Format('%s %.3d Callback %s', [FormatDateTime('nn:ss.zzz', Now - Start), XRTLGetAsInteger(ATask.GetData), S]));
end;

procedure ASyncProcScheduleCallback(const ATask: IXRTLASyncTask);
var
  S: string;
begin
  S:= '';
  if ATask.IsCanceled then
    S:= 'canceled';
  WriteLn(Format('%s %.3d ScheduleCallback %s', [FormatDateTime('nn:ss.zzz', Now - Start), XRTLGetAsInteger(ATask.GetData), S]));
  Sleep(2000);
end;

var
  I: Integer;
  LTask: IXRTLASyncTask;
begin
  Start:= Now;
{
  for I:= 127 downto 0 do
  begin
    LTask:= XRTLASyncContextManager.Invoke(XRTLASyncInvokeable(ASyncProc), Pointer(I),
                                    XRTLASyncInvokeable(ASyncProcCallback), []);
    if I mod 4 = 0 then
      LTask.Cancel;
  end;
  while XRTLASyncManager.ThreadBusyCount > 0 do
  begin
    Sleep(100);
    WriteLn(Format('%s %.3d %.3d', [FormatDateTime('nn:ss.zzz', Now - Start),
                                    XRTLASyncManager.ThreadTotalCount,
                                    XRTLASyncManager.ThreadBusyCount]));
  end;
  WriteLn('Press Enter to exit...');
//}
//{
  LTask:= XRTLASyncContextManager.Schedule(XRTLASyncInvokeable(ASyncProc),
                                    XRTLValue(Integer(25)),
                                    XRTLASyncDelays([1000]),
                                    XRTLASyncInvokeable(ASyncProcCallback),
                                    XRTLASyncInvokeable(ASyncProcScheduleCallback),
                                    []);//aoRepeatable]);
//}
//  XRTLASyncManager.CancelAll;
//  LTask.Cancel;
//  Sleep(17000);
  ReadLn;
end.

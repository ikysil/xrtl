unit xrtl_math_Stack;

{$INCLUDE xrtl.inc}

interface

uses
  Classes, Contnrs, SysUtils,
  xrtl_util_Exception,
  xrtl_math_Integer;

type
  TXRTLStack = class
  private
    FStack: TObjectList;
    function   GetStack(Index: Integer): TXRTLInteger;
  protected
    procedure  EnsureDepth(const ADepth: Integer); virtual;
    procedure  PushResult(const AInteger: TXRTLInteger); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property   Stack[Index: Integer]: TXRTLInteger read GetStack;
    procedure  Push(const AInteger: TXRTLInteger); overload;
    procedure  Push(const AInteger: Int64); overload;
    procedure  Pop(var AResult: TXRTLInteger);
    procedure  Add;
  end;

implementation

type
  TXRTLStackItem = class
  public
    Value: TXRTLInteger;
    constructor Create(const AValue: TXRTLInteger);
    destructor Destroy; override;
  end;

{ TXRTLStackItem }

constructor TXRTLStackItem.Create(const AValue: TXRTLInteger);
begin
  inherited Create;
  XRTLAssign(AValue, Value);
end;

destructor TXRTLStackItem.Destroy;
begin
  XRTLZero(Value);
  inherited;
end;

{ TXRTLStack }

constructor TXRTLStack.Create;
begin
  inherited;
  FStack:= TObjectList.Create(True);
end;

destructor TXRTLStack.Destroy;
begin
  FreeAndNil(FStack);
  inherited;
end;

procedure TXRTLStack.EnsureDepth(const ADepth: Integer);
begin
  if ADepth > FStack.Count then
    raise EXRTLException.Create('Stack depth is not enough');
end;

function TXRTLStack.GetStack(Index: Integer): TXRTLInteger;
begin
  EnsureDepth(Index + 1);
  XRTLAssign((FStack[Index] as TXRTLStackItem).Value, Result);
end;

procedure TXRTLStack.PushResult(const AInteger: TXRTLInteger);
begin
  FStack.Insert(0, TXRTLStackItem.Create(AInteger));
end;

procedure TXRTLStack.Push(const AInteger: TXRTLInteger);
begin
  PushResult(AInteger);
end;

procedure TXRTLStack.Push(const AInteger: Int64);
var
  AInt: TXRTLInteger;
begin
  XRTLAssign(AInteger, AInt);
  PushResult(AInt);
end;

procedure TXRTLStack.Pop(var AResult: TXRTLInteger);
begin
  EnsureDepth(1);
  XRTLAssign((FStack[0] as TXRTLStackItem).Value, AResult);
  FStack.Delete(0);
end;

procedure TXRTLStack.Add;
var
  AInt1, AInt2, ARes: TXRTLInteger;
begin
  EnsureDepth(2);
  Pop(AInt2);
  Pop(AInt1);
  XRTLAdd(AInt1, AInt2, ARes);
  PushResult(ARes);
end;

end.

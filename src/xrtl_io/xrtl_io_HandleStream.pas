unit xrtl_io_HandleStream;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils,
  xrtl_io_Stream;

type
  TXRTLHandleInputStream = class(TXRTLInputStream)
  private
    FHandle: THandle;
    FOwnHandle: Boolean;
  protected
    procedure  DoClose; override;
    function   _ReadBuffer(var Buffer; const Count: Integer): Integer; override;
  public
    constructor Create(const AHandle: THandle; AOwnHandle: Boolean = True);
    property   Handle: THandle read FHandle;
    property   OwnHandle: Boolean read FOwnHandle write FOwnHandle;
  end;

  TXRTLHandleOutputStream = class(TXRTLOutputStream)
  private
    FHandle: THandle;
    FOwnHandle: Boolean;
  protected
    procedure  DoClose; override;
    procedure  _WriteBuffer(const Buffer; const Count: Integer); override;
  public
    constructor Create(const AHandle: THandle; AOwnHandle: Boolean = True);
    property   Handle: THandle read FHandle;
    property   OwnHandle: Boolean read FOwnHandle write FOwnHandle;
  end;

implementation

{ TXRTLHandleInputStream }

constructor TXRTLHandleInputStream.Create(const AHandle: THandle; AOwnHandle: Boolean = True);
begin
  inherited Create;
  FHandle:= AHandle;
  FOwnHandle:= AOwnHandle;
end;

procedure TXRTLHandleInputStream.DoClose;
begin
  inherited;
  if FOwnHandle then
  begin
    CloseHandle(FHandle);
    FOwnHandle:= False;
  end;
  FHandle:= $FFFFFFFF;
end;

function TXRTLHandleInputStream._ReadBuffer(var Buffer; const Count: Integer): Integer;
var
  RCount: Cardinal;
begin
  Win32Check(ReadFile(FHandle, Buffer, Count, RCount, nil));
  if (RCount = 0) then
    Result:= XRTLEndOfStreamValue
  else
    Result:= RCount;
end;

{ TXRTLHandleOutputStream }

constructor TXRTLHandleOutputStream.Create(const AHandle: THandle; AOwnHandle: Boolean = True);
begin
  inherited Create;
  FHandle:= AHandle;
  FOwnHandle:= AOwnHandle;
end;

procedure TXRTLHandleOutputStream.DoClose;
begin
  inherited;
  if FOwnHandle then
  begin
    CloseHandle(FHandle);
    FOwnHandle:= False;
  end;
  FHandle:= $FFFFFFFF;
end;

procedure TXRTLHandleOutputStream._WriteBuffer(const Buffer; const Count: Integer);
var
  WCount: Cardinal;
begin
  Win32Check(WriteFile(FHandle, Buffer, Count, WCount, nil));
end;

end.

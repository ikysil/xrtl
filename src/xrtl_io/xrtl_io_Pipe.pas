unit xrtl_io_Pipe;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, 
  xrtl_io_Stream, xrtl_io_HandleStream;

type
  TXRTLPipe = class
  private
    FInputHandle: THandle;
    FInputStream: TXRTLInputStream;
    FOutputHandle: THandle;
    FOutputStream: TXRTLOutputStream;
    function   GetInputStream: TXRTLInputStream;
    function   GetOutputStream: TXRTLOutputStream;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property   InputStream: TXRTLInputStream read GetInputStream;
    property   OutputStream: TXRTLOutputStream read GetOutputStream;
  end;

implementation

type
  TXRTLPipeOutputStream = class(TXRTLHandleOutputStream)
  public
    procedure  Flush; override;
  end;

{ TXRTLPipeOutputStream }

procedure TXRTLPipeOutputStream.Flush;
begin
  inherited;
  Win32Check(FlushFileBuffers(Handle));
end;

{ TXRTLPipe }

constructor TXRTLPipe.Create;
begin
  inherited Create;
  FInputStream:= nil;
  FOutputStream:= nil;
  Win32Check(CreatePipe(FInputHandle, FOutputHandle, nil, 0));
end;

destructor TXRTLPipe.Destroy;
begin
  FreeAndNil(FOutputStream);
  FreeAndNil(FInputStream);
  CloseHandle(FInputHandle);
  CloseHandle(FOutputHandle);
  inherited;
end;

function TXRTLPipe.GetInputStream: TXRTLInputStream;
begin
  if not Assigned(FInputStream) then
    FInputStream:= TXRTLHandleInputStream.Create(FInputHandle, False);
  Result:= FInputStream;
end;

function TXRTLPipe.GetOutputStream: TXRTLOutputStream;
begin
  if not Assigned(FOutputStream) then
    FOutputStream:= TXRTLPipeOutputStream.Create(FOutputHandle, False);
  Result:= FOutputStream;
end;

end.

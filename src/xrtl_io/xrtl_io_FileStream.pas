unit xrtl_io_FileStream;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Math,
  xrtl_util_Exception,
  xrtl_io_StreamUtils, xrtl_io_Stream, xrtl_io_HandleStream;

type
  TXRTLFileInputStream = class(TXRTLHandleInputStream)
  private
    FOwnFile: Boolean;
    FFilePath: WideString;
    procedure  SetPosition(const Value: Int64);
    function   GetPosition: Int64;
  protected
    procedure  DoClose; override;
  public
    constructor Create(const AFilePath: WideString; AOwnFile: Boolean = False);
    property   FilePath: WideString read FFilePath;
    property   OwnFile: Boolean read FOwnFile write FOwnFile;
    property   Position: Int64 read GetPosition write SetPosition;
    function   BytesAvailable: Int64; override;
    function   MarkPosition: TXRTLMarkData; override;
    procedure  RestorePosition(const MarkData: TXRTLMarkData); override;
    function   Skip(const Count: Int64): Int64; override;
  end;

  TXRTLFileOutputStream = class(TXRTLHandleOutputStream)
  private
    FOwnFile: Boolean;
    FFilePath: WideString;
  protected
    procedure  DoClose; override;
  public
    constructor Create(AFilePath: WideString; AOwnFile: Boolean = False);
    property   FilePath: WideString read FFilePath;
    property   OwnFile: Boolean read FOwnFile write FOwnFile;
    procedure  Flush; override;
  end;

  TXRTLTempFileOutputStream = class(TXRTLFileOutputStream)
  private
  public
    constructor Create(ADirectory: WideString = ''; AOwnFile: Boolean = True);
  end;

implementation

type
  TXRTLFileInputStreamMarkData = class(TXRTLMarkData)
  public
    Position: Int64;
  end;

{ TXRTLFileInputStream }

constructor TXRTLFileInputStream.Create(const AFilePath: WideString; AOwnFile: Boolean = False);
var
  H: THandle;
begin
  H:= CreateFileW(PWideChar(AFilePath), GENERIC_READ,
                  FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if H = INVALID_HANDLE_VALUE then
    XRTLRaiseError;
  inherited Create(H, True);
  FFilePath:= AFilePath;
  FOwnFile:= AOwnFile;
end;

function TXRTLFileInputStream.GetPosition: Int64;
begin
  Result:= 0;
  Int64Rec(Result).Lo:= SetFilePointer(Handle, 0, @Int64Rec(Result).Hi, FILE_CURRENT);
  if Int64Rec(Result).Lo = $FFFFFFFF then XRTLRaiseError;
end;

procedure TXRTLFileInputStream.SetPosition(const Value: Int64);
var
  LValue: Int64;
begin
  LValue:= Value;
  Int64Rec(LValue).Lo:= SetFilePointer(Handle, Int64Rec(LValue).Lo, @Int64Rec(LValue).Hi, FILE_BEGIN);
  if Int64Rec(LValue).Lo = $FFFFFFFF then XRTLRaiseError;
end;

function TXRTLFileInputStream.BytesAvailable: Int64;
var
  CPos, CSize: Int64;
begin
  CPos:= GetPosition;
  Int64Rec(CSize).Lo:= GetFileSize(Handle, @Int64Rec(CSize).Hi);
  if Int64Rec(CSize).Lo = $FFFFFFFF then XRTLRaiseError;
  Result:= CSize - CPos;
end;

procedure TXRTLFileInputStream.DoClose;
begin
  inherited;
  if FOwnFile then
  begin
    Win32Check(Windows.DeleteFileW(PWideChar(FFilePath)));
    FOwnFile:= False;
  end;
end;

function TXRTLFileInputStream.MarkPosition: TXRTLMarkData;
var
  MD: TXRTLFileInputStreamMarkData;
begin
  MD:= TXRTLFileInputStreamMarkData.Create(Self);
  MD.Position:= Position;
  Result:= MD;
end;

procedure TXRTLFileInputStream.RestorePosition(const MarkData: TXRTLMarkData);
var
  MD: TXRTLFileInputStreamMarkData;
begin
  MarkData.CheckOwner(Self);
  MD:= MarkData as TXRTLFileInputStreamMarkData;
  Position:= MD.Position;
end;

function TXRTLFileInputStream.Skip(const Count: Int64): Int64;
var
  CPos, NPos: Int64;
begin
  CPos:= Position;
  NPos:= Min(Count, BytesAvailable);
  Int64Rec(NPos).Lo:= SetFilePointer(Handle, Int64Rec(NPos).Lo, @Int64Rec(NPos).Hi, FILE_CURRENT);
  if Int64Rec(NPos).Lo = $FFFFFFFF then XRTLRaiseError;
  Result:= NPos - CPos;
end;

{ TXRTLFileOutputStream }

constructor TXRTLFileOutputStream.Create(AFilePath: WideString; AOwnFile: Boolean = False);
var
  H: THandle;
begin
  H:= CreateFileW(PWideChar(AFilePath), GENERIC_WRITE,
                  FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if H = INVALID_HANDLE_VALUE then
    XRTLRaiseError;
  inherited Create(H, True);
  FFilePath:= AFilePath;
  FOwnFile:= AOwnFile;
end;

procedure TXRTLFileOutputStream.DoClose;
begin
  inherited;
  if FOwnFile then
  begin
    Win32Check(Windows.DeleteFileW(PWideChar(FFilePath)));
    FOwnFile:= False;
  end;
end;

procedure TXRTLFileOutputStream.Flush;
begin
  inherited;
  Win32Check(FlushFileBuffers(Handle));
end;

{ TXRTLTempFileOutputStream }

constructor TXRTLTempFileOutputStream.Create(ADirectory: WideString = ''; AOwnFile: Boolean = True);
begin
  inherited Create(XRTLGetTempFileName(ADirectory, 'TFS'), AOwnFile);
end;

end.

unit OPCDA20CTServer_Device;

interface

uses
  Windows, ActiveX, SysUtils,
  xrtl_util_COMUtils, xrtl_util_TimeStamp,
  xrtl_opc_DA,
  xrtl_opc_sdk_OPCDADataSource;

type
  TOPCDA20CTServerDevice = class(TXRTLOPCDADataSource)
  private
    FTimeStamp: TXRTLTimeStamp;
  public
    constructor Create;
    destructor Destroy; override;
    function   Read(szItemID: POLEStr; out Value: OleVariant; out Quality: Word; out TimeStamp: TFileTime): HResult; override;
    function   Write(szItemID: POLEStr; Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult; override;
  end;

implementation

{ TOPCDA20CTServerDevice }

constructor TOPCDA20CTServerDevice.Create;
begin
  inherited;
  FTimeStamp:= TXRTLTimeStamp.Create;
end;

destructor TOPCDA20CTServerDevice.Destroy;
begin
  FreeAndNil(FTimeStamp);
  inherited;
end;

function TOPCDA20CTServerDevice.Read(szItemID: POLEStr;
  out Value: OleVariant; out Quality: Word;
  out TimeStamp: TFileTime): HResult;
begin
  Result:= S_OK;
  try
    FTimeStamp.SetCurrentTime;
    TimeStamp:= FTimeStamp.UTCFileTime;
    Value:= Variant((Round(FTimeStamp.UTCDateTime * 1024 * 1024) mod (1024 * 1024)) / (8 * 1024));
    Quality:= OPC_QUALITY_GOOD;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TOPCDA20CTServerDevice.Write(szItemID: POLEStr;
  Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult;
begin
  Result:= S_OK;
end;

end.

unit OPCDA20Server_Device;

interface

uses
  Windows, ActiveX, SysUtils,
  xrtl_util_COMUtils, xrtl_util_TimeStamp,
  xrtl_opc_DA,
  xrtl_opc_sdk_OPCDADataSource;

type
  TOPCDA20ServerDevice = class(TXRTLOPCDADataSource)
  private
    FTimeStamp: TXRTLTimeStamp;
  public
    constructor Create;
    destructor Destroy; override;
    function   Read(szItemID: POLEStr; out Value: OleVariant; out Quality: Word; out TimeStamp: TFileTime): HResult; override;
    function   Write(szItemID: POLEStr; Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult; override;
  end;

implementation

{ TOPCDA20ServerDevice }

constructor TOPCDA20ServerDevice.Create;
begin
  inherited;
  FTimeStamp:= TXRTLTimeStamp.Create;
end;

destructor TOPCDA20ServerDevice.Destroy;
begin
  FreeAndNil(FTimeStamp);
  inherited;
end;

function TOPCDA20ServerDevice.Read(szItemID: POLEStr;
  out Value: OleVariant; out Quality: Word;
  out TimeStamp: TFileTime): HResult;
begin
  Result:= S_OK;
  try
    FTimeStamp.SetCurrentTime;
    TimeStamp:= FTimeStamp.UTCFileTime;
    Value:= Variant((Round(FTimeStamp.UTCDateTime * 1024 * 1024) mod (1024 * 1024)) / (4 * 1024));
    Quality:= OPC_QUALITY_GOOD;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TOPCDA20ServerDevice.Write(szItemID: POLEStr;
  Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult;
begin
  Result:= S_OK;
end;

end.

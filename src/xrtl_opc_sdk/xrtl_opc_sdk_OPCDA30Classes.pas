unit xrtl_opc_sdk_OPCDA30Classes;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_COMUtils,
  xrtl_opc_DA, xrtl_opc_Error, xrtl_opc_Types;

type
  TXRTLOPCDA30_IOPCItemIO = class(TAggregatedObject, IOPCItemIO)
  public
    function   Read(dwCount: DWORD; pszItemIDs: POleStrList; pdwMaxAge: PDWORDARRAY;
                    out ppvValues: POleVariantArray; out ppwQualities: PWordArray;
                    out ppftTimeStamps: PFileTimeArray; out ppErrors: PResultList): HResult; stdcall;
    function   WriteVQT(dwCount: DWORD; pszItemIDs: POleStrList; pItemVQT: POPCITEMVQTARRAY;
                        out ppErrors: PResultList): HResult; stdcall;
  end;

implementation

{ TXRTLOPCDA30_IOPCItemIO }

function TXRTLOPCDA30_IOPCItemIO.Read(dwCount: DWORD;
  pszItemIDs: POleStrList; pdwMaxAge: PDWORDARRAY;
  out ppvValues: POleVariantArray; out ppwQualities: PWordArray;
  out ppftTimeStamps: PFileTimeArray; out ppErrors: PResultList): HResult;
begin
  Result:= S_OK;
  try
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDA30_IOPCItemIO.WriteVQT(dwCount: DWORD;
  pszItemIDs: POleStrList; pItemVQT: POPCITEMVQTARRAY;
  out ppErrors: PResultList): HResult;
begin
  Result:= S_OK;
  try
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

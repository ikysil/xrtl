unit xrtl_opc_Utils;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, ActiveX,
  SysUtils,
  xrtl_util_COMUtils,
  xrtl_opc_Common, xrtl_opc_Types, xrtl_opc_DA;//, xrtl_opc_HDA;


procedure XRTLFreeOPCHANDLEARRAY(var Value: POPCHANDLEARRAY);
procedure XRTLFreeVarTypeList(var Value: PVarTypeList);
procedure XRTLFreeOleVariantArray(var Value: POleVariantArray; dwCount: DWORD);
procedure XRTLFreeBOOLARRAY(var Value: PBOOLARRAY);
procedure XRTLFreeDWORDARRAY(var Value: PDWORDARRAY);
procedure XRTLFreeWORDARRAY(var Value: PWORDARRAY);
procedure XRTLFreeSingleArray(var Value: PSingleArray);
procedure XRTLFreeFileTimeArray(var Value: PFileTimeArray);
procedure XRTLFreeOleStrList(var Value: POleStrList; dwCount: DWORD);
procedure XRTLFreeResultList(var Value: PResultList);
procedure XRTLFreeLCIDARRAY(var Value: PLCIDARRAY);

procedure XRTLFreeOPCITEMSTATEARRAY(var Value: POPCITEMSTATEARRAY; dwCount: DWORD);
procedure XRTLFreeOPCSERVERSTATUS(var Value: POPCSERVERSTATUS);
procedure XRTLFreeOPCITEMDEFARRAY(var Value: POPCITEMDEFARRAY; dwCount: DWORD);
procedure XRTLFreeOPCITEMATTRIBUTESARRAY(var Value: POPCITEMATTRIBUTESARRAY; dwCount: DWORD);
procedure XRTLFreeOPCITEMRESULTARRAY(var Value: POPCITEMRESULTARRAY; dwCount: DWORD);
procedure XRTLFreeOPCITEMPROPERTYARRAY(var Value: POPCITEMPROPERTYARRAY; dwCount: DWORD);
procedure XRTLFreeOPCITEMPROPERTIESARRAY(var Value: POPCITEMPROPERTIESARRAY; dwCount: DWORD);
procedure XRTLFreeOPCBROWSEELEMENTARRAY(var Value: POPCBROWSEELEMENTARRAY; dwCount: DWORD);
procedure XRTLFreeOPCITEMVQTARRAY(var Value: POPCITEMVQTARRAY; dwCount: DWORD);

implementation

procedure XRTLFreeOPCHANDLEARRAY(var Value: POPCHANDLEARRAY);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeVarTypeList(var Value: PVarTypeList);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOleVariantArray(var Value: POleVariantArray; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    VarClear(Value[I]);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeBOOLARRAY(var Value: PBOOLARRAY);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeDWORDARRAY(var Value: PDWORDARRAY);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeWORDARRAY(var Value: PWORDARRAY);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeSingleArray(var Value: PSingleArray);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeFileTimeArray(var Value: PFileTimeArray);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOleStrList(var Value: POleStrList; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOutWideString(Value[I]);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeResultList(var Value: PResultList);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeLCIDARRAY(var Value: PLCIDARRAY);
begin
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMSTATEARRAY(var Value: POPCITEMSTATEARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    VarClear(Value[I].vDataValue);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCSERVERSTATUS(var Value: POPCSERVERSTATUS);
begin
  if not Assigned(Value) then Exit;
  XRTLFreeOutWideString(Value.szVendorInfo);
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMDEFARRAY(var Value: POPCITEMDEFARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOutWideString(Value[I].szAccessPath);
    XRTLFreeOutWideString(Value[I].szItemID);
    CoTaskMemFree(Value[I].pBlob);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMATTRIBUTESARRAY(var Value: POPCITEMATTRIBUTESARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOutWideString(Value[I].szAccessPath);
    XRTLFreeOutWideString(Value[I].szItemID);
    CoTaskMemFree(Value[I].pBlob);
    VarClear(Value[I].vEUInfo);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMRESULTARRAY(var Value: POPCITEMRESULTARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    CoTaskMemFree(Value[I].pBlob);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMPROPERTYARRAY(var Value: POPCITEMPROPERTYARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOutWideString(Value[I].szItemID);
    XRTLFreeOutWideString(Value[I].szDescription);
    VarClear(Value[I].vValue);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMPROPERTIESARRAY(var Value: POPCITEMPROPERTIESARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOPCITEMPROPERTYARRAY(Value[I].pItemProperties, Value[I].dwNumProperties);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCBROWSEELEMENTARRAY(var Value: POPCBROWSEELEMENTARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    XRTLFreeOutWideString(Value[I].szName);
    XRTLFreeOutWideString(Value[I].szItemID);
    XRTLFreeOPCITEMPROPERTYARRAY(Value[I].ItemProperties.pItemProperties, Value[I].ItemProperties.dwNumProperties)
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeOPCITEMVQTARRAY(var Value: POPCITEMVQTARRAY; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
    VarClear(Value[I].vDataValue);
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;

{
procedure XRTLFreeXXX(var Value: Pointer);
begin
  if not Assigned(Value) then Exit;
//
  CoTaskMemFree(Value);
  Value:= nil;
end;

procedure XRTLFreeXXXARRAY(var Value: Pointer; dwCount: DWORD);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;
  for I:= 0 to dwCount - 1 do
  begin
//
  end;
  CoTaskMemFree(Value);
  Value:= nil;
end;
}

end.

unit xrtl_opc_sdk_OPCDAVariantManager;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_Type, xrtl_util_Compat,
  xrtl_util_COMUtils,
  xrtl_opc_Error,
  xrtl_opc_sdk_DA;

type
  TXRTLOPCDAVariantManager = class(TInterfacedObject, IXRTLOPCDAVariantManager)
  private
    function   DoConvert(out Dest: OLEVariant; vtDestType: DWORD;
                         Src: OLEVariant; vtSrcType: DWORD; dwLocaleID: TLCID): HResult;
    function   GetVarArrayElementSize(vtDataType: DWORD): Cardinal;
    procedure  VariantFromPointer(var Value: OLEVariant; vtDataType: DWORD;
                                  var ArrayData: Pointer; ElemSize: Integer);
    procedure  VariantToPointer(Value: OLEVariant; var ArrayData: Pointer; ElemSize: Integer);
    function   LVariantChangeType(out DestValue: OLEVariant; vtDestType: DWORD;
                                  SrcValue: OLEVariant; vtSrcType: DWORD; dwLocaleID: TLCID): HResult;
  public
    function   IsTypeSupported(vtDataType: DWORD; out bResult: BOOL): HResult; stdcall;
    function   Convert(out Dest: OLEVariant; vtDestType: DWORD; Src: OLEVariant; dwLocaleID: TLCID): HResult; stdcall;
    function   CanConvert(vtSrcDataType, vtDestDataType: DWORD; out bResult: BOOL): HResult; stdcall;
    function   VarEqual(Var1, Var2: OLEVariant; out bResult: BOOL): HResult; stdcall;
  end;

var
  XRTLOPCDAVariantManager: IXRTLOPCDAVariantManager = nil;

implementation

{ TXRTLOPCDAVariantManager }

function TXRTLOPCDAVariantManager.GetVarArrayElementSize(vtDataType: DWORD): Cardinal;
begin
  Result:= 0;
  case vtDataType of
    VT_BOOL:       Result:= SizeOf(WordBool);
    VT_I1, VT_UI1: Result:= SizeOf(Byte);
    VT_I2, VT_UI2: Result:= SizeOf(SmallInt);
    VT_I4, VT_UI4: Result:= SizeOf(Integer);
    VT_R4:         Result:= SizeOf(Single);
    VT_R8:         Result:= SizeOf(Double);
    VT_DATE:       Result:= SizeOf(TDateTime);
    VT_BSTR:       Result:= SizeOf(PWideChar);
    VT_CY:         Result:= SizeOf(Currency);
  else
    OLEError(OPC_E_BADTYPE);
  end;
end;

procedure TXRTLOPCDAVariantManager.VariantFromPointer(var Value: OLEVariant;
  vtDataType: DWORD; var ArrayData: Pointer; ElemSize: Integer);
begin
  VarClear(Value);
  case vtDataType of
    VT_BOOL:       Value:= PWordBool(ArrayData)^;
    VT_I1, VT_UI1: Value:= PByte(ArrayData)^;
    VT_I2, VT_UI2: Value:= PSmallInt(ArrayData)^;
    VT_I4, VT_UI4: Value:= PInteger(ArrayData)^;
    VT_R4:         Value:= PSingle(ArrayData)^;
    VT_R8:         Value:= PDouble(ArrayData)^;
    VT_DATE:       Value:= PDateTime(ArrayData)^;
    VT_BSTR:       Value:= PWideChar(ArrayData)^;
    VT_CY:         Value:= PCurrency(ArrayData)^;
  else
    OLEError(OPC_E_BADTYPE);
  end;
  ArrayData:= Pointer(Integer(ArrayData) + ElemSize);
end;

procedure TXRTLOPCDAVariantManager.VariantToPointer(Value: OLEVariant;
  var ArrayData: Pointer; ElemSize: Integer);
begin
  case VarType(Value) and varTypeMask of
    VT_BOOL:       PWordBool(ArrayData)^:= Value;
    VT_I1, VT_UI1: PByte(ArrayData)^:= Value;
    VT_I2, VT_UI2: PSmallInt(ArrayData)^:= Value;
    VT_I4, VT_UI4: PInteger(ArrayData)^:= Value;
    VT_R4:         PSingle(ArrayData)^:= Value;
    VT_R8:         PDouble(ArrayData)^:= Value;
    VT_DATE:       PDateTime(ArrayData)^:= Value;
    VT_BSTR:       WideString(ArrayData^):= Value;
    VT_CY:         PCurrency(ArrayData)^:= Value;
  else
    OLEError(OPC_E_BADTYPE);
  end;
  ArrayData:= Pointer(Integer(ArrayData) + ElemSize);
end;

function TXRTLOPCDAVariantManager.IsTypeSupported(vtDataType: DWORD; out bResult: BOOL): HResult;
begin
  try
    XRTLCheckOutArgument(bResult);
    bResult:= (vtDataType and varTypeMask) in [VT_EMPTY,
                                               VT_BOOL,
                                               VT_I1, VT_UI1,
                                               VT_I2, VT_UI2,
                                               VT_I4, VT_UI4,
                                               VT_R4, VT_R8,
                                               VT_DATE,
                                               VT_BSTR,
                                               VT_CY];
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAVariantManager.LVariantChangeType(out DestValue: OLEVariant;
  vtDestType: DWORD; SrcValue: OLEVariant; vtSrcType: DWORD; dwLocaleID: TLCID): HResult;
begin
  Result:= VariantChangeTypeEx(DestValue, SrcValue, dwLocaleID, 0, vtDestType);
  if (Result = DISP_E_TYPEMISMATCH) and (vtDestType = VT_DATE) then
    Result:= DISP_E_OVERFLOW;
end;

function TXRTLOPCDAVariantManager.DoConvert(out Dest: OLEVariant; vtDestType: DWORD; Src: OLEVariant; vtSrcType: DWORD; dwLocaleID: TLCID): HResult;
var
  I, DimCount, DimElemCount, ElemCount, SrcElemSize, DestElemSize: Integer;
  ArrayDims: array of Integer;
  vtSrcElemType, vtDestElemType: DWORD;
  SrcArrayData, DestArrayData: Pointer;
  SrcValue, DestValue: OLEVariant;
begin
  Result:= S_OK;
  try
    if vtDestType = vtSrcType then
    begin
      OLECheck(VariantCopy(Dest, Src));
      Exit;
    end;
    if ((vtDestType and varArray) = 0) and ((vtSrcType and varArray) = 0) then
    begin
      OLECheck(LVariantChangeType(Dest, vtDestType, Src, vtSrcType, dwLocaleID));
      Exit;
    end;
    DimCount:= VarArrayDimCount(Src);
    if DimCount > 0 then
    begin
      SetLength(ArrayDims, DimCount * 2);
      if DimCount > 0 then
        ElemCount:= 1
      else
        ElemCount:= 0;
      for I:= 0 to DimCount - 1 do
      begin
        DimElemCount:= VarArrayHighBound(Src, I + 1) - VarArrayLowBound(Src, I + 1) + 1;
        ElemCount:= ElemCount * DimElemCount;
        ArrayDims[I * 2]:= 0;
        ArrayDims[I * 2 + 1]:= DimElemCount - 1;
      end;
      vtSrcElemType:= vtSrcType and varTypeMask;
      SrcElemSize:= GetVarArrayElementSize(vtSrcElemType);
      vtDestElemType:= vtDestType and varTypeMask;
      DestElemSize:= GetVarArrayElementSize(vtDestElemType);
      Dest:= VarArrayCreate(ArrayDims, vtDestElemType);
      try
        SrcArrayData:= VarArrayLock(Src);
        DestArrayData:= VarArrayLock(Dest);
        for I:= 0 to ElemCount - 1 do
        begin
          VariantFromPointer(SrcValue, vtSrcElemType, SrcArrayData, SrcElemSize);
          OLECheck(LVariantChangeType(DestValue, vtDestElemType, SrcValue, vtSrcElemType, dwLocaleID));
          VariantToPointer(DestValue, DestArrayData, DestElemSize);
        end;
      finally
        VarArrayUnlock(Dest);
        VarArrayUnlock(Src);
      end;
    end
    else
    begin
      SetLength(ArrayDims, 2);
      ArrayDims[0]:= 0;
      ArrayDims[1]:= 0;
      vtDestElemType:= vtDestType and varTypeMask;
      Dest:= VarArrayCreate(ArrayDims, vtDestElemType);
      OLECheck(LVariantChangeType(DestValue, vtDestElemType, Src, vtSrcType, dwLocaleID));
      Dest[0]:= DestValue;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAVariantManager.Convert(out Dest: OLEVariant; vtDestType: DWORD; Src: OLEVariant; dwLocaleID: TLCID): HResult;
var
  LvtDestType, LvtSrcType: DWORD;
  bResult: BOOL;
begin
  try
    XRTLCheckOutArgument(Dest);
    LvtSrcType:= VarType(Src);
    LvtDestType:= vtDestType;
    if LvtDestType = VT_EMPTY then
      LvtDestType:= LvtSrcType;
    OLECheck(CanConvert(LvtSrcType, LvtDestType, bResult));
    if not bResult then
      OLEError(OPC_E_BADTYPE);
    Result:= DoConvert(Dest, LvtDestType, Src, LvtSrcType, dwLocaleID);
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAVariantManager.CanConvert(vtSrcDataType, vtDestDataType: DWORD; out bResult: BOOL): HResult;
var
  IsSrcArray, IsDestArray: Boolean;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(bResult);
    if (vtSrcDataType <> vtDestDataType) and (vtSrcDataType = VT_EMPTY) then
    begin
      bResult:= False;
      Exit;
    end;
    OLECheck(IsTypeSupported(vtSrcDataType, bResult));
    if not bResult then
      Exit;
    OLECheck(IsTypeSupported(vtDestDataType, bResult));
    if not bResult then
      Exit;
    IsSrcArray:= (vtSrcDataType and varArray) = varArray;
    IsDestArray:= (vtDestDataType and varArray) = varArray;
    if not (IsSrcArray xor IsDestArray) then
    begin
      bResult:= True;
      Exit;
    end;
    if IsSrcArray and not IsDestArray then
    begin
      bResult:= vtDestDataType = VT_EMPTY;
      Exit;
    end;
    if IsDestArray and not IsSrcArray then
    begin
      bResult:= True;
      Exit;
    end;
    bResult:= False;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDAVariantManager.VarEqual(Var1, Var2: OLEVariant; out bResult: BOOL): HResult;

  function GetElemCount(V: OLEVariant): Integer;
  var
    DimCount, DimElemCount, I: Integer;
  begin
    Result:= 0;
    DimCount:= VarArrayDimCount(V);
    if DimCount > 0 then
    begin
      Result:= 1;
      for I:= 0 to DimCount - 1 do
      begin
        DimElemCount:= VarArrayHighBound(V, I + 1) - VarArrayLowBound(V, I + 1) + 1;
        Result:= Result * DimElemCount;
      end;
    end;
  end;

var
  LvtVar1Type, LvtVar2Type, LvtVar1ElemType, LvtVar2ElemType: DWORD;
  IsVar1Array, IsVar2Array: Boolean;
  Var1ElemCount, Var2ElemCount: Integer;
  Var1ElemSize, Var2ElemSize: Integer;
  Var1ArrayData, Var2ArrayData: Pointer;
  Var1ElemValue, Var2ElemValue: OLEVariant;
  I: Integer;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(bResult);
    bResult:= False;
// check varType
    LvtVar1Type:= VarType(Var1);
    LvtVar2Type:= VarType(Var2);
    if LvtVar1Type <> LvtVar2Type then
      Exit;
// check simple values
    IsVar1Array:= (LvtVar1Type and varArray) = varArray;
    IsVar2Array:= (LvtVar2Type and varArray) = varArray;
    if not (IsVar1Array or IsVar2Array) then
    begin
      {$IFDEF HAS_UNIT_VARIANTS}
      bResult:= VarCompareValue(Var1, Var2) = vrEqual;
      {$ELSE}
      bResult:= Var1 = Var2;
      {$ENDIF}
      Exit;
    end;
// check arrays
// check element count
    Var1ElemCount:= GetElemCount(Var1);
    Var2ElemCount:= GetElemCount(Var2);
    if Var1ElemCount <> Var2ElemCount then
      Exit;
// check element values
    LvtVar1ElemType:= LvtVar1Type and varTypeMask;
    Var1ElemSize:= GetVarArrayElementSize(LvtVar1ElemType);
    LvtVar2ElemType:= LvtVar2Type and varTypeMask;
    Var2ElemSize:= GetVarArrayElementSize(LvtVar2ElemType);
    try
      Var1ArrayData:= VarArrayLock(Var1);
      Var2ArrayData:= VarArrayLock(Var2);
      for I:= 0 to Var1ElemCount - 1 do
      begin
        VariantFromPointer(Var1ElemValue, LvtVar1ElemType, Var1ArrayData, Var1ElemSize);
        VariantFromPointer(Var2ElemValue, LvtVar2ElemType, Var2ArrayData, Var2ElemSize);
        OLECheck(VarEqual(Var1ElemValue, Var2ElemValue, bResult));
        if not bResult then
          Break;
      end;
    finally
      VarArrayUnlock(Var2);
      VarArrayUnlock(Var1);
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

initialization
begin
  XRTLOPCDAVariantManager:= TXRTLOPCDAVariantManager.Create;
end;

finalization
begin
  XRTLOPCDAVariantManager:= nil;
end;

end.


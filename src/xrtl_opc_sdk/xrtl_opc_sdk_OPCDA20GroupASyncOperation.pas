unit xrtl_opc_sdk_OPCDA20GroupASyncOperation;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Classes, COMObj, ActiveX, AxCtrls,
  xrtl_util_TimeStamp, xrtl_util_Array, xrtl_util_Value, xrtl_util_Compare, 
  xrtl_opc_DA, xrtl_opc_Types,
  xrtl_opc_sdk_OPCDAServer, xrtl_opc_sdk_OPCDA20Group;

type
  TXRTLOPCDA20GroupASyncRefresh = class(TXRTLOPCDA20GroupASyncOperation)
    procedure  DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint); override;
  end;

  TXRTLOPCDA20GroupASyncDataChange = class(TXRTLOPCDA20GroupASyncOperation)
    procedure  DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint); override;
  end;

  TXRTLOPCDA20GroupASyncRead = class(TXRTLOPCDA20GroupASyncOperation)
    phServer: POPCHANDLEARRAY;
    dwCount: DWORD;
    procedure  DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint); override;
    destructor Destroy; override;
  end;

  TXRTLOPCDA20GroupASyncWrite = class(TXRTLOPCDA20GroupASyncOperation)
    phServer: POPCHANDLEARRAY;
    dwCount: DWORD;
    pItemValues: POleVariantArray;
    procedure  DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint); override;
    destructor Destroy; override;
  end;

implementation

uses
  xrtl_opc_Utils;

{ TXRTLOPCDA20GroupASyncRefresh }

procedure TXRTLOPCDA20GroupASyncRefresh.DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
var
  ppErrors: PResultList;
  ppCallbackErrors: PResultList;
  I: Integer;
  MasterError: HResult;
  ppQualityArray: PWORDARRAY;
  ppServerItems: POPCHANDLEARRAY;
  ppClientItems: POPCHANDLEARRAY;
  pVariants: POleVariantArray;
  ppTimeArray: PFILETIMEARRAY;
  pItemValues: POPCITEMSTATEARRAY;
  GroupItem: TXRTLOPCDAGroupItem;
  dwCount: DWORD;
  Items: TXRTLArray;
  ItemState: POPCITEMSTATE;
  MasterQuality: HRESULT;
  Client: IOPCDataCallback;
begin
  ppServerItems:= nil;
  ppClientItems:= nil;
  pVariants:= nil;
  ppErrors:= nil;
  ppQualityArray:= nil;
  ppTimeArray:= nil;
  pItemValues:= nil;
  Items:= nil;
  dwCount:= 0;
  try
    try
      try
        Group.BeginRead;
        Items:= Group.GetActiveItems;
        dwCount:= Items.GetSize;
        if dwCount = 0 then Exit;
        ppServerItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCHANDLE)));
        ZeroMemory(ppServerItems, dwCount * SizeOf(OPCHANDLE));
        for I:= 0 to dwCount - 1 do
        begin
          XRTLGetAsObject(Items.GetValue(I), GroupItem);
          ppServerItems[I]:= GroupItem.ServerHandle;
        end;
        MasterError:= Group.IOPCSyncIO_Read(Source, dwCount, ppServerItems, pItemValues, ppErrors);
        OLECheck(MasterError);
      finally
        Group.EndRead;
      end;
      ppClientItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCHANDLE)));
      ZeroMemory(ppClientItems, dwCount * SizeOf(OPCHANDLE));
      pVariants:= POleVariantArray(CoTaskMemAlloc(dwCount * SizeOf(OleVariant)));
      ZeroMemory(pVariants, dwCount * SizeOf(OleVariant));
      ppQualityArray:= PWORDARRAY(CoTaskMemAlloc(dwCount * SizeOf(Word)));
      ZeroMemory(ppQualityArray, dwCount * SizeOf(Word));
      ppTimeArray:= PFILETIMEARRAY(CoTaskMemAlloc(dwCount * SizeOf(TFileTime)));
      ZeroMemory(ppTimeArray, dwCount * SizeOf(TFileTime));
      ppCallbackErrors:= PResultList(CoTaskMemAlloc(dwCount * SizeOf(HResult)));
      ZeroMemory(ppCallbackErrors, dwCount * SizeOf(HResult));
      MasterQuality:= S_OK;
      for I:= 0 to dwCount - 1 do
      begin
        ItemState:= @pItemValues[I];
        ppClientItems[I]:=    ItemState.hClient;
        VarCopy(pVariants[I], ItemState.vDataValue);
        ppQualityArray[I]:=   ItemState.wQuality;
        ppTimeArray[I]:=      ItemState.ftTimeStamp;
        ppCallbackErrors[I]:= ppErrors[I];
        if (ppQualityArray[I] and OPC_QUALITY_MASK) <> OPC_QUALITY_GOOD then
          MasterQuality:= S_FALSE;
      end;
      if Canceled then Exit;
      for I:= 0 to AIOPCDataCallbackConnectionPoint.SinkList.Count - 1 do
      begin
        try
          if Assigned(AIOPCDataCallbackConnectionPoint.SinkList[I]) and
             Supports(IUnknown(AIOPCDataCallbackConnectionPoint.SinkList[I]), IOPCDataCallback, Client) then
          begin
            OLECheck(Client.OnDataChange(TransactionID, Group.ClientHandle,
                                         MasterQuality, MasterError, dwCount,
                                         ppClientItems,
                                         pVariants, ppQualityArray, ppTimeArray,
                                         ppCallbackErrors));
          end;
        except
        end;
      end;
    finally
      XRTLFreeOPCITEMSTATEARRAY(pItemValues, dwCount);
      XRTLFreeFileTimeArray(ppTimeArray);
      XRTLFreeWORDARRAY(ppQualityArray);
      XRTLFreeResultList(ppErrors);
      XRTLFreeOleVariantArray(pVariants, dwCount);
      XRTLFreeOPCHANDLEARRAY(ppClientItems);
      XRTLFreeOPCHANDLEARRAY(ppServerItems);
      FreeAndNil(Items);
    end;
  except
  end;
end;

{ TXRTLOPCDA20GroupASyncDataChange }

var
  OnDataChangeCount: Integer = 0;

procedure TXRTLOPCDA20GroupASyncDataChange.DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
var
  ppErrors: PResultList;
  ppCallbackErrors: PResultList;
  I: Integer;
  MasterError: HResult;
  ppQualityArray: PWORDARRAY;
  ppServerItems: POPCHANDLEARRAY;
  ppClientItems: POPCHANDLEARRAY;
  pVariants: POleVariantArray;
  ppTimeArray: PFILETIMEARRAY;
  pItemValues: POPCITEMSTATEARRAY;
  GroupItem: TXRTLOPCDAGroupItem;
  dwCount, dwCallbackCount: DWORD;
  Items: TXRTLArray;
  TimeStamp, GTimeStamp: TXRTLTimeStamp;
  ItemState: POPCITEMSTATE;
  MasterQuality: HRESULT;
  Client: IOPCDataCallback;
begin
  ppServerItems:= nil;
  ppClientItems:= nil;
  pVariants:= nil;
  ppErrors:= nil;
  ppQualityArray:= nil;
  ppTimeArray:= nil;
  pItemValues:= nil;
  Items:= nil;
  TimeStamp:= nil;
  GTimeStamp:= nil;
  dwCount:= 0;
  dwCallbackCount:= 0;
  try
    try
      GTimeStamp:= TXRTLTimeStamp.Create;
      GTimeStamp.Assign(Group.LastClientUpdate);
      Group.LastClientUpdate.SetCurrentTime;
      try
        Group.BeginRead;
        Items:= Group.GetActiveItems;
        dwCount:= Items.GetSize;
        if dwCount = 0 then Exit;
        ppServerItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCHANDLE)));
        ZeroMemory(ppServerItems, dwCount * SizeOf(OPCHANDLE));
        for I:= 0 to dwCount - 1 do
        begin
          XRTLGetAsObject(Items.GetValue(I), GroupItem);
          ppServerItems[I]:= GroupItem.ServerHandle;
        end;
        MasterQuality:= S_OK;
        MasterError:= Group.IOPCSyncIO_Read(Source, dwCount, ppServerItems, pItemValues, ppErrors);
        OLECheck(MasterError);
      finally
        Group.EndRead;
      end;
      TimeStamp:= TXRTLTimeStamp.Create;
      dwCallbackCount:= 0;
      for I:= 0 to dwCount - 1 do
      begin
        try
          OLECheck(ppErrors[I]);
// if item has not been updated then mark it as FAILED
          TimeStamp.UTCFileTime:= pItemValues[I].ftTimeStamp;
          if TimeStamp.Compare(GTimeStamp) = XRTLLessThanValue then
            ppErrors[I]:= E_FAIL
          else
            Inc(dwCallbackCount);
        except
        end;
      end;
      if dwCallbackCount = 0 then Exit;
      ppClientItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCallbackCount * SizeOf(OPCHANDLE)));
      ZeroMemory(ppClientItems, dwCallbackCount * SizeOf(OPCHANDLE));
      pVariants:= POleVariantArray(CoTaskMemAlloc(dwCallbackCount * SizeOf(OleVariant)));
      ZeroMemory(pVariants, dwCallbackCount * SizeOf(OleVariant));
      ppQualityArray:= PWORDARRAY(CoTaskMemAlloc(dwCallbackCount * SizeOf(Word)));
      ZeroMemory(ppQualityArray, dwCallbackCount * SizeOf(Word));
      ppTimeArray:= PFILETIMEARRAY(CoTaskMemAlloc(dwCallbackCount * SizeOf(TFileTime)));
      ZeroMemory(ppTimeArray, dwCallbackCount * SizeOf(TFileTime));
      ppCallbackErrors:= PResultList(CoTaskMemAlloc(dwCallbackCount * SizeOf(HResult)));
      ZeroMemory(ppCallbackErrors, dwCallbackCount * SizeOf(HResult));
      dwCallbackCount:= 0;
      for I:= 0 to dwCount - 1 do
      begin
        try
          OLECheck(ppErrors[I]);
          ItemState:= @pItemValues[I];
          ppClientItems[dwCallbackCount]:=    ItemState.hClient;
          VarCopy(pVariants[dwCallbackCount], ItemState.vDataValue);
          ppQualityArray[dwCallbackCount]:=   ItemState.wQuality;
          ppTimeArray[dwCallbackCount]:=      ItemState.ftTimeStamp;
          ppCallbackErrors[dwCallbackCount]:= ppErrors[I];
          if (ppQualityArray[dwCallbackCount] and OPC_QUALITY_MASK) <> OPC_QUALITY_GOOD then
            MasterQuality:= S_FALSE;
          Inc(dwCallbackCount);
        except
        end;
      end;
      if Canceled then
        Exit;
      for I:= 0 to AIOPCDataCallbackConnectionPoint.SinkList.Count - 1 do
      begin
        try
          if Assigned(AIOPCDataCallbackConnectionPoint.SinkList[I]) and
             Supports(IUnknown(AIOPCDataCallbackConnectionPoint.SinkList[I]), IOPCDataCallback, Client) then
          begin
            OLECheck(Client.OnDataChange(TransactionID, Group.ClientHandle,
                                         MasterQuality, MasterError, dwCallbackCount,
                                         ppClientItems,
                                         pVariants, ppQualityArray, ppTimeArray, ppCallbackErrors));
          end;
        except
        end;
      end;
    finally
      XRTLFreeOPCITEMSTATEARRAY(pItemValues, dwCount);
      XRTLFreeFileTimeArray(ppTimeArray);
      XRTLFreeWORDARRAY(ppQualityArray);
      XRTLFreeResultList(ppErrors);
      XRTLFreeOleVariantArray(pVariants, dwCallbackCount);
      XRTLFreeOPCHANDLEARRAY(ppClientItems);
      XRTLFreeOPCHANDLEARRAY(ppServerItems);
      FreeAndNil(Items);
      FreeAndNil(TimeStamp);
      FreeAndNil(GTimeStamp);
    end;
  except
  end;
end;

{ TXRTLOPCDA20GroupASyncRead }

destructor TXRTLOPCDA20GroupASyncRead.Destroy;
begin
  XRTLFreeOPCHANDLEARRAY(phServer);
  inherited;
end;

procedure TXRTLOPCDA20GroupASyncRead.DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
var
  ppErrors: PResultList;
  I: Integer;
  MasterError: HResult;
  ppQualityArray: PWORDARRAY;
  ppClientItems: POPCHANDLEARRAY;
  pVariants: POleVariantArray;
  ppTimeArray: PFILETIMEARRAY;
  pItemValues: POPCITEMSTATEARRAY;
  ItemState: POPCITEMSTATE;
  MasterQuality: HRESULT;
  Client: IOPCDataCallback;
begin
  ppClientItems:= nil;
  pVariants:= nil;
  ppErrors:= nil;
  ppQualityArray:= nil;
  ppTimeArray:= nil;
  pItemValues:= nil;
  try
    try
      if dwCount = 0 then Exit;
      ppClientItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCHANDLE)));
      ZeroMemory(ppClientItems, dwCount * SizeOf(OPCHANDLE));
      pVariants:= POleVariantArray(CoTaskMemAlloc(dwCount * SizeOf(OleVariant)));
      ZeroMemory(pVariants, dwCount * SizeOf(OleVariant));
      ppQualityArray:= PWORDARRAY(CoTaskMemAlloc(dwCount * SizeOf(Word)));
      ZeroMemory(ppQualityArray, dwCount * SizeOf(Word));
      ppTimeArray:= PFILETIMEARRAY(CoTaskMemAlloc(dwCount * SizeOf(TFileTime)));
      ZeroMemory(ppTimeArray, dwCount * SizeOf(TFileTime));
      try
        Group.BeginRead;
        MasterQuality:= S_OK;
        MasterError:= Group.IOPCSyncIO_Read(OPC_DS_DEVICE, dwCount, phServer, pItemValues, ppErrors);
        OLECheck(MasterError);
      finally
        Group.EndRead;
      end;
      for I:= 0 to dwCount - 1 do
      begin
        ItemState:= @pItemValues[I];
        ppClientItems[I]:=  ItemState.hClient;
        VarCopy(pVariants[I], ItemState.vDataValue);
        ppQualityArray[I]:= ItemState.wQuality;
        ppTimeArray[I]:=    ItemState.ftTimeStamp;
        if (ppQualityArray[I] and OPC_QUALITY_MASK) <> OPC_QUALITY_GOOD then
          MasterQuality:= S_FALSE;
      end;
      if Canceled then
        Exit;
      for I:= 0 to AIOPCDataCallbackConnectionPoint.SinkList.Count - 1 do
      begin
        try
          if Assigned(AIOPCDataCallbackConnectionPoint.SinkList[I]) and
             Supports(IUnknown(AIOPCDataCallbackConnectionPoint.SinkList[I]), IOPCDataCallback, Client) then
          begin
            Client.OnReadComplete(TransactionID, Group.ClientHandle,
                                  MasterQuality, MasterError, dwCount,
                                  ppClientItems,
                                  pVariants, ppQualityArray, ppTimeArray, ppErrors);
          end;
        except
        end;
      end;
    finally
      XRTLFreeOPCITEMSTATEARRAY(pItemValues, dwCount);
      XRTLFreeFileTimeArray(ppTimeArray);
      XRTLFreeWORDARRAY(ppQualityArray);
      XRTLFreeResultList(ppErrors);
      XRTLFreeOleVariantArray(pVariants, dwCount);
      XRTLFreeOPCHANDLEARRAY(ppClientItems);
    end;
  except
  end;
end;

{ TXRTLOPCDA20GroupASyncWrite }

destructor TXRTLOPCDA20GroupASyncWrite.Destroy;
begin
  XRTLFreeOPCHANDLEARRAY(phServer);
  XRTLFreeOleVariantArray(pItemValues, dwCount);
  inherited;
end;

procedure TXRTLOPCDA20GroupASyncWrite.DoExecute(const AIOPCDataCallbackConnectionPoint: TConnectionPoint);
var
  ppErrors: PResultList;
  MasterError: HResult;
  ppClientItems: POPCHANDLEARRAY;
  I: DWORD;
  GroupItem: TXRTLOPCDAGroupItem;
  Client: IOPCDataCallback;
begin
  ppClientItems:= nil;
  ppErrors:= nil;
  try
    try
      if dwCount = 0 then Exit;
      try
        Group.BeginRead;
        MasterError:= Group.IOPCSyncIO_Write(dwCount, phServer, pItemValues, ppErrors);
        OLECheck(MasterError);
        ppClientItems:= POPCHANDLEARRAY(CoTaskMemAlloc(dwCount * SizeOf(OPCHANDLE)));
        ZeroMemory(ppClientItems, dwCount * SizeOf(OPCHANDLE));
        for I:= 0 to dwCount - 1 do
        begin
          GroupItem:= Group.GetItem(phServer[I]);
          if Assigned(GroupItem) then
            ppClientItems[I]:= GroupItem.ClientHandle;
        end;
      finally
        Group.EndRead;
      end;
      if Canceled then
        Exit;
      for I:= 0 to AIOPCDataCallbackConnectionPoint.SinkList.Count - 1 do
      begin
        try
          if Assigned(AIOPCDataCallbackConnectionPoint.SinkList[I]) and
             Supports(IUnknown(AIOPCDataCallbackConnectionPoint.SinkList[I]), IOPCDataCallback, Client) then
          begin
            OLECheck(Client.OnWriteComplete(TransactionID, Group.ClientHandle,
                                            MasterError, dwCount,
                                            ppClientItems, ppErrors));
          end;
        except
        end;
      end;
    finally
      XRTLFreeOPCHANDLEARRAY(ppClientItems);
      XRTLFreeResultList(ppErrors);
    end;
  except
  end;
end;

end.

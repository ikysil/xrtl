unit xrtl_opc_sdk_EnumOPCItemAttributes;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, Classes, COMObj, ActiveX,
  xrtl_util_COMUtils,
  xrtl_opc_DA, 
  xrtl_opc_sdk_OPCDAServer;

type
  TXRTLEnumOPCItemAttributes = class(TXRTLEnumXXXX, IEnumOPCItemAttributes)
  private
    FItemAttributes: TList;
    procedure  Copy(var AItemAttributes: OPCITEMATTRIBUTES; Data: POPCITEMATTRIBUTES);
  protected
    function   GetItem(const ItemIndex: LongInt): Pointer; override;
    function   GetCount: LongInt; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  Add(const Item: TXRTLOPCDAGroupItem);
    function   Next(celt: ULONG; out ppItemArray: POPCITEMATTRIBUTESARRAY; out pceltFetched: ULONG): HResult; stdcall;
    function   Skip(celt: ULONG): HResult; stdcall;
    function   Clone(out ppEnumItemAttributes: IEnumOPCItemAttributes): HResult; stdcall;
  end;

implementation

uses
  xrtl_opc_Utils;

{ TXRTLEnumOPCItemAttributes }

constructor TXRTLEnumOPCItemAttributes.Create;
begin
  inherited;
  FItemAttributes:= TList.Create;
end;

destructor TXRTLEnumOPCItemAttributes.Destroy;
var
  Value: POPCITEMATTRIBUTESARRAY;
begin
  while FItemAttributes.Count > 0 do
  begin
    Value:= FItemAttributes[0];
    Value[0].szAccessPath:= nil;
    Value[0].szItemID:= nil;
    Value[0].pBlob:= nil;
    XRTLFreeOPCITEMATTRIBUTESARRAY(Value, 1);
    FItemAttributes.Delete(0);
  end;
  inherited;
end;

function TXRTLEnumOPCItemAttributes.GetCount: LongInt;
begin
  Result:= FItemAttributes.Count;
end;

function TXRTLEnumOPCItemAttributes.GetItem(const ItemIndex: Integer): Pointer;
begin
  Result:= FItemAttributes[ItemIndex];
end;

procedure TXRTLEnumOPCItemAttributes.Add(const Item: TXRTLOPCDAGroupItem);
var
  ItemAttributes: POPCITEMATTRIBUTES;
begin
  ItemAttributes:= POPCITEMATTRIBUTES(CoTaskMemAlloc(SizeOf(OPCITEMATTRIBUTES)));
  ZeroMemory(ItemAttributes, SizeOf(OPCITEMATTRIBUTES));
  ItemAttributes.szAccessPath:=        nil;
  ItemAttributes.szItemID:=            PWideChar(Item.ItemID);
  ItemAttributes.bActive:=             Item.Active;
  ItemAttributes.hClient:=             Item.ClientHandle;
  ItemAttributes.hServer:=             Item.ServerHandle;
  ItemAttributes.dwAccessRights:=      Item.AccessRights;
  ItemAttributes.dwBlobSize:=          0;
  ItemAttributes.pBlob:=               nil;
  ItemAttributes.vtRequestedDataType:= Item.RequestedDataType;
  ItemAttributes.vtCanonicalDataType:= Item.CanonicalDataType;
  ItemAttributes.dwEUType:=            OPC_NOENUM;
  VarClear(ItemAttributes.vEUInfo);
  FItemAttributes.Add(ItemAttributes);
end;

procedure TXRTLEnumOPCItemAttributes.Copy(var AItemAttributes: OPCITEMATTRIBUTES; Data: POPCITEMATTRIBUTES);
begin
  ZeroMemory(@AItemAttributes, SizeOf(OPCITEMATTRIBUTES));
  AItemAttributes.szAccessPath:=        XRTLAllocOutWideString(Data.szAccessPath);
  AItemAttributes.szItemID:=            XRTLAllocOutWideString(Data.szItemID);
  AItemAttributes.bActive:=             Data.bActive;
  AItemAttributes.hClient:=             Data.hClient;
  AItemAttributes.hServer:=             Data.hServer;
  AItemAttributes.dwAccessRights:=      Data.dwAccessRights;
  AItemAttributes.dwBlobSize:=          Data.dwBlobSize;
  AItemAttributes.pBlob:=               Data.pBlob;
  AItemAttributes.vtRequestedDataType:= Data.vtRequestedDataType;
  AItemAttributes.vtCanonicalDataType:= Data.vtCanonicalDataType;
  AItemAttributes.dwEUType:=            Data.dwEUType;
  AItemAttributes.vEUInfo:=             Data.vEUInfo;
end;

function TXRTLEnumOPCItemAttributes.Next(celt: ULONG; out ppItemArray: POPCITEMATTRIBUTESARRAY; out pceltFetched: ULONG): HResult; stdcall;
var
  Fetched: Integer;
  Data: Pointer;
begin
  try
    XRTLCheckOutArgument(ppItemArray);
    XRTLCheckOutArgument(pceltFetched);
    XRTLCheckArgument(celt > 0);
    try
      pceltFetched:= 0;
      ppItemArray:= POPCITEMATTRIBUTESARRAY(CoTaskMemAlloc(celt * SizeOf(OPCITEMATTRIBUTES)));
      if not Assigned(ppItemArray) then
        OLEError(E_OUTOFMEMORY);
      repeat
        OLECheck(inherited Next(1, Data, @Fetched));
        if Fetched > 0 then
        begin
          Copy(ppItemArray[pceltFetched], Data);
          Inc(pceltFetched);
        end;
      until (Fetched = 0) or (pceltFetched = celt);
      if pceltFetched = celt then
        Result:= S_OK
      else
        Result:= S_FALSE;
    except
      XRTLFreeOPCITEMATTRIBUTESARRAY(ppItemArray, celt);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLEnumOPCItemAttributes.Skip(celt: ULONG): HResult; stdcall;
begin
  Result:= inherited Skip(celt);
end;

function TXRTLEnumOPCItemAttributes.Clone(out ppEnumItemAttributes: IEnumOPCItemAttributes): HResult;
begin
  try
    Result:= E_NOTIMPL;
  except
    Result:= E_UNEXPECTED;
  end;
end;

end.

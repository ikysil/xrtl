unit xrtl_opc_sdk_OPCDADataSourceCache;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX,
  xrtl_util_COMUtils, xrtl_util_TimeStamp, xrtl_util_Map, xrtl_util_Container,
  xrtl_opc_sdk_OPCDADataSource, xrtl_opc_sdk_DA;

type
  TXRTLOPCDADataSourceCache = class(TXRTLOPCDADataSource)
  private
    FItems: TXRTLMap;
  protected
    procedure  DoUpdate(ADataSource: IXRTLOPCDADataSource); override;
  public
    constructor Create;
    destructor Destroy; override;
    function   Read(szItemID: POLEStr; out Value: OleVariant; out Quality: Word; out TimeStamp: TFileTime): HResult; override;
    function   Write(szItemID: POLEStr; Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult; override;
    function   CreateItemEnumerator(out EnumString: IEnumString): HResult; override;
    function   AddItem(szItemID: POleStr): HResult; override;
    function   ClearItems: HResult; override;
    function   RemoveItem(szItemID: POleStr): HResult; override;
  end;

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}Variants, {$ENDIF}
  xrtl_util_Value,
  xrtl_opc_DA,
  xrtl_opc_sdk_OPCDAVariantManager;

type
  TXRTLOPCDADataSourceCacheItem = class
    ItemID: WideString;
    Value: OleVariant;
    Quality: Word;
    TimeStamp: TXRTLTimeStamp;
    constructor Create;
    destructor Destroy; override;
  end;

{ TXRTLOPCDADataSourceCacheItem }

constructor TXRTLOPCDADataSourceCacheItem.Create;
begin
  inherited;
  TimeStamp:= TXRTLTimeStamp.Create;
  VarClear(Value);
  Quality:= OPC_QUALITY_OUT_OF_SERVICE;
end;

destructor TXRTLOPCDADataSourceCacheItem.Destroy;
begin
  FreeAndNil(TimeStamp);
  inherited;
end;

{ TXRTLOPCDADataSourceCache }

constructor TXRTLOPCDADataSourceCache.Create;
begin
  inherited;
  FItems:= TXRTLArrayMap.Create;
end;

destructor TXRTLOPCDADataSourceCache.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TXRTLOPCDADataSourceCache.Read(szItemID: POLEStr;
  out Value: OleVariant; out Quality: Word;
  out TimeStamp: TFileTime): HResult;
var
  Item: TXRTLOPCDADataSourceCacheItem;
  ItemID: WideString;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(Value);
    XRTLCheckOutArgument(Quality);
    XRTLCheckOutArgument(TimeStamp);
    try
      try
        BeginRead;
        AddItem(szItemID);
        ItemID:= WideString(szItemID);
        XRTLGetAsObject(FItems.GetValue(XRTLValue(ItemID)), Item);
        OLECheck(VariantCopy(Value, Item.Value));
        Quality:= Item.Quality;
        TimeStamp:= Item.TimeStamp.UTCFileTime;
      finally
        EndRead;
      end;
    except
      VarClear(Value);
      Quality:= OPC_QUALITY_BAD;
      Int64(TimeStamp):= 0;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSourceCache.Write(szItemID: POLEStr;
  Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult;
var
  Item: TXRTLOPCDADataSourceCacheItem;
  ItemID: WideString;
  bResult: BOOL;
begin
  Result:= S_OK;
  try
    try
      BeginRead;
      AddItem(szItemID);
      ItemID:= WideString(szItemID);
      XRTLGetAsObject(FItems.GetValue(XRTLValue(ItemID)), Item);
      OLECheck(XRTLOPCDAVariantManager.VarEqual(Item.Value, Value, bResult));
      if not bResult or (Item.Quality <> Quality) then
      begin
        OLECheck(VariantCopy(Item.Value, Value));
        Item.Quality:= Quality;
        if Int64(TimeStamp) = 0 then
          Item.TimeStamp.SetCurrentTime
        else
          Item.TimeStamp.UTCFileTime:= TimeStamp;
      end;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSourceCache.CreateItemEnumerator(out EnumString: IEnumString): HResult;
var
  Enum: TXRTLEnumString;
  I: Integer;
  LItems: TXRTLValueArray;
begin
  SetLength(LItems, 0);
  try
    XRTLCheckOutArgument(EnumString);
    try
      BeginRead;
      Enum:= TXRTLEnumString.Create;
      EnumString:= Enum;
      LItems:= FItems.GetKeys;
      for I:= 0 to Length(LItems) - 1 do
      begin
        Enum.Add(XRTLGetAsWideString(LItems[I]));
      end;
      if Enum.Count > 0 then
        Result:= S_OK
      else
        Result:= S_FALSE;
    finally
      EndRead;
    end;
  except
    EnumString:= nil;
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCDADataSourceCache.DoUpdate(ADataSource: IXRTLOPCDADataSource);
var
  Value: OLEVariant;
  Quality: Word;
  ftTimeStamp: TFileTime;
  EnumString: IEnumString;
  ItemID: POLEStr;
  Fetched: Integer;
begin
  try
    try
      BeginRead;
//      OLECheck(XRTLOPCDANameSpace.CreateItemEnumerator(VarArrayOf([]), OPC_FLAT, '', VT_EMPTY, OPC_READABLE, EnumString));
      OLECheck(CreateItemEnumerator(EnumString));
      while (EnumString.Next(1, ItemID, @Fetched) = S_OK) and (Fetched > 0) do
      begin
        if Succeeded(ADataSource.Read(ItemID, Value, Quality, ftTimeStamp)) then
        begin
          Write(ItemID, Value, Quality, ftTimeStamp);
        end;
        XRTLFreeOutWideString(ItemID);
      end;
    finally
      EndRead;
    end;
  except
  end;
end;

function TXRTLOPCDADataSourceCache.AddItem(szItemID: POleStr): HResult;
var
  ItemID: WideString;
  Item: TXRTLOPCDADataSourceCacheItem;
  ItemValue: IXRTLValue;
begin
  try
    Result:= S_OK;
    try
      BeginWrite;
      ItemID:= WideString(szItemID);
      ItemValue:= FItems.GetValue(XRTLValue(ItemID));
      if not Assigned(ItemValue) then
      begin
        Item:= TXRTLOPCDADataSourceCacheItem.Create;
        Item.ItemID:= ItemID;
        FItems.SetValue(XRTLValue(ItemID), XRTLValue(Item, True));
      end;
    finally
      ItemID:= '';
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSourceCache.RemoveItem(szItemID: POleStr): HResult;
var
  ItemID: WideString;
begin
  try
    Result:= S_OK;
    try
      BeginWrite;
      ItemID:= WideString(szItemID);
      FItems.Remove(XRTLValue(ItemID));
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDADataSourceCache.ClearItems: HResult;
begin
  try
    Result:= S_OK;
    try
      BeginWrite;
      FItems.Clear;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

end.

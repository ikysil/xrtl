unit xrtl_opc_sdk_OPCDANameSpace;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_util_StrUtils, xrtl_util_COMUtils, xrtl_util_Compat,
  xrtl_util_NameSpace, xrtl_util_NameSpacePath,
  xrtl_util_Lock,
  xrtl_opc_DA, xrtl_opc_Error,
  xrtl_opc_sdk_OPCDANameSpaceItem, xrtl_opc_sdk_DA,
  xrtl_opc_sdk_OPCDAVariantManager;

type
  TXRTLOPCDANameSpace = class(TInterfacedObject, IXRTLReadWriteLock,
                              IXRTLOPCDANameSpace)
  private
    FOrganization: OPCNAMESPACETYPE;
    FLock: IXRTLReadWriteLock;
    FItems: TXRTLNameSpace;
    FItemIDSeparator: WideString;
  public
    constructor Create;
    destructor Destroy; override;
    property   ItemIDSeparator: WideString read FItemIDSeparator write FItemIDSeparator;
    function   GetOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
    function   SetOrganization(pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
    procedure  BeginRead;
    procedure  EndRead;
    function   BeginWrite: Boolean;
    procedure  EndWrite;
    function   CheckPath(ItemIDChunks: OleVariant{array of WideString}): HResult; stdcall;
    function   AddItem(ItemIDChunks: OleVariant{array of WideString};
                       Description: POLEStr;
                       DataType: TVarType;
                       out Item: IXRTLOPCDANameSpaceItem): HResult; stdcall;
    function   GetItem(ItemIDChunks: OleVariant{array of WideString}; out Item: IXRTLOPCDANameSpaceItem): HResult; stdcall;
    function   HasItem(ItemIDChunks: OleVariant{array of WideString}; out bResult: BOOL): HResult; stdcall;
    function   RemoveItem(ItemIDChunks: OleVariant{array of WideString}): HResult; stdcall;
    function   Clear: HResult; stdcall;
    function   CreateItemEnumerator(RootItemIDChunks: OleVariant{array of WideString};
                                    dwBrowseFilterType: OPCBROWSETYPE;
                                    szFilterCriteria: POleStr;
                                    vtDataTypeFilter: TVarType;
                                    dwAccessRightsFilter: DWORD;
                                    out ppIEnumString: IEnumString): HResult; stdcall;
    function   GetItemIDSeparator(out AItemIDSeparator: POleStr): HResult; stdcall;
    function   SetItemIDSeparator(AItemIDSeparator: POleStr): HResult; stdcall;
    function   CombineItemID(ItemIDChunks: OleVariant{array of WideString}; out ItemID: POleStr): HResult; stdcall;
    function   SplitItemID(ItemID: POleStr; out ItemIDChunks: OleVariant{array of WideString}): HResult; stdcall;
    function   IsLeaf(ItemIDChunks: OleVariant{array of WideString}; out bResult: BOOL): HResult; stdcall;
    function   IsBranch(ItemIDChunks: OleVariant{array of WideString}; out bResult: BOOL): HResult; stdcall;
  end;

var
  XRTLOPCDANameSpace: IXRTLOPCDANameSpace = nil;

implementation

uses
  xrtl_util_Value,
  xrtl_opc_sdk_OPCDADataSource;

{ TXRTLOPCDANameSpace }

constructor TXRTLOPCDANameSpace.Create;
begin
  inherited;
  FOrganization:= OPC_NS_FLAT;
  FLock:= XRTLCreateReadWriteLock;
  FItems:= TXRTLNameSpace.Create;
  FItemIDSeparator:= '.';
end;

destructor TXRTLOPCDANameSpace.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TXRTLOPCDANameSpace.GetOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult;
begin
  try
    XRTLCheckOutArgument(pNameSpaceType);
    pNameSpaceType:= FOrganization;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.SetOrganization(pNameSpaceType: OPCNAMESPACETYPE): HResult;
begin
  try
    XRTLCheckArgument(pNameSpaceType in [OPC_NS_HIERARCHIAL, OPC_NS_FLAT]);
    FOrganization:= pNameSpaceType;
    Result:= S_OK;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

procedure TXRTLOPCDANameSpace.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TXRTLOPCDANameSpace.EndRead;
begin
  FLock.EndRead;
end;

function TXRTLOPCDANameSpace.BeginWrite: Boolean;
begin
  Result:= FLock.BeginWrite;
end;

procedure TXRTLOPCDANameSpace.EndWrite;
begin
  FLock.EndWrite;
end;

function TXRTLOPCDANameSpace.CheckPath(ItemIDChunks: OleVariant{array of WideString}): HResult;
const
  IllegalChars: WideString = '*?[]';
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
  I: Integer;
begin
  Result:= S_OK;
  Path:= nil;
  try
    try
      BeginRead;
      if VarIsArray(ItemIDChunks) then
        OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
      else
        OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
      Path:= XRTLNameSpacePathCreate(LItemIDChunks);
      if XRTLNameSpacePathGetLength(Path) = 0 then
        OLEError(OPC_E_INVALIDITEMID);
      for I:= 0 to XRTLNameSpacePathGetLength(Path) - 1 do
      begin
        if (XRTLPos(Path[I], IllegalChars) > 0) or (Path[I] = '') or
           (XRTLPos(ItemIDSeparator, Path[I]) > 0) then
          OLEError(OPC_E_INVALIDITEMID);
      end;
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.AddItem(
  ItemIDChunks: OleVariant{array of WideString}; Description: POLEStr;
  DataType: TVarType;
  out Item: IXRTLOPCDANameSpaceItem): HResult;
var
  bResult: BOOL;
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
  LItem: TXRTLOPCDANameSpaceItem;
  LItemID: POLEStr;
begin
  Result:= S_OK;
  Path:= nil;
  LItemID:= nil;
  try
    XRTLCheckOutArgument(Item);
    try
      try
        BeginWrite;
        OLECheck(XRTLOPCDAVariantManager.IsTypeSupported(DataType, bResult));
        if not bResult then
          OLEError(OPC_E_BADTYPE);
        if VarIsArray(ItemIDChunks) then
          OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
        else
          OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
        OLECheck(CheckPath(LItemIDChunks));
        OLECheck(HasItem(LItemIDChunks, bResult));
        if not bResult then
        begin
          Path:= XRTLNameSpacePathCreate(LItemIDChunks);
          OLECheck(CombineItemID(LItemIDChunks, LItemID));
          if Assigned(XRTLOPCDADataSourceCache) then
            OLECheck(XRTLOPCDADataSourceCache.AddItem(LItemID));
          LItem:= TXRTLOPCDANameSpaceItem.Create(Path, DataType, Description);
          Item:= LItem;
          LItem.ItemID:= LItemID;
          FItems.SetValue(Path, XRTLValue(Item));
        end
        else
          OLEError(OPC_E_DUPLICATENAME);
      finally
        XRTLFreeOutWideString(LItemID);
        EndWrite;
      end;
    except
      Item:= nil;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.GetItem(ItemIDChunks: OleVariant{array of WideString};
  out Item: IXRTLOPCDANameSpaceItem): HResult;
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
  ItemValue: IXRTLValue;
begin
  Result:= S_OK;
  Path:= nil;
  try
    XRTLCheckOutArgument(Item);
    try
      try
        BeginRead;
        if VarIsArray(ItemIDChunks) then
          OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
        else
          OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
        OLECheck(CheckPath(LItemIDChunks));
        Path:= XRTLNameSpacePathCreate(LItemIDChunks);
        ItemValue:= FItems.GetValue(Path);
        if Assigned(ItemValue) then
        begin
          Item:= XRTLGetAsInterface(ItemValue) as IXRTLOPCDANameSpaceItem;
        end
        else
          Result:= OPC_E_UNKNOWNITEMID;
      finally
        EndRead;
      end;
    except
      Item:= nil;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.HasItem(ItemIDChunks: OleVariant{array of WideString};
  out bResult: BOOL): HResult;
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
begin
  Result:= S_OK;
  Path:= nil;
  try
    XRTLCheckOutArgument(bResult);
    try
      BeginRead;
      if VarIsArray(ItemIDChunks) then
        OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
      else
        OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
      OLECheck(CheckPath(LItemIDChunks));
      Path:= XRTLNameSpacePathCreate(LItemIDChunks);
      bResult:= FItems.HasKey(Path);
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.RemoveItem(ItemIDChunks: OleVariant{array of WideString}): HResult;
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
  LItemID: POLEStr;
begin
  Result:= S_OK;
  Path:= nil;
  LItemID:= nil;
  try
    try
      BeginWrite;
      if VarIsArray(ItemIDChunks) then
        OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
      else
        OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
      OLECheck(CheckPath(LItemIDChunks));
      OLECheck(CombineItemID(LItemIDChunks, LItemID));
      if Assigned(XRTLOPCDADataSourceCache) then
        OLECheck(XRTLOPCDADataSourceCache.RemoveItem(LItemID));
      Path:= XRTLNameSpacePathCreate(LItemIDChunks);
      if FItems.HasKey(Path) then
        FItems.Remove(Path)
      else
        OLEError(OPC_E_UNKNOWNITEMID);
    finally
      XRTLFreeOutWideString(LItemID);
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.Clear: HResult;
begin
  try
    try
      BeginWrite;
      FItems.Clear;
      if Assigned(XRTLOPCDADataSourceCache) then
        OLECheck(XRTLOPCDADataSourceCache.ClearItems);
      Result:= S_OK;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.CreateItemEnumerator(
  RootItemIDChunks: OleVariant{array of WideString}; dwBrowseFilterType: OPCBROWSETYPE;
  szFilterCriteria: POleStr; vtDataTypeFilter: TVarType;
  dwAccessRightsFilter: DWORD; out ppIEnumString: IEnumString): HResult;
var
  Enum: TXRTLNameSpaceEnumerator;
  Path, RootPath, ItemPath: TXRTLNameSpacePath;
  EnumString: TXRTLEnumString;
  EnumType: TXRTLNameSpaceEnumeratorType;
  PPath: Pointer;
  Count: Integer;
  FilterString: WideString;
  ItemAccessRights: DWORD;
  ItemID: POLEStr;
  Item: IXRTLOPCDANameSpaceItem;
  DataTypeFilter, ItemDataType: TVarType;
  bResult: BOOL;
begin
  Enum:= nil;
  RootPath:= nil;
  Path:= nil;
  ItemPath:= nil;
  EnumType:= nsEnumerateRecursive;
  try
    XRTLCheckOutArgument(ppIEnumString);
    XRTLCheckArgument(dwBrowseFilterType in [OPC_BRANCH, OPC_LEAF, OPC_FLAT]);
    try
      OLECheck(XRTLOPCDAVariantManager.IsTypeSupported(vtDataTypeFilter, bResult));
      XRTLCheckArgument(bResult);
      try
        BeginRead;
        RootPath:= XRTLNameSpacePathCreate(RootItemIDChunks);
        case dwBrowseFilterType of
          OPC_BRANCH:
            EnumType:= nsEnumerateBranches;
          OPC_LEAF:
            EnumType:= nsEnumerateLeaves;
          OPC_FLAT:
            EnumType:= nsEnumerateRecursive;
        else
          OLEError(E_INVALIDARG);
        end;
        FilterString:= Trim(szFilterCriteria);
        if FilterString = '' then
          FilterString:= '*';
        DataTypeFilter:= vtDataTypeFilter and (varTypeMask or varArray);
        Enum:= FItems.CreateEnumerator(RootPath, EnumType, FilterString);
        EnumString:= TXRTLEnumString.Create;
        ppIEnumString:= EnumString;
        while (Enum.Next(1, PPath, @Count) = S_OK) and (Count > 0) do
        begin
          ItemID:= nil;
          Path:= TXRTLNameSpacePath(PPath^);
          ItemPath:= XRTLNameSpacePathConcat(RootPath, Path);
          OLECheck(GetItem(ItemPath, Item));
          OLECheck(Item.GetAccessRights(ItemAccessRights));
          OLECheck(Item.GetDataType(ItemDataType));
          if (((ItemAccessRights and dwAccessRightsFilter) <> 0) or (dwAccessRightsFilter = 0)) and
             ((ItemDataType = DataTypeFilter) or (DataTypeFilter = 0)) then
          begin
            if dwBrowseFilterType = OPC_FLAT then
              OLECheck(CombineItemID(ItemPath, ItemID))
            else
              OLECheck(CombineItemID(Path, ItemID));
            EnumString.Add(ItemID);
            XRTLFreeOutWideString(ItemID);
          end;
        end;
        if EnumString.Strings.IsEmpty then
          Result:= S_FALSE
        else
          Result:= S_OK;
      finally
        FreeAndNil(Enum);
        EndRead;
      end;
    except
      ppIEnumString:= nil;
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.GetItemIDSeparator(out AItemIDSeparator: POleStr): HResult;
begin
  try
    XRTLCheckOutArgument(AItemIDSeparator);
    try
      AItemIDSeparator:= nil;
      try
        BeginRead;
        AItemIDSeparator:= XRTLAllocOutWideString(FItemIDSeparator);
        Result:= S_OK;
      finally
        EndRead;
      end;
    except
      XRTLFreeOutWideString(AItemIDSeparator);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.SetItemIDSeparator(AItemIDSeparator: POleStr): HResult;
begin
  try
    XRTLCheckArgument(Length(AItemIDSeparator) > 0);
    try
      BeginWrite;
      if not FItems.IsEmpty then
        OLEError(E_FAIL);
      FItemIDSeparator:= AItemIDSeparator;
      Result:= S_OK;
    finally
      EndWrite;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.CombineItemID(ItemIDChunks: OleVariant; out ItemID: POleStr): HResult;
var
  Path: TXRTLNameSpacePath;
  I: Integer;
  S: WideString;
begin
  Path:= nil;
  try
    XRTLCheckOutArgument(ItemID);
    try
      ItemID:= nil;
      try
        BeginRead;
        S:= '';
        Path:= XRTLNameSpacePathCreate(ItemIDChunks);
        for I:= 0 to XRTLNameSpacePathGetLength(Path) - 1 do
        begin
          if not WideSameStr('', S) then
            S:= S + FItemIDSeparator;
          S:= S + Path[I];
        end;
        ItemID:= XRTLAllocOutWideString(S);
        Result:= S_OK;
      finally
        EndRead;
      end;
    except
      XRTLFreeOutWideString(ItemID);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.SplitItemID(ItemID: POleStr; out ItemIDChunks: OleVariant{array of WideString}): HResult;
var
  LItemID, LItemIDChunk: WideString;
begin
  Result:= S_OK;
  try
    XRTLCheckOutArgument(ItemIDChunks);
    try
      try
        BeginRead;
        ItemIDChunks:= VarArrayCreate([0, -1], varOleStr);
        if ItemID = nil then
          LItemID:= ''
        else
          LItemID:= ItemID;
        if WideSameStr('', LItemID) then
          OLEError(OPC_E_INVALIDITEMID);
        while Length(LItemID) > 0 do
        begin
          VarArrayRedim(ItemIDChunks, VarArrayHighBound(ItemIDChunks, 1) + 1);
          LItemIDChunk:= XRTLFetch(LItemID, FItemIDSeparator, True);
          OLECheck(CheckPath(VarArrayOf([LItemIDChunk])));
          ItemIDChunks[VarArrayHighBound(ItemIDChunks, 1)]:= LItemIDChunk;
        end;
      finally
        EndRead;
      end;
    except
      VarClear(ItemIDChunks);
      raise;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.IsLeaf(ItemIDChunks: OleVariant{array of WideString};
  out bResult: BOOL): HResult;
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
begin
  Result:= S_OK;
  Path:= XRTLNameSpacePathCreate([]);
  try
    XRTLCheckOutArgument(bResult);
    try
      BeginRead;
      if VarIsArray(ItemIDChunks) then
        OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
      else
        OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
      OLECheck(CheckPath(LItemIDChunks));
      Path:= XRTLNameSpacePathCreate(LItemIDChunks);
      bResult:= FItems.IsLeaf(Path);
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

function TXRTLOPCDANameSpace.IsBranch(ItemIDChunks: OleVariant{array of WideString};
  out bResult: BOOL): HResult;
var
  Path: TXRTLNameSpacePath;
  LItemIDChunks: OleVariant;
begin
  Result:= S_OK;
  Path:= XRTLNameSpacePathCreate([]);
  try
    XRTLCheckOutArgument(bResult);
    try
      BeginRead;
      if VarIsArray(ItemIDChunks) then
        OLECheck(VariantCopy(LItemIDChunks, ItemIDChunks))
      else
        OLECheck(SplitItemID(PWideChar(WideString(ItemIDChunks)), LItemIDChunks));
      OLECheck(CheckPath(LItemIDChunks));
      Path:= XRTLNameSpacePathCreate(LItemIDChunks);
      bResult:= FItems.IsBranch(Path);
    finally
      EndRead;
    end;
  except
    Result:= XRTLHandleCOMException;
  end;
end;

initialization
begin
end;

finalization
begin
  XRTLOPCDANameSpace:= nil;
end;

end.

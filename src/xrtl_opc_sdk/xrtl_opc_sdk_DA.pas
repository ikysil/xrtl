unit xrtl_opc_sdk_DA;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, ActiveX,
  xrtl_opc_DA, xrtl_opc_Types;

const
  CLASS_OPCDA20Group:       TGUID = '{B0659A5C-EF3A-457D-856A-4039F2137F63}';
  CLASS_OPCDANameSpace:     TGUID = '{B868317A-82E1-495C-BA20-7950E60289FF}';
  CLASS_OPCDANameSpaceItem: TGUID = '{23FAC4AA-8E6F-4E2A-9EE0-B3EC3FDF8BBE}';
  IID_IXRTLOPCDAServer:     TGUID = '{C9108B6D-B9A7-4396-BF9F-55F24849E920}';
  IID_IXRTLOPCDANameSpaceItem: TGUID = '{3C914D07-3FE2-4339-B562-6D849177F787}';

type
  IXRTLOPCDAServer      = interface;
  IXRTLOPCDAGroup       = interface;
  IXRTLOPCDANameSpace     = interface;
  IXRTLOPCDANameSpaceItem = interface;
  IXRTLOPCDADataSource    = interface;

  IXRTLOPCDAServer = interface
  ['{C9108B6D-B9A7-4396-BF9F-55F24849E920}']
    function   GetNameSpace(out ANameSpace: IXRTLOPCDANameSpace): HResult; stdcall;
    function   SetNameSpace(ANameSpace: IXRTLOPCDANameSpace): HResult; stdcall;
    function   GetDataSource(dwSource: OPCDATASOURCE; out ADataSource: IXRTLOPCDADataSource): HResult; stdcall;
    function   SetDataSource(dwSource: OPCDATASOURCE; ADataSource: IXRTLOPCDADataSource): HResult; stdcall;
  end;

  IXRTLOPCDAGroup = interface
  ['{D2ADCF12-1553-4040-AA0F-B74EDA6FE1D2}']
    function   SetDeleted(bFlag: BOOL): HResult; stdcall;
    function   GetDeleted(out bFLag: BOOL): HResult; stdcall;
  end;

  IXRTLOPCDANameSpace = interface
  ['{EF4B587B-7426-43C7-9C6F-A513E1367198}']
    function   GetOrganization(out pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
    function   SetOrganization(pNameSpaceType: OPCNAMESPACETYPE): HResult; stdcall;
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

  IXRTLOPCDANameSpaceItem = interface
  ['{3C914D07-3FE2-4339-B562-6D849177F787}']
    function   QueryAvailableProperties(out pdwCount: DWORD; out ppPropertyIDs: PDWORDARRAY;
                 out ppDescriptions: POleStrList; out ppvtDataTypes: PVarTypeList): HResult; stdcall;
    function   GetItemProperties(dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                 out ppvData: POleVariantArray; out ppErrors: PResultList): HResult; stdcall;
    function   LookupItemIDs(dwCount: DWORD; pdwPropertyIDs: PDWORDARRAY;
                 out ppszNewItemIDs: POleStrList; out ppErrors: PResultList): HResult; stdcall;
    function   GetAccessRights(out AAccessRights: DWORD): HResult; stdcall;
    function   SetAccessRights(AAccessRights: DWORD): HResult; stdcall;
    function   GetDataType(out ADataType: TVarType): HResult; stdcall;
    function   SetDataType(ADataType: TVarType): HResult; stdcall;
    function   CanRead(out bResult: BOOL): HResult; stdcall;
    function   CanWrite(out bResult: BOOL): HResult; stdcall;
    function   GetItemID(out szItemID: POLEStr): HResult; stdcall;
  end;

  IXRTLOPCDADataSource = interface
  ['{5C389437-7A4D-4DB0-8E02-15FE2523D9B8}']
    function   Read(szItemID: POLEStr; out Value: OleVariant; out Quality: Word; out TimeStamp: TFileTime): HResult; stdcall;
    function   Write(szItemID: POLEStr; Value: OleVariant; Quality: Word; TimeStamp: TFileTime): HResult; stdcall;
    function   CreateItemEnumerator(out Enum: IEnumString): HResult; stdcall;
    function   Update(ADataSource: IXRTLOPCDADataSource): HResult; stdcall;
    function   AddItem(szItemID: POleStr): HResult; stdcall;
    function   RemoveItem(szItemID: POleStr): HResult; stdcall;
    function   ClearItems: HResult; stdcall;
  end;

  IXRTLOPCDAVariantManager = interface
  ['{1A19161E-84F0-4C01-9A7C-9A1DA4C8FAE0}']
    function   IsTypeSupported(vtDataType: DWORD; out bResult: BOOL): HResult; stdcall;
    function   Convert(out Dest: OLEVariant; vtDestType: DWORD; Src: OLEVariant; dwLocaleID: TLCID): HResult; stdcall;
    function   CanConvert(vtSrcDataType, vtDestDataType: DWORD; out bResult: BOOL): HResult; stdcall;
    function   VarEqual(Var1, Var2: OLEVariant; out bResult: BOOL): HResult; stdcall;
  end;

implementation

end.

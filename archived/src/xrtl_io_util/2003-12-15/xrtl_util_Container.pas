unit xrtl_util_Container;

interface

uses
  SysUtils,
  xrtl_base_Compare, xrtl_base_Exception;

type
  EXRTLInvalidContainerItemData = class(EXRTLException);
  EXRTLInvalidContainerCompare  = class(EXRTLException);

  TXRTLContainer        = class;
  TXRTLContainerAdapter = class;

  TXRTLContainerItem = class
  private
  protected
    FContainer: TXRTLContainer;
    FContainerAdapter: TXRTLContainerAdapter;
  public
    constructor Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
    property   Container: TXRTLContainer read FContainer;
    property   ContainerAdapter: TXRTLContainerAdapter read FContainerAdapter;
  end;

  TXRTLContainerAdapter = class
  protected
  public
    Container: TXRTLContainer;
    function   Compare(const Item1, Item2: TXRTLContainerItem): TXRTLValueRelationship; overload;
    function   CreateContainerItem: TXRTLContainerItem; virtual; abstract;
    procedure  CheckData(const Data: Pointer); virtual; abstract;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; overload; virtual; abstract;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); virtual; abstract;
    function   GetData(const Item: TXRTLContainerItem): Pointer; virtual; abstract;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; virtual; abstract;
  end;

  TXRTLFindItemCompareProc = function(Index: Integer; const AItem: Pointer): TXRTLValueRelationship of object;

  TXRTLContainer = class
  protected
  public
    procedure  Clear; virtual; abstract;
    function   GetCount: Integer; virtual; abstract;
    function   FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean; virtual;
  end;

  TXRTLSortedContainerDuplicates = (dupAllow, dupIgnore, dupError);

  TXRTLSortedContainer = class(TXRTLContainer)
  protected
    FSorted: Boolean;
    FDuplicates: TXRTLSortedContainerDuplicates;
  public
    constructor Create;
    function   FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean; override;
    procedure  SetSorted(const ASorted: Boolean); virtual;
    function   GetSorted: Boolean; virtual;
    procedure  SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates); virtual;
    function   GetDuplicates: TXRTLSortedContainerDuplicates; virtual;
    procedure  Sort; virtual; abstract;
  end;

  TXRTLInterfaceContainerAdapter = class(TXRTLContainerAdapter)
  public
    function   CreateContainerItem: TXRTLContainerItem; override;
    procedure  CheckData(const Data: Pointer); override;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); override;
    function   GetData(const Item: TXRTLContainerItem): Pointer; override;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; override;
  end;

  TXRTLObjectContainerAdapter = class(TXRTLContainerAdapter)
  private
    FOwnObjects: Boolean;
  public
    constructor Create(const AOwnObjects: Boolean = True);
    property   OwnObjects: Boolean read FOwnObjects write FOwnObjects;
    function   CreateContainerItem: TXRTLContainerItem; override;
    procedure  CheckData(const Data: Pointer); override;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); override;
    function   GetData(const Item: TXRTLContainerItem): Pointer; override;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; override;
  end;

  TXRTLPointerContainerAdapter = class(TXRTLContainerAdapter)
  public
    function   CreateContainerItem: TXRTLContainerItem; override;
    procedure  CheckData(const Data: Pointer); override;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); override;
    function   GetData(const Item: TXRTLContainerItem): Pointer; override;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; override;
  end;

  TXRTLVariantContainerAdapter = class(TXRTLContainerAdapter)
  public
    function   CreateContainerItem: TXRTLContainerItem; override;
    procedure  CheckData(const Data: Pointer); override;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); override;
    function   GetData(const Item: TXRTLContainerItem): Pointer; override;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; override;
  end;

  TXRTLWideStringContainerAdapter = class(TXRTLContainerAdapter)
  public
    function   CreateContainerItem: TXRTLContainerItem; override;
    procedure  CheckData(const Data: Pointer); override;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
    procedure  SetData(const Item: TXRTLContainerItem; const AData: Pointer); override;
    function   GetData(const Item: TXRTLContainerItem): Pointer; override;
    function   ExtractData(const Item: TXRTLContainerItem): Pointer; override;
  end;

implementation

{ TXRTLContainerItem }

constructor TXRTLContainerItem.Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited Create;
  FContainer:= AContainer;
  FContainerAdapter:= AContainerAdapter;
end;

{ TXRTLContainerAdapter }

function TXRTLContainerAdapter.Compare(const Item1, Item2: TXRTLContainerItem): TXRTLValueRelationship;
begin
  if Item1.ContainerAdapter.ClassType <> Item2.ContainerAdapter.ClassType then
    XRTLInvalidOperation(ClassName, 'Compare', 'can''t compare items created with different adapter types');
  Result:= Compare(GetData(Item1), GetData(Item2));
end;

{ TXRTLContainer }

function TXRTLContainer.FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean;
var
  I: Integer;
begin
  Result:= False;
  Index:= 0;
  for I:= 0 to GetCount - 1 do
  begin
    if FindItemCompare(I, AItem) = XRTLEqualsValue then
    begin
      Index:= I;
      Result:= True;
      Exit;
    end;
  end;
end;

{ TXRTLSortedContainer }

constructor TXRTLSortedContainer.Create;
begin
  inherited;
  FSorted:= False;
  FDuplicates:= dupAllow;
end;

function TXRTLSortedContainer.GetSorted: Boolean;
begin
  Result:= FSorted;
end;

procedure TXRTLSortedContainer.SetSorted(const ASorted: Boolean);
begin
  if FSorted <> ASorted then
  begin
    FSorted:= ASorted;
    if FSorted then
      Sort;
  end;
end;

function TXRTLSortedContainer.FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean;
var
  L, H, I: Integer;
  C: TXRTLValueRelationship;
begin
  Result:= False;
  if FSorted then
  begin
// binary search on sorted sequence
    L:= 0;
    H:= GetCount - 1;
    while L <= H do
    begin
      I:= (L + H) shr 1;
      C:= FindItemCompare(I, AItem);
      if C = XRTLLessThanValue then
      begin
        L:= I + 1;
      end
      else
      begin
        H:= I - 1;
        if C = XRTLEqualsValue then
        begin
          Result:= True;
          L:= I;
        end;
      end;
    end;
    Index:= L;
  end
  else
    Result:= inherited FindItem(AItem, Index, FindItemCompare);
end;

function TXRTLSortedContainer.GetDuplicates: TXRTLSortedContainerDuplicates;
begin
  Result:= FDuplicates;
end;

procedure TXRTLSortedContainer.SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates);
begin
  FDuplicates:= ADuplicates;
end;

type
  TXRTLInterfaceContainerItem = class(TXRTLContainerItem)
  public
    Data: IInterface;
    destructor Destroy; override;
  end;

{ TXRTLInterfaceContainerItem }

destructor TXRTLInterfaceContainerItem.Destroy;
begin
  Data:= nil;
  inherited;
end;

{ TXRTLInterfaceContainerAdapter }

function TXRTLInterfaceContainerAdapter.CreateContainerItem: TXRTLContainerItem;
begin
  Result:= TXRTLInterfaceContainerItem.Create(Container, Self);
end;

procedure TXRTLInterfaceContainerAdapter.CheckData(const Data: Pointer);
begin
end;

function TXRTLInterfaceContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  AD1, AD2: Integer;
begin
  CheckData(Data1);
  CheckData(Data2);
  AD1:= Integer(Data1);
  AD2:= Integer(Data2);
  if AD1 < AD2 then
  begin
    Result:= XRTLLessThanValue;
  end
  else
  begin
    if AD1 > AD2 then
    begin
      Result:= XRTLGreaterThanValue;
    end
    else
    begin
      Result:= XRTLEqualsValue;
    end;
  end;
end;

function TXRTLInterfaceContainerAdapter.GetData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= Pointer((Item as TXRTLInterfaceContainerItem).Data);
end;

procedure TXRTLInterfaceContainerAdapter.SetData(const Item: TXRTLContainerItem; const AData: Pointer);
begin
  CheckData(AData);
  (Item as TXRTLInterfaceContainerItem).Data:= IInterface(AData);
end;

function TXRTLInterfaceContainerAdapter.ExtractData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= Pointer((Item as TXRTLInterfaceContainerItem).Data);
  Pointer((Item as TXRTLInterfaceContainerItem).Data):= nil;
end;

type
  TXRTLObjectContainerItem = class(TXRTLContainerItem)
  public
    Data: TObject;
    constructor Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
  end;

{ TXRTLObjectContainerItem }

constructor TXRTLObjectContainerItem.Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited;
  Data:= nil;
end;

destructor TXRTLObjectContainerItem.Destroy;
begin
  if (FContainerAdapter as TXRTLObjectContainerAdapter).OwnObjects then
    FreeAndNil(Data);
  inherited;
end;

{ TXRTLObjectContainerAdapter }

constructor TXRTLObjectContainerAdapter.Create(const AOwnObjects: Boolean = True);
begin
  inherited Create;
  FOwnObjects:= AOwnObjects;
end;

function TXRTLObjectContainerAdapter.CreateContainerItem: TXRTLContainerItem;
begin
  Result:= TXRTLObjectContainerItem.Create(Container, Self);
end;

procedure TXRTLObjectContainerAdapter.CheckData(const Data: Pointer);
begin
end;

function TXRTLObjectContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  AD1, AD2: Integer;
begin
  CheckData(Data1);
  CheckData(Data2);
  AD1:= Integer(Data1);
  AD2:= Integer(Data2);
  if AD1 < AD2 then
  begin
    Result:= XRTLLessThanValue;
  end
  else
  begin
    if AD1 > AD2 then
    begin
      Result:= XRTLGreaterThanValue;
    end
    else
    begin
      Result:= XRTLEqualsValue;
    end;
  end;
end;

function TXRTLObjectContainerAdapter.GetData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= (Item as TXRTLObjectContainerItem).Data;
end;

procedure TXRTLObjectContainerAdapter.SetData(const Item: TXRTLContainerItem; const AData: Pointer);
begin
  CheckData(AData);
  (Item as TXRTLObjectContainerItem).Data:= AData;
end;

function TXRTLObjectContainerAdapter.ExtractData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= (Item as TXRTLObjectContainerItem).Data;
  (Item as TXRTLObjectContainerItem).Data:= nil;
end;

type
  TXRTLPointerContainerItem = class(TXRTLContainerItem)
  public
    Data: Pointer;
  end;

{ TXRTLPointerContainerAdapter }

function TXRTLPointerContainerAdapter.CreateContainerItem: TXRTLContainerItem;
begin
  Result:= TXRTLPointerContainerItem.Create(Container, Self);
end;

procedure TXRTLPointerContainerAdapter.CheckData(const Data: Pointer);
begin
end;

function TXRTLPointerContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  AD1, AD2: Integer;
begin
  CheckData(Data1);
  CheckData(Data2);
  AD1:= Integer(Data1);
  AD2:= Integer(Data2);
  if AD1 < AD2 then
  begin
    Result:= XRTLLessThanValue;
  end
  else
  begin
    if AD1 > AD2 then
    begin
      Result:= XRTLGreaterThanValue;
    end
    else
    begin
      Result:= XRTLEqualsValue;
    end;
  end;
end;

function TXRTLPointerContainerAdapter.GetData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= (Item as TXRTLPointerContainerItem).Data;
end;

procedure TXRTLPointerContainerAdapter.SetData(const Item: TXRTLContainerItem; const AData: Pointer);
begin
  CheckData(AData);
  (Item as TXRTLPointerContainerItem).Data:= AData;;
end;

function TXRTLPointerContainerAdapter.ExtractData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= (Item as TXRTLPointerContainerItem).Data;
  (Item as TXRTLPointerContainerItem).Data:= nil;
end;

type
  TXRTLVariantContainerItem = class(TXRTLContainerItem)
  public
    Data: Variant;
    constructor Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
  end;

{ TXRTLVariantContainerItem }

constructor TXRTLVariantContainerItem.Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited;
  VarClear(Data);
end;

destructor TXRTLVariantContainerItem.Destroy;
begin
  VarClear(Data);
  inherited;
end;

{ TXRTLVariantContainerAdapter }

function TXRTLVariantContainerAdapter.CreateContainerItem: TXRTLContainerItem;
begin
  Result:= TXRTLVariantContainerItem.Create(Container, Self);
end;

procedure TXRTLVariantContainerAdapter.CheckData(const Data: Pointer);
begin
  if Data = nil then
    raise EXRTLInvalidContainerItemData.Create('Data can''t be null');
end;

function TXRTLVariantContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  V1, V2: Variant;
begin
  CheckData(Data1);
  CheckData(Data2);
  V1:= Variant(Data1^);
  V2:= Variant(Data2^);
  if V1 < V2 then
  begin
    Result:= XRTLLessThanValue
  end
  else
  begin
    if V1 > V2 then
      Result:= XRTLGreaterThanValue
    else
      Result:= XRTLEqualsValue;
  end;
end;

function TXRTLVariantContainerAdapter.GetData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= @(Item as TXRTLVariantContainerItem).Data;
end;

procedure TXRTLVariantContainerAdapter.SetData(const Item: TXRTLContainerItem; const AData: Pointer);
begin
  CheckData(AData);
  (Item as TXRTLVariantContainerItem).Data:= Variant(AData^);
end;

function TXRTLVariantContainerAdapter.ExtractData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= @(Item as TXRTLVariantContainerItem).Data;
  VarClear((Item as TXRTLVariantContainerItem).Data);
end;

type
  TXRTLWideStringContainerItem = class(TXRTLContainerItem)
  public
    Data: WideString;
    constructor Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
  end;

{ TXRTLWideStringContainerItem }

constructor TXRTLWideStringContainerItem.Create(const AContainer: TXRTLContainer; const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited;
  Data:= '';
end;

destructor TXRTLWideStringContainerItem.Destroy;
begin
  Data:= '';
  inherited;
end;

{ TXRTLWideStringContainerAdapter }

function TXRTLWideStringContainerAdapter.CreateContainerItem: TXRTLContainerItem;
begin
  Result:= TXRTLWideStringContainerItem.Create(Container, Self);
end;

procedure TXRTLWideStringContainerAdapter.CheckData(const Data: Pointer);
begin
  if Data = nil then
    raise EXRTLInvalidContainerItemData.Create('Data can''t be null');
end;

function TXRTLWideStringContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  WS1, WS2: WideString;
begin
  CheckData(Data1);
  CheckData(Data2);
  WS1:= WideString(Data1^);
  WS2:= WideString(Data2^);
  Result:= WideCompareStr(WS1, WS2);
end;

function TXRTLWideStringContainerAdapter.GetData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= @(Item as TXRTLWideStringContainerItem).Data;
end;

procedure TXRTLWideStringContainerAdapter.SetData(const Item: TXRTLContainerItem; const AData: Pointer);
begin
  CheckData(AData);
  (Item as TXRTLWideStringContainerItem).Data:= WideString(AData^);
end;

function TXRTLWideStringContainerAdapter.ExtractData(const Item: TXRTLContainerItem): Pointer;
begin
  Result:= @(Item as TXRTLWideStringContainerItem).Data;
  (Item as TXRTLWideStringContainerItem).Data:= '';
end;

end.

unit xrtl_util_Map;

interface

uses
  SysUtils,
  xrtl_base_Compare,  
  xrtl_util_Container,
  xrtl_util_List, xrtl_util_Synchronizer;

type
  TXRTLMap             = class;

  TXRTLMapKeyValuePair = class
  private
    FMap: TXRTLMap;
    FValue: TXRTLContainerItem;
    FKey: TXRTLContainerItem;
  public
    constructor Create(const AMap: TXRTLMap);
    destructor Destroy; override;
    property   Map: TXRTLMap read FMap;
    property   Key: TXRTLContainerItem read FKey;
    property   Value: TXRTLContainerItem read FValue;
  end;

  TXRTLMap = class(TXRTLSortedContainer)
  private
    FPairs: TXRTLList;
    function   GetPairs(Index: Integer): TXRTLMapKeyValuePair;
    function   FindKeyCompare(Index: Integer; const AItem: Pointer): TXRTLValueRelationship;
  protected
    FKeyContainerAdapter: TXRTLContainerAdapter;
    FValueContainerAdapter: TXRTLContainerAdapter;
    function   FindKey(const AKey: Pointer; var Index: Integer): Boolean;
    property   Pairs[Index: Integer]: TXRTLMapKeyValuePair read GetPairs;
  public
    constructor Create(const AKeyContainerAdapter, AValueContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
    procedure  BeforeDestruction; override;
    property   KeyContainerAdapter: TXRTLContainerAdapter read FKeyContainerAdapter;
    property   ValueContainerAdapter: TXRTLContainerAdapter read FValueContainerAdapter;
    procedure  Clear; override;
    procedure  Sort; override;
    function   GetCount: Integer; override;
    function   ContainsKey(const AKey: Pointer): Boolean; virtual;
    function   ContainsValue(const AValue: Pointer): Boolean; virtual;
    function   Get(const AKey: Pointer): Pointer; virtual;
    function   Put(const AKey: Pointer; const AValue: Pointer): Pointer; virtual;
    procedure  Remove(const AKey: Pointer); virtual;
    function   Extract(const AKey: Pointer): Pointer; virtual;
    function   GetIsEmpty: Boolean; virtual;
    function   GetKey(Index: Integer): Pointer; virtual;
    function   GetValue(Index: Integer): Pointer; virtual;
  end;

  TXRTLFilterMap = class(TXRTLMap)
  private
    FOwnCoreMap: Boolean;
    FCoreMap: TXRTLMap;
  protected
  public
    constructor Create(const ACoreMap: TXRTLMap; AOwnCoreMap: Boolean = True);
    destructor Destroy; override;
    function   ContainsKey(const AKey: Pointer): Boolean; override;
    function   ContainsValue(const AValue: Pointer): Boolean; override;
    function   Get(const AKey: Pointer): Pointer; override;
    function   Put(const AKey: Pointer; const AValue: Pointer): Pointer; override;
    procedure  Remove(const AKey: Pointer); override;
    function   Extract(const AKey: Pointer): Pointer; override;
    property   OwnCoreMap: Boolean read FOwnCoreMap write FOwnCoreMap;
    property   CoreMap: TXRTLMap read FCoreMap;
    function   GetCount: Integer; override;
    function   GetIsEmpty: Boolean; override;
    function   GetKey(Index: Integer): Pointer; override;
    function   GetValue(Index: Integer): Pointer; override;
    function   FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean; override;
    procedure  SetSorted(const ASorted: Boolean); override;
    function   GetSorted: Boolean; override;
    procedure  SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates); override;
    function   GetDuplicates: TXRTLSortedContainerDuplicates; override;
    procedure  Sort; override;
  end;

  TXRTLSynchronizedMap = class(TXRTLFilterMap)
  private
    FSynchronizer: TXRTLSynchronizer;
  protected
  public
    constructor Create(const ACoreMap: TXRTLMap; AOwnCoreMap: Boolean = True;
                       const ASynchronizer: TXRTLSynchronizer = nil);
    destructor Destroy; override;
    procedure  Clear; override;
    procedure  Sort; override;
    function   ContainsKey(const AKey: Pointer): Boolean; override;
    function   ContainsValue(const AValue: Pointer): Boolean; override;
    function   Get(const AKey: Pointer): Pointer; override;
    function   Put(const AKey: Pointer; const AValue: Pointer): Pointer; override;
    procedure  Remove(const AKey: Pointer); override;
    function   Extract(const AKey: Pointer): Pointer; override;
    procedure  BeginRead;
    procedure  EndRead;
    procedure  BeginWrite;
    procedure  EndWrite;
    function   GetCount: Integer; override;
    function   GetIsEmpty: Boolean; override;
    function   GetKey(Index: Integer): Pointer; override;
    function   GetValue(Index: Integer): Pointer; override;
    function   FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean; override;
    procedure  SetSorted(const ASorted: Boolean); override;
    function   GetSorted: Boolean; override;
    procedure  SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates); override;
    function   GetDuplicates: TXRTLSortedContainerDuplicates; override;
  end;

implementation

type
  TXRTLMapObjectContainerAdapter = class(TXRTLObjectContainerAdapter)
  public
    Map: TXRTLMap;
    constructor Create;
    function   Compare(const Data1, Data2: Pointer): TXRTLValueRelationship; override;
  end;

{ TXRTLMapObjectContainerAdapter }

constructor TXRTLMapObjectContainerAdapter.Create;
begin
  inherited Create(True);
end;

function TXRTLMapObjectContainerAdapter.Compare(const Data1, Data2: Pointer): TXRTLValueRelationship;
var
  Pair1, Pair2: TXRTLMapKeyValuePair;
begin
  Pair1:= TXRTLMapKeyValuePair(Data1);
  Pair2:= TXRTLMapKeyValuePair(Data2);
  Result:= Map.KeyContainerAdapter.Compare(Pair1.Key, Pair2.Key);
end;

{ TXRTLMapKeyValuePair }

constructor TXRTLMapKeyValuePair.Create(const AMap: TXRTLMap);
begin
  inherited Create;
  FMap:= AMap;
  FKey:= FMap.KeyContainerAdapter.CreateContainerItem;
  FValue:= FMap.ValueContainerAdapter.CreateContainerItem;
end;

destructor TXRTLMapKeyValuePair.Destroy;
begin
  FreeAndNil(FValue);
  FreeAndNil(FKey);
  inherited;
end;

{ TXRTLMap }

constructor TXRTLMap.Create(const AKeyContainerAdapter, AValueContainerAdapter: TXRTLContainerAdapter);
var
  MOCA: TXRTLMapObjectContainerAdapter;
begin
  inherited Create;
  FKeyContainerAdapter:= AKeyContainerAdapter;
  FKeyContainerAdapter.Container:= Self;
  FValueContainerAdapter:= AValueContainerAdapter;
  FValueContainerAdapter.Container:= Self;
  MOCA:= TXRTLMapObjectContainerAdapter.Create;
  MOCA.Map:= Self;
  FPairs:= TXRTLList.Create(MOCA);
end;

destructor TXRTLMap.Destroy;
begin
  FreeAndNil(FPairs);
  FreeAndNil(FValueContainerAdapter);
  FreeAndNil(FKeyContainerAdapter);
  inherited;
end;

procedure TXRTLMap.BeforeDestruction;
begin
  Clear;
  inherited;
end;

function TXRTLMap.GetPairs(Index: Integer): TXRTLMapKeyValuePair;
begin
  Result:= FPairs.GetItem(Index);
end;

function TXRTLMap.GetIsEmpty: Boolean;
begin
  Result:= GetCount = 0;
end;

function TXRTLMap.GetKey(Index: Integer): Pointer;
begin
  Result:= KeyContainerAdapter.GetData(Pairs[Index].Key);
end;

function TXRTLMap.GetValue(Index: Integer): Pointer;
begin
  Result:= ValueContainerAdapter.GetData(Pairs[Index].Value);
end;

function TXRTLMap.GetCount: Integer;
begin
  Result:= FPairs.GetCount;
end;

procedure TXRTLMap.Clear;
begin
  FPairs.Clear;
end;

function TXRTLMap.ContainsKey(const AKey: Pointer): Boolean;
var
  KeyIndex: Integer;
begin
  KeyContainerAdapter.CheckData(AKey);
  Result:= FindKey(AKey, KeyIndex);
end;

function TXRTLMap.ContainsValue(const AValue: Pointer): Boolean;
var
  I: Integer;
begin
  ValueContainerAdapter.CheckData(AValue);
  Result:= False;
  for I:= 0 to GetCount - 1 do
  begin
    Result:= ValueContainerAdapter.Compare(GetValue(I), AValue) = XRTLEqualsValue;
    if Result then Exit;
  end;
end;

function TXRTLMap.Get(const AKey: Pointer): Pointer;
var
  Index: Integer;
begin
  KeyContainerAdapter.CheckData(AKey);
  Result:= nil;
  if FindKey(AKey, Index) then
    Result:= GetValue(Index);
end;

function TXRTLMap.Put(const AKey, AValue: Pointer): Pointer;
var
  InsertIndex: Integer;
  KeyValuePair: TXRTLMapKeyValuePair;
  ShouldInsert: Boolean;
begin
  KeyContainerAdapter.CheckData(AKey);
  ValueContainerAdapter.CheckData(AValue);
  try
    ShouldInsert:= not FindKey(AKey, InsertIndex);
    if ShouldInsert then
    begin
      KeyValuePair:= TXRTLMapKeyValuePair.Create(Self);
      KeyContainerAdapter.SetData(KeyValuePair.Key, AKey);
    end
    else
    begin
      KeyValuePair:= Pairs[InsertIndex];
    end;
    Result:= ValueContainerAdapter.GetData(KeyValuePair.Value);
    ValueContainerAdapter.SetData(KeyValuePair.Value, AValue);
    if ShouldInsert then
      FPairs.Insert(InsertIndex, KeyValuePair);
  except
    FreeAndNil(KeyValuePair);
    raise;
  end;
end;

procedure TXRTLMap.Remove(const AKey: Pointer);
var
  RemoveIndex: Integer;
  KeyValuePair: TXRTLMapKeyValuePair;
begin
  KeyContainerAdapter.CheckData(AKey);
  if FindKey(AKey, RemoveIndex) then
  begin
    KeyValuePair:= Pairs[RemoveIndex];
    FPairs.Remove(KeyValuePair);
  end;
end;

function TXRTLMap.Extract(const AKey: Pointer): Pointer;
var
  RemoveIndex: Integer;
  KeyValuePair: TXRTLMapKeyValuePair;
begin
  Result:= nil;
  KeyContainerAdapter.CheckData(AKey);
  if FindKey(AKey, RemoveIndex) then
  begin
    KeyValuePair:= Pairs[RemoveIndex];
    Result:= ValueContainerAdapter.ExtractData(KeyValuePair.Value);
    FPairs.Remove(KeyValuePair);
  end;
end;

procedure TXRTLMap.Sort;
begin
  FPairs.Sort;
end;

function TXRTLMap.FindKeyCompare(Index: Integer; const AItem: Pointer): TXRTLValueRelationship;
begin
  Result:= KeyContainerAdapter.Compare(GetKey(Index), AItem);
end;

function TXRTLMap.FindKey(const AKey: Pointer; var Index: Integer): Boolean;
begin
  Result:= FindItem(AKey, Index, FindKeyCompare);
end;

{ TXRTLFilterMap }

constructor TXRTLFilterMap.Create(const ACoreMap: TXRTLMap; AOwnCoreMap: Boolean = True);
begin
  inherited Create(ACoreMap.KeyContainerAdapter, ACoreMap.ValueContainerAdapter);
  FCoreMap:= ACoreMap;
  FOwnCoreMap:= AOwnCoreMap;
end;

destructor TXRTLFilterMap.Destroy;
begin
  FValueContainerAdapter:= nil;
  FKeyContainerAdapter:= nil;
  if FOwnCoreMap then
    FreeAndNil(FCoreMap);
  inherited;
end;

function TXRTLFilterMap.GetCount: Integer;
begin
  Result:= FCoreMap.GetCount;
end;

function TXRTLFilterMap.GetIsEmpty: Boolean;
begin
  Result:= FCoreMap.GetIsEmpty;
end;

procedure TXRTLFilterMap.SetSorted(const ASorted: Boolean);
begin
  FCoreMap.SetSorted(ASorted);
end;

function TXRTLFilterMap.GetSorted: Boolean;
begin
  Result:= FCoreMap.GetSorted;
end;

function TXRTLFilterMap.ContainsKey(const AKey: Pointer): Boolean;
begin
  Result:= FCoreMap.ContainsKey(AKey);
end;

function TXRTLFilterMap.ContainsValue(const AValue: Pointer): Boolean;
begin
  Result:= FCoreMap.ContainsValue(AValue);
end;

function TXRTLFilterMap.Get(const AKey: Pointer): Pointer;
begin
  Result:= FCoreMap.Get(AKey);
end;

function TXRTLFilterMap.Put(const AKey, AValue: Pointer): Pointer;
begin
  Result:= FCoreMap.Put(AKey, AValue);
end;

procedure TXRTLFilterMap.Remove(const AKey: Pointer);
begin
  FCoreMap.Remove(AKey);
end;

function TXRTLFilterMap.Extract(const AKey: Pointer): Pointer;
begin
  Result:= FCoreMap.Extract(AKey);
end;

function TXRTLFilterMap.GetKey(Index: Integer): Pointer;
begin
  Result:= FCoreMap.GetKey(Index);
end;

function TXRTLFilterMap.GetValue(Index: Integer): Pointer;
begin
  Result:= FCoreMap.GetValue(Index);
end;

function TXRTLFilterMap.FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean;
begin
  Result:= FCoreMap.FindItem(AItem, Index, FindItemCompare);
end;

function TXRTLFilterMap.GetDuplicates: TXRTLSortedContainerDuplicates;
begin
  Result:= FCoreMap.GetDuplicates;
end;

procedure TXRTLFilterMap.SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates);
begin
  FCoreMap.SetDuplicates(ADuplicates);
end;

procedure TXRTLFilterMap.Sort;
begin
  FCoreMap.Sort;
end;

{ TXRTLSynchronizedMap }

constructor TXRTLSynchronizedMap.Create(const ACoreMap: TXRTLMap;
  AOwnCoreMap: Boolean = True; const ASynchronizer: TXRTLSynchronizer = nil);
begin
  inherited Create(ACoreMap, AOwnCoreMap);
  FSynchronizer:= ASynchronizer;
  if not Assigned(FSynchronizer) then
    FSynchronizer:= TXRTLCriticalSectionSynchronizer.Create;
end;

destructor TXRTLSynchronizedMap.Destroy;
begin
  FreeAndNil(FSynchronizer);
  inherited;
end;

function TXRTLSynchronizedMap.GetIsEmpty: Boolean;
begin
  try
    BeginRead;
    Result:= CoreMap.GetIsEmpty;
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.GetCount: Integer;
begin
  try
    BeginRead;
    Result:= CoreMap.GetCount;
  finally
    EndRead;
  end;
end;

procedure TXRTLSynchronizedMap.SetSorted(const ASorted: Boolean);
begin
  try
    BeginWrite;
    CoreMap.SetSorted(ASorted);
  finally
    EndWrite;
  end;
end;

function TXRTLSynchronizedMap.GetSorted: Boolean;
begin
  Result:= CoreMap.GetSorted;
end;

function TXRTLSynchronizedMap.ContainsKey(const AKey: Pointer): Boolean;
begin
  try
    BeginRead;
    Result:= CoreMap.ContainsKey(AKey);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.ContainsValue(const AValue: Pointer): Boolean;
begin
  try
    BeginRead;
    Result:= CoreMap.ContainsValue(AValue);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.Get(const AKey: Pointer): Pointer;
begin
  try
    BeginRead;
    Result:= CoreMap.Get(AKey);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.Put(const AKey, AValue: Pointer): Pointer;
begin
  try
    BeginWrite;
    Result:= CoreMap.Put(AKey, AValue);
  finally
    EndWrite;
  end;
end;

procedure TXRTLSynchronizedMap.Remove(const AKey: Pointer);
begin
  try
    BeginWrite;
    CoreMap.Remove(AKey);
  finally
    EndWrite;
  end;
end;

function TXRTLSynchronizedMap.Extract(const AKey: Pointer): Pointer;
begin
  try
    BeginWrite;
    Result:= CoreMap.Extract(AKey);
  finally
    EndWrite;
  end;
end;

procedure TXRTLSynchronizedMap.Clear;
begin
  try
    BeginWrite;
    CoreMap.Clear;
  finally
    EndWrite;
  end;
end;

procedure TXRTLSynchronizedMap.Sort;
begin
  try
    BeginWrite;
    CoreMap.Sort;
  finally
    EndWrite;
  end;
end;

procedure TXRTLSynchronizedMap.BeginRead;
begin
  FSynchronizer.BeginRead;
end;

procedure TXRTLSynchronizedMap.EndRead;
begin
  FSynchronizer.EndRead;
end;

procedure TXRTLSynchronizedMap.BeginWrite;
begin
  FSynchronizer.BeginWrite;
end;

procedure TXRTLSynchronizedMap.EndWrite;
begin
  FSynchronizer.EndWrite;
end;

function TXRTLSynchronizedMap.GetKey(Index: Integer): Pointer;
begin
  try
    BeginRead;
    Result:= CoreMap.GetKey(Index);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.GetValue(Index: Integer): Pointer;
begin
  try
    BeginRead;
    Result:= CoreMap.GetValue(Index);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.FindItem(const AItem: Pointer; var Index: Integer; FindItemCompare: TXRTLFindItemCompareProc): Boolean;
begin
  try
    BeginRead;
    Result:= CoreMap.FindItem(AItem, Index, FindItemCompare);
  finally
    EndRead;
  end;
end;

function TXRTLSynchronizedMap.GetDuplicates: TXRTLSortedContainerDuplicates;
begin
  try
    BeginRead;
    Result:= CoreMap.GetDuplicates;
  finally
    EndRead;
  end;
end;

procedure TXRTLSynchronizedMap.SetDuplicates(const ADuplicates: TXRTLSortedContainerDuplicates);
begin
  try
    BeginWrite;
    CoreMap.SetDuplicates(ADuplicates);
  finally
    EndWrite;
  end;
end;

end.


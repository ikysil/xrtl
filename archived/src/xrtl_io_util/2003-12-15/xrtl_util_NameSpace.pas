unit xrtl_util_NameSpace;

interface

uses
  SysUtils, Classes, Contnrs, Masks,
  xrtl_base_Exception,
  xrtl_util_COMUtils, xrtl_util_Map, xrtl_util_Container,
  xrtl_util_NameSpacePath;

type
  EXRTLInvalidNameSpacePath = class(EXRTLException);

  TXRTLNameSpaceEnumeratorType = (nsEnumerateBranches, // entries with SubItems.Size > 0
                                  nsEnumerateLeaves,   // entries with SubItems.Size = 0
                                  nsEnumerateRecursive,
                                  nsEnumerateBranchesAndLeaves);

  TXRTLNameSpace = class;

  TXRTLNameSpaceEntry = class
  private
    FSubItems: TXRTLMap;
    FParent: TXRTLNameSpaceEntry;
    FNameSpace: TXRTLNameSpace;
  public
    Data: TXRTLContainerItem;
    constructor Create(const ANameSpace: TXRTLNameSpace; const AParent: TXRTLNameSpaceEntry = nil);
    destructor Destroy; override;
    property   SubItems: TXRTLMap read FSubItems;
    property   Parent: TXRTLNameSpaceEntry read FParent;
  end;

  TXRTLNameSpaceEnumerator = class(TXRTLEnumXXXX)
  private
    FItems: TObjectList;
    procedure  EnumItems(const RootEntry: TXRTLNameSpaceEntry;
                         const RootPath: TXRTLNameSpacePath;
                         EnumType: TXRTLNameSpaceEnumeratorType;
                         const NameFilter: WideString);
  protected
    function   GetItem(const ItemIndex: LongInt): Pointer; override;
    function   GetCount: LongInt; override;
  public
    constructor Create(const RootEntry: TXRTLNameSpaceEntry; EnumType: TXRTLNameSpaceEnumeratorType;
                       const NameFilter: WideString = '');
    destructor Destroy; override;
  end;

  TXRTLNameSpace = class(TXRTLContainer)
  private
    FRoot: TXRTLNameSpaceEntry;
    function   GetNameSpaceEntry(const Path: TXRTLNameSpacePath; AutoCreate: Boolean = False): TXRTLNameSpaceEntry;
    procedure  CheckPath(const Path: TXRTLNameSpacePath);
  protected
    FValueContainerAdapter: TXRTLContainerAdapter;
  public
    constructor Create(const AValueContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
    property   ValueContainerAdapter: TXRTLContainerAdapter read FValueContainerAdapter;
    function   Put(const Path: TXRTLNameSpacePath; Data: Pointer = nil): Pointer; virtual;
    function   Get(const Path: TXRTLNameSpacePath): Pointer; virtual;
    procedure  Remove(const Path: TXRTLNameSpacePath); virtual;
    function   Contains(const Path: TXRTLNameSpacePath): Boolean; virtual;
    procedure  Clear; override;
    function   CreateEnumerator(const RootPath: TXRTLNameSpacePath;
                                EnumType: TXRTLNameSpaceEnumeratorType;
                                const NameFilter: WideString = '*'): TXRTLNameSpaceEnumerator; virtual;
    function   GetCount: Integer; override;
    function   IsLeaf(const Path: TXRTLNameSpacePath): Boolean; virtual;
    function   IsBranch(const Path: TXRTLNameSpacePath): Boolean; virtual;
  end;

implementation

{ TXRTLNameSpaceEntry }

constructor TXRTLNameSpaceEntry.Create(const ANameSpace: TXRTLNameSpace; const AParent: TXRTLNameSpaceEntry = nil);
begin
  inherited Create;
  FNameSpace:= ANameSpace;
  Data:= FNameSpace.ValueContainerAdapter.CreateContainerItem;
  FSubItems:= TXRTLMap.Create(TXRTLWideStringContainerAdapter.Create,
                              TXRTLObjectContainerAdapter.Create(True));
  FSubItems.SetSorted(True);
  FParent:= AParent;
end;

destructor TXRTLNameSpaceEntry.Destroy;
begin
  FreeAndNil(FSubItems);
  FreeAndNil(Data);
  inherited;
end;

type
  TXRTLNameSpaceEnumeratorItem = class
  public
    Path: TXRTLNameSpacePath;
  end;

{ TXRTLNameSpaceEnumerator }

constructor TXRTLNameSpaceEnumerator.Create(const RootEntry: TXRTLNameSpaceEntry;
                                            EnumType: TXRTLNameSpaceEnumeratorType;
                                            const NameFilter: WideString = '');
begin
  inherited Create;
  FItems:= TObjectList.Create(True);
  EnumItems(RootEntry, XRTLNameSpacePathCreate([]), EnumType, NameFilter);
end;

destructor TXRTLNameSpaceEnumerator.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TXRTLNameSpaceEnumerator.EnumItems(const RootEntry: TXRTLNameSpaceEntry;
                                             const RootPath: TXRTLNameSpacePath;
                                             EnumType: TXRTLNameSpaceEnumeratorType;
                                             const NameFilter: WideString);
var
  I: Integer;
  SubPath: TXRTLNameSpacePath;
  Entry: TXRTLNameSpaceEntry;
  Item: TXRTLNameSpaceEnumeratorItem;
  SubName: WideString;
begin
  SubPath:= XRTLNameSpacePathCreate([]);
  if not Assigned(RootEntry) then Exit;
  for I:= 0 to RootEntry.SubItems.GetCount - 1 do
  begin
    Entry:= RootEntry.SubItems.GetValue(I);
    SubName:= WideString(RootEntry.SubItems.GetKey(I)^);
    if not MatchesMask(SubName, NameFilter) then
      Continue;
    SubPath:= XRTLNameSpacePathConcat(RootPath, XRTLNameSpacePathCreate([SubName]));
    case EnumType of
      nsEnumerateBranches:
      begin
        if Entry.SubItems.GetCount > 0 then
        begin
          Item:= TXRTLNameSpaceEnumeratorItem.Create;
          Item.Path:= Copy(SubPath);
          FItems.Add(Item);
        end;
      end;
      nsEnumerateLeaves:
      begin
        if Entry.SubItems.GetCount = 0 then
        begin
          Item:= TXRTLNameSpaceEnumeratorItem.Create;
          Item.Path:= Copy(SubPath);
          FItems.Add(Item);
        end;
      end;
      nsEnumerateRecursive:
      begin
        Item:= TXRTLNameSpaceEnumeratorItem.Create;
        Item.Path:= Copy(SubPath);
        FItems.Add(Item);
        if Entry.SubItems.GetCount > 0 then
        begin
          EnumItems(Entry, SubPath, EnumType, NameFilter);
        end;
      end;
      nsEnumerateBranchesAndLeaves:
      begin
        Item:= TXRTLNameSpaceEnumeratorItem.Create;
        Item.Path:= Copy(SubPath);
        FItems.Add(Item);
      end;
    else
      raise EXRTLException.Create('Invalid EnumType');
    end;
  end;
end;

function TXRTLNameSpaceEnumerator.GetCount: LongInt;
begin
  Result:= FItems.Count;
end;

function TXRTLNameSpaceEnumerator.GetItem(const ItemIndex: Integer): Pointer;
begin
  Result:= @(FItems[ItemIndex] as TXRTLNameSpaceEnumeratorItem).Path;
end;

{ TXRTLNameSpace }

constructor TXRTLNameSpace.Create(const AValueContainerAdapter: TXRTLContainerAdapter);
begin
  inherited Create;
  FValueContainerAdapter:= AValueContainerAdapter;
  FValueContainerAdapter.Container:= Self;
  FRoot:= TXRTLNameSpaceEntry.Create(Self);
end;

destructor TXRTLNameSpace.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FValueContainerAdapter);
  inherited;
end;

function TXRTLNameSpace.Put(const Path: TXRTLNameSpacePath; Data: Pointer = nil): Pointer;
var
  Entry: TXRTLNameSpaceEntry;
begin
  CheckPath(Path);
  Result:= nil;
  Entry:= GetNameSpaceEntry(Path, True);
  if Assigned(Entry) then
  begin
    Result:= ValueContainerAdapter.GetData(Entry.Data);
    ValueContainerAdapter.SetData(Entry.Data, Data)
  end;
end;

function TXRTLNameSpace.Get(const Path: TXRTLNameSpacePath): Pointer;
var
  Entry: TXRTLNameSpaceEntry;
begin
  CheckPath(Path);
  Result:= nil;
  Entry:= GetNameSpaceEntry(Path, False);
  if Assigned(Entry) then
  begin
    Result:= ValueContainerAdapter.GetData(Entry.Data);
  end;
end;

procedure TXRTLNameSpace.Remove(const Path: TXRTLNameSpacePath);
var
  Entry: TXRTLNameSpaceEntry;
begin
  CheckPath(Path);
  Entry:= GetNameSpaceEntry(Path, False);
  if Assigned(Entry) and Assigned(Entry.Parent) then
  begin
    Entry.Parent.SubItems.Remove(@Path[XRTLNameSpacePathGetLength(Path) - 1]);
  end;
end;

function TXRTLNameSpace.Contains(const Path: TXRTLNameSpacePath): Boolean;
begin
  CheckPath(Path);
  Result:= Assigned(GetNameSpaceEntry(Path, False));
end;

function TXRTLNameSpace.GetNameSpaceEntry(const Path: TXRTLNameSpacePath; AutoCreate: Boolean = False): TXRTLNameSpaceEntry;
var
  LEntry: TXRTLNameSpaceEntry;
  I: Integer;
begin
  Result:= FRoot;
  for I:= 0 to XRTLNameSpacePathGetLength(Path) - 1 do
  begin
    LEntry:= Result.SubItems.Get(@Path[I]);
    if not Assigned(LEntry) then
    begin
      if AutoCreate then
      begin
        LEntry:= TXRTLNameSpaceEntry.Create(Self, Result);
        Result.SubItems.Put(@Path[I], LEntry);
      end
      else
      begin
        Result:= nil;
        Exit;
      end;
    end;
    Result:= LEntry;
  end;
end;

procedure TXRTLNameSpace.CheckPath(const Path: TXRTLNameSpacePath);
begin
  if XRTLNameSpacePathGetLength(Path) = 0 then
    raise EXRTLInvalidNameSpacePath.Create('Invalid namespace path');
end;

function TXRTLNameSpace.CreateEnumerator(const RootPath: TXRTLNameSpacePath;
                                         EnumType: TXRTLNameSpaceEnumeratorType;
                                         const NameFilter: WideString = '*'): TXRTLNameSpaceEnumerator;
begin
  Result:= TXRTLNameSpaceEnumerator.Create(GetNameSpaceEntry(RootPath), EnumType, NameFilter);
end;

procedure TXRTLNameSpace.Clear;
begin
  FRoot.SubItems.Clear;
end;

function TXRTLNameSpace.GetCount: Integer;
begin
  Result:= 0;
end;

function TXRTLNameSpace.IsLeaf(const Path: TXRTLNameSpacePath): Boolean;
var
  LEntry: TXRTLNameSpaceEntry;
begin
  CheckPath(Path);
  Result:= False;
  LEntry:= GetNameSpaceEntry(Path, False);
  if Assigned(LEntry) then
  begin
    Result:= LEntry.FSubItems.GetCount = 0;
  end;
end;

function TXRTLNameSpace.IsBranch(const Path: TXRTLNameSpacePath): Boolean;
var
  LEntry: TXRTLNameSpaceEntry;
begin
  CheckPath(Path);
  Result:= False;
  LEntry:= GetNameSpaceEntry(Path, False);
  if Assigned(LEntry) then
  begin
    Result:= LEntry.FSubItems.GetCount > 0;
  end;
end;

end.

unit xrtl_util_List;

interface

uses
  SysUtils, Classes, Contnrs,
  xrtl_base_Compare, xrtl_base_Exception,
  xrtl_util_Container;

type
  TXRTLList = class(TXRTLSortedContainer)
  private
    FContainerAdapter: TXRTLContainerAdapter;
    FItems: TObjectList;
    function   FindItemCompare(Index: Integer; const AItem: Pointer): TXRTLValueRelationship;
  public
    constructor Create(const AContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
    property   ContainerAdapter: TXRTLContainerAdapter read FContainerAdapter;
    procedure  Clear; override;
    function   GetCount: Integer; override;
    procedure  Sort; override;
    function   GetItem(Index: Integer): Pointer; virtual;
    procedure  Delete(Index: Integer); virtual;
    function   Extract(const Data: Pointer): Pointer; virtual;
    function   IndexOf(const Data: Pointer): Integer; virtual;
    function   Remove(const Data: Pointer): Integer; virtual;
    function   Add(const Data: Pointer): Integer; virtual;
    procedure  Insert(Index: Integer; const Data: Pointer); virtual;
  end;

implementation

{ TXRTLList }

constructor TXRTLList.Create(const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited Create;
  FSorted:= False;
  FContainerAdapter:= AContainerAdapter;
  FContainerAdapter.Container:= Self;
  FItems:= TObjectList.Create(True);
end;

destructor TXRTLList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FContainerAdapter);
  inherited;
end;

procedure TXRTLList.Clear;
begin
  FItems.Clear;
end;

function TXRTLList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  ContainerItem1, ContainerItem2: TXRTLContainerItem;
  ContainerAdapter: TXRTLContainerAdapter;
begin
  ContainerItem1:= TXRTLContainerItem(Item1);
  ContainerItem2:= TXRTLContainerItem(Item2);
  if ContainerItem1.ContainerAdapter.ClassType <> ContainerItem2.ContainerAdapter.ClassType then
    raise EXRTLInvalidContainerCompare.Create('Can''t compare items created with different key adapter types');
  ContainerAdapter:= ContainerItem1.ContainerAdapter;
  Result:= ContainerAdapter.Compare(ContainerItem1, ContainerItem2);
end;

procedure TXRTLList.Sort;
begin
  FItems.Sort(CompareItems);
end;

function TXRTLList.GetItem(Index: Integer): Pointer;
begin
  Result:= ContainerAdapter.GetData(FItems[Index] as TXRTLContainerItem);
end;

procedure TXRTLList.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

function TXRTLList.FindItemCompare(Index: Integer; const AItem: Pointer): TXRTLValueRelationship;
var
  Item1: Pointer;
begin
  Item1:= GetItem(Index);
  Result:= ContainerAdapter.Compare(Item1, AItem);
end;

function TXRTLList.Remove(const Data: Pointer): Integer;
var
  RemoveIndex: Integer;
  ContainerItem: TXRTLContainerItem;
begin
  ContainerAdapter.CheckData(Data);
  Result:= -1;
  if FindItem(Data, RemoveIndex, FindItemCompare) then
  begin
    Result:= RemoveIndex;
    ContainerItem:= FItems[RemoveIndex] as TXRTLContainerItem;
    FItems.Remove(ContainerItem);
  end;
end;

function TXRTLList.Extract(const Data: Pointer): Pointer;
var
  RemoveIndex: Integer;
  ContainerItem: TXRTLContainerItem;
begin
  ContainerAdapter.CheckData(Data);
  Result:= nil;
  if FindItem(Data, RemoveIndex, FindItemCompare) then
  begin
    ContainerItem:= FItems[RemoveIndex] as TXRTLContainerItem;
    Result:= ContainerAdapter.ExtractData(ContainerItem);
    FItems.Remove(ContainerItem);
  end;
end;

function TXRTLList.IndexOf(const Data: Pointer): Integer;
var
  Index: Integer;
begin
  ContainerAdapter.CheckData(Data);
  Result:= -1;
  if FindItem(Data, Index, FindItemCompare) then
    Result:= Index;
end;

function TXRTLList.Add(const Data: Pointer): Integer;
var
  InsertIndex: Integer;
  ContainerItem: TXRTLContainerItem;
begin
  ContainerAdapter.CheckData(Data);
  ContainerItem:= nil;
  Result:= -1;
  try
    if GetSorted then
    begin
      if FindItem(Data, InsertIndex, FindItemCompare) then
      begin
        case GetDuplicates of
          dupAllow:
          begin
          end;
          dupIgnore:
          begin
            Exit;
          end;
          dupError:
          begin
            XRTLInvalidOperation(ClassName, 'Add', 'Duplicates not allowed');
          end;
        end;
      end;
    end
    else
      InsertIndex:= GetCount;
    ContainerItem:= ContainerAdapter.CreateContainerItem;
    ContainerAdapter.SetData(ContainerItem, Data);
    FItems.Insert(InsertIndex, ContainerItem);
    Result:= InsertIndex;
  except
    FreeAndNil(ContainerItem);
    raise;
  end;
end;

procedure TXRTLList.Insert(Index: Integer; const Data: Pointer);
var
  ContainerItem: TXRTLContainerItem;
begin
  if GetSorted then
    XRTLInvalidOperation(ClassName, 'Insert', 'inserts are not allowed on sorted list');
  ContainerAdapter.CheckData(Data);
  ContainerItem:= nil;
  try
    ContainerItem:= ContainerAdapter.CreateContainerItem;
    ContainerAdapter.SetData(ContainerItem, Data);
    FItems.Insert(Index, ContainerItem);
  except
    FreeAndNil(ContainerItem);
    raise;
  end;
end;

end.

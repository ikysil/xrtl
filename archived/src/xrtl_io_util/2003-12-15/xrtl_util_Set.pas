unit xrtl_util_Set;

interface

uses
  SysUtils, 
  xrtl_util_Container, xrtl_util_List;

type
  TXRTLSet = class(TXRTLContainer)
  private
    FContainerAdapter: TXRTLContainerAdapter;
    FItems: TXRTLList;
  public
    constructor Create(const AContainerAdapter: TXRTLContainerAdapter);
    destructor Destroy; override;
    property   ContainerAdapter: TXRTLContainerAdapter read FContainerAdapter;
    procedure  Clear; override;
    function   GetCount: Integer; override;
    function   GetItem(Index: Integer): Pointer; virtual;
    procedure  Delete(Index: Integer); virtual;
    function   Extract(const Data: Pointer): Pointer; virtual;
    function   IndexOf(const Data: Pointer): Integer; virtual;
    function   Remove(const Data: Pointer): Integer; virtual;
    function   Add(const Data: Pointer): Integer; virtual;
  end;

implementation

{ TXRTLSet }

constructor TXRTLSet.Create(const AContainerAdapter: TXRTLContainerAdapter);
begin
  inherited Create;
  FContainerAdapter:= AContainerAdapter;
  FItems:= TXRTLList.Create(AContainerAdapter);
  FItems.SetSorted(True);
  FItems.SetDuplicates(dupIgnore);
end;

destructor TXRTLSet.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TXRTLSet.Clear;
begin
  FItems.Clear;
end;

function TXRTLSet.GetCount: Integer;
begin
  Result:= FItems.GetCount;
end;

function TXRTLSet.GetItem(Index: Integer): Pointer;
begin
  Result:= FItems.GetItem(Index);
end;

procedure TXRTLSet.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

function TXRTLSet.Extract(const Data: Pointer): Pointer;
begin
  Result:= FItems.Extract(Data);
end;

function TXRTLSet.IndexOf(const Data: Pointer): Integer;
begin
  Result:= FItems.IndexOf(Data);
end;

function TXRTLSet.Remove(const Data: Pointer): Integer;
begin
  Result:= FItems.Remove(Data);
end;

function TXRTLSet.Add(const Data: Pointer): Integer;
begin
  Result:= FItems.Add(Data);
end;

end.

unit xrtl_util_NamedResourceAccessSynchronizer;

interface

uses
  SysUtils, SyncObjs,
  xrtl_util_Map;

type
  TXRTLNamedResourceAccessSynchronizer = class
  private
    FMapLock: TSynchroObject;
    FOwnsMap: Boolean;
    FMap: TXRTLMap;
  public
    constructor Create(const AMap: TXRTLMap = nil; AOwnsMap: Boolean = True);
    destructor Destroy; override;
    property   OwnsMap: Boolean read FOwnsMap write FOwnsMap;
    property   Map: TXRTLMap read FMap;
  end;

implementation

{ TXRTLNamedResourceAccessSynchronizer }

constructor TXRTLNamedResourceAccessSynchronizer.Create(const AMap: TXRTLMap = nil;
                                                        AOwnsMap: Boolean = True);
begin
  inherited Create;
  FMapLock:= TCriticalSection.Create;
  FMap:= AMap;
  if Assigned(FMap) then
    FOwnsMap:= AOwnsMap
  else
  begin
    FMap:= TXRTLMap.Create(nil, nil);
    FOwnsMap:= True;
  end;
  FMap.Clear;
end;

destructor TXRTLNamedResourceAccessSynchronizer.Destroy;
begin
  if FOwnsMap then
    FreeAndNil(FMap);
  FreeAndNil(FMapLock);
  inherited;
end;

end.

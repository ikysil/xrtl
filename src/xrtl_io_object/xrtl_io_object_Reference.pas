unit xrtl_io_object_Reference;

{$INCLUDE xrtl.inc}

interface

uses
  Windows,
  SysUtils;

type
  TXRTLInstanceReference = class
  private
    FAllowShared: Boolean;
    FIsNil: Boolean;
    FReferenceId: string;
  protected
    procedure  InitReference(const Obj: TObject);
  public
    constructor Create(const Obj: TObject; const _AllowShared: Boolean);
    destructor Destroy; override;
    property   AllowShared: Boolean read FAllowShared;
    property   IsNil: Boolean read FIsNil;
    property   ReferenceId: string read FReferenceId;
  end;

implementation

var
  FReferenceCount: Integer = 0;

{ TXRTLInstanceReference }

constructor TXRTLInstanceReference.Create(const Obj: TObject; const _AllowShared: Boolean);
begin
  inherited Create;
  FAllowShared:= _AllowShared;
  InitReference(Obj);
end;

destructor TXRTLInstanceReference.Destroy;
begin
  inherited;
end;

procedure TXRTLInstanceReference.InitReference(const Obj: TObject);
begin
  FIsNil:= not Assigned(Obj);
  FReferenceId:= '';
  if not FIsNil then
  begin
    FReferenceId:= Format('obj_%.8x_%.8x_%.8x', [Cardinal(Obj), GetTickCount,
                                                 InterlockedExchangeAdd(FReferenceCount, 1)]);
  end;
end;

end.

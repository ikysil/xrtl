unit xrtl_io_object_Coder;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_Exception, xrtl_io_object_CoderResolver;

type
  EXRTLObjectCoderException = class(EXRTLIOException);

  TXRTLObjectDecoder = class
  private
    FResolver: TXRTLObjectCoderResolver;
    FOwnResolver: Boolean;
  protected
    procedure  ReadReference(Reference: TXRTLObjectCoderReference); virtual; abstract;
    procedure  ReadClassInfo(ClassInfo: TXRTLObjectCoderClassInfo); virtual; abstract;
    procedure  ReadInstance(Instance: TObject; ClassInfo: TXRTLObjectCoderClassInfo); virtual; abstract;
  public
    constructor Create(const AResolver: TXRTLObjectCoderResolver;
                       AOwnResolver: Boolean = True);
    destructor Destroy; override;
    property   Resolver: TXRTLObjectCoderResolver read FResolver;
    property   OwnResolver: Boolean read FOwnResolver write FOwnResolver;
    function   ReadObject(Instance: TObject = nil): TObject;
  end;

  TXRTLObjectEncoder = class
  private
    FResolver: TXRTLObjectCoderResolver;
    FOwnResolver: Boolean;
  protected
    function   WriteInstanceReference(Instance: TObject): Boolean;
    procedure  WriteReference(Reference: TXRTLObjectCoderReference); virtual; abstract;
    procedure  WriteClassInfo(ClassInfo: TXRTLObjectCoderClassInfo); virtual; abstract;
    procedure  WriteInstance(Instance: TObject; ClassInfo: TXRTLObjectCoderClassInfo); virtual; abstract;
  public
    constructor Create(const AResolver: TXRTLObjectCoderResolver;
                       AOwnResolver: Boolean = True);
    destructor Destroy; override;
    property   Resolver: TXRTLObjectCoderResolver read FResolver;
    property   OwnResolver: Boolean read FOwnResolver write FOwnResolver;
    procedure  WriteObject(Instance: TObject);
  end;

implementation

{ TXRTLObjectDecoder }

constructor TXRTLObjectDecoder.Create(const AResolver: TXRTLObjectCoderResolver;
  AOwnResolver: Boolean = True);
begin
  inherited Create;
  FResolver:= AResolver;
  FOwnResolver:= AOwnResolver;
end;

destructor TXRTLObjectDecoder.Destroy;
begin
  FResolver.Reset;
  if FOwnResolver then
    FreeAndNil(FResolver);
  inherited;
end;

function TXRTLObjectDecoder.ReadObject(Instance: TObject = nil): TObject;
var
  Ref: TXRTLObjectCoderReference;
  ClassInfo: TXRTLObjectCoderClassInfo;
begin
  Result:= nil;
  Ref:= nil;
  ClassInfo:= nil;
  try
    Ref:= TXRTLObjectCoderReference.Create;
    ReadReference(Ref);
    if (Ref.Flags and osrfNil) = osrfNil then
      Exit;
    ClassInfo:= TXRTLObjectCoderClassInfo.Create;
    ReadClassInfo(ClassInfo);
    if not Assigned(Instance) then
      Instance:= Resolver.ResolveReference(Ref);
    if not Assigned(Instance) then
    begin
      Instance:= Resolver.GetInstance(ClassInfo);
      Resolver.RegisterReference(Ref, Instance);
      Ref:= nil;
    end;
    ReadInstance(Instance, ClassInfo);
    Result:= Instance;
  finally
    FreeAndNil(ClassInfo);
    FreeAndNil(Ref);
  end;
end;

{ TXRTLObjectEncoder }

constructor TXRTLObjectEncoder.Create(const AResolver: TXRTLObjectCoderResolver;
  AOwnResolver: Boolean = True);
begin
  inherited Create;
  FResolver:= AResolver;
  FOwnResolver:= AOwnResolver;
end;

destructor TXRTLObjectEncoder.Destroy;
begin
  FResolver.Reset;
  if FOwnResolver then
    FreeAndNil(FResolver);
  inherited;
end;

function TXRTLObjectEncoder.WriteInstanceReference(Instance: TObject): Boolean;
var
  Ref: TXRTLObjectCoderReference;
begin
  Result:= True;
  Ref:= nil;
  try
    if not Assigned(Instance) then
    begin
      Ref:= TXRTLNilObjectCoderReference.Create();
      WriteReference(Ref);
      Exit;
    end;
  finally
    FreeAndNil(Ref);
  end;
  Ref:= Resolver.ResolveInstance(Instance);
  if Assigned(Ref) then
  begin
    WriteReference(Ref);
    Exit;
  end;
  Ref:= TXRTLObjectCoderReference.Create;
  Resolver.RegisterReference(Ref, Instance);
  WriteReference(Ref);
  Result:= False;
end;

procedure TXRTLObjectEncoder.WriteObject(Instance: TObject);
var
  ClassInfo: TXRTLObjectCoderClassInfo;
begin
  if WriteInstanceReference(Instance) then
    Exit;
  ClassInfo:= Resolver.GetClassInfo(Instance);
  WriteClassInfo(ClassInfo);
  WriteInstance(Instance, ClassInfo);
end;

end.

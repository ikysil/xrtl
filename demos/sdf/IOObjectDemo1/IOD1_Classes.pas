unit IOD1_Classes;

interface

uses
  Windows,
  SysUtils,
  xrtl_util_Value,
  xrtl_reflect_ClassDescriptor, xrtl_reflect_Introspector,
  xrtl_sdf_Serializer;

type
  TBase = class
  private
    FName: string;
  public
    constructor Create;
    property   Name: string read FName;
  end;

  TBaseClassDescriptor = class(TXRTLClassDescriptor)
  protected
    procedure  DoDefineProperties(const Properties: IXRTLPropertyList); override;
    procedure  DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    procedure  DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    function   DoCreateInstance: TObject; override;
  public
    constructor Create;
  end;

  TBaseStreamer = class(TXRTLObjectStreamer)
    procedure  WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject); override;
    procedure  ReadNoData(const Obj: TObject); override;
    procedure  ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject); override;
  end;

  TDerived1 = class(TBase)
  private
    FParent: TBase;
  public
    constructor Create(const AParent: TBase);
    property   Parent: TBase read FParent;
  end;

  TDerived1ClassDescriptor = class(TXRTLClassDescriptor)
  protected
    procedure  DoDefineProperties(const Properties: IXRTLPropertyList); override;
    procedure  DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    procedure  DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    function   DoCreateInstance: TObject; override;
  public
    constructor Create;
  end;

  TDerived1Introspector = class(TXRTLIntrospector)
    procedure  DefineProperties(const Clazz: TClass; const Properties: IXRTLPropertyList); override;
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
  end;

  TDerived2 = class(TDerived1)
  public
    constructor Create;
  end;

  TDerived2ClassDescriptor = class(TXRTLClassDescriptor)
  protected
    procedure  DoDefineProperties(const Properties: IXRTLPropertyList); override;
    procedure  DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    procedure  DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList); override;
    function   DoCreateInstance: TObject; override;
  public
    constructor Create;
  end;

implementation

{ TBase }

constructor TBase.Create;
begin
  inherited Create;
  FName:= IntToStr(GetTickCount);
end;

{ TBaseClassDescriptor }

constructor TBaseClassDescriptor.Create;
begin
  inherited Create('iodemo::TBase', TBase);
end;

function TBaseClassDescriptor.DoCreateInstance: TObject;
begin
  Result:= TBase.Create;
end;

procedure TBaseClassDescriptor.DoDefineProperties(const Properties: IXRTLPropertyList);
begin
end;

procedure TBaseClassDescriptor.DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

procedure TBaseClassDescriptor.DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

{ TBaseStreamer }

procedure TBaseStreamer.WriteObjectData(const Writer: IXRTLObjectWriter; const Obj: TObject);
begin
  Writer.WriteUTF8String((Obj as TBase).FName);
end;

procedure TBaseStreamer.ReadNoData(const Obj: TObject);
begin
end;

procedure TBaseStreamer.ReadObjectData(const Reader: IXRTLObjectReader; const Obj: TObject);
begin
  (Obj as TBase).FName:= Reader.ReadUTF8String;
end;

{ TDerived1 }

constructor TDerived1.Create(const AParent: TBase);
begin
  inherited Create;
  FParent:= AParent;
end;

{ TDerived1ClassDescriptor }

constructor TDerived1ClassDescriptor.Create;
begin
  inherited Create('iodemo::TDerived1', TDerived1, nil, TDerived1Introspector.Create);
end;

function TDerived1ClassDescriptor.DoCreateInstance: TObject;
begin
  Result:= TDerived1.Create(nil);
end;

procedure TDerived1ClassDescriptor.DoDefineProperties(const Properties: IXRTLPropertyList);
begin
end;

procedure TDerived1ClassDescriptor.DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

procedure TDerived1ClassDescriptor.DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

{ TDerived1Introspector }

procedure TDerived1Introspector.DefineProperties(const Clazz: TClass;
  const Properties: IXRTLPropertyList);
begin
  Properties.Add(TXRTLProperty.Create('Parent', XRTLValue(TObject(nil))));
end;

procedure TDerived1Introspector.GetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
var
  LObj: TDerived1;
begin
  LObj:= Obj as TDerived1;
  XRTLSetValue(Properties.GetByName('Parent').Value, LObj.FParent);
end;

procedure TDerived1Introspector.SetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
var
  LObj: TDerived1;
begin
  LObj:= Obj as TDerived1;
  LObj.FParent:= XRTLGetAsObject(Properties.GetByName('Parent').Value) as TBase;
end;

{ TDerived2 }

constructor TDerived2.Create;
begin
  inherited Create(nil);
end;

{ TDerived2ClassDescriptor }

constructor TDerived2ClassDescriptor.Create;
begin
  inherited Create('iodemo::TDerived2', TDerived2);
end;

function TDerived2ClassDescriptor.DoCreateInstance: TObject;
begin
  Result:= TDerived2.Create;
end;

procedure TDerived2ClassDescriptor.DoDefineProperties(const Properties: IXRTLPropertyList);
begin
end;

procedure TDerived2ClassDescriptor.DoGetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

procedure TDerived2ClassDescriptor.DoSetValues(const Obj: TObject; const Properties: IXRTLPropertyList);
begin
end;

procedure RegisterClasses;
var
  Descriptor: IXRTLClassDescriptor;
begin
  Descriptor:= TBaseClassDescriptor.Create;
  XRTLRegisterClassDescriptor(Descriptor);
  XRTLStreamerRegistry.Register(TBase, TBaseStreamer.Create);
  Descriptor:= TDerived1ClassDescriptor.Create;
  XRTLRegisterClassDescriptor(Descriptor);
  Descriptor:= TDerived2ClassDescriptor.Create;
  XRTLRegisterClassDescriptor(Descriptor);
end;

initialization
begin
  RegisterClasses;
end;

end.

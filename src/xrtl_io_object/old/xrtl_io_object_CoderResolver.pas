unit xrtl_io_object_CoderResolver;

{$INCLUDE xrtl.inc}

interface

const
  osrfNil = $00000001;

type
  TXRTLObjectCoderReference = class
  public
    Flags: Cardinal;
    ReferenceID: string;
  end;

  TXRTLNilObjectCoderReference = class(TXRTLObjectCoderReference)
  public
    constructor Create;
  end;

  TXRTLObjectCoderClassInfo = class
  public
    ClassID: string;
    ClassVersion: Int64;
  end;

  TXRTLObjectCoderResolver = class
  public
    function   ResolveReference(Reference: TXRTLObjectCoderReference): TObject; virtual; abstract;
    function   ResolveInstance(Instance: TObject): TXRTLObjectCoderReference; virtual; abstract;
    function   GetClassInfo(Instance: TObject): TXRTLObjectCoderClassInfo; virtual; abstract;
    function   GetInstance(ClassInfo: TXRTLObjectCoderClassInfo): TObject; virtual; abstract;
//  Removes all registered references.
    procedure  Reset; virtual; abstract;
    procedure  RegisterReference(Reference: TXRTLObjectCoderReference; Instance: TObject); virtual; abstract;
  end;

implementation

{ TXRTLNilObjectCoderReference }

constructor TXRTLNilObjectCoderReference.Create;
begin
  Flags:= osrfNil;
  ReferenceID:= '';
end;

end.

unit xrtl_reflect_Introspector;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_reflect_ClassDescriptor, xrtl_reflect_PropertyList;

type
  TXRTLIntrospector = class(TInterfacedObject, IXRTLIntrospector)
  public
    constructor Create;
    destructor Destroy; override;
    procedure  DefineProperties(const Descriptor: IXRTLClassDescriptor;
                                const Properties: IXRTLPropertyList); virtual; abstract;
  end;

implementation

{ TXRTLIntrospector }

constructor TXRTLIntrospector.Create;
begin
  inherited Create;
end;

destructor TXRTLIntrospector.Destroy;
begin
  inherited;
end;

end.

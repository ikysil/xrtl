unit xrtl_reflect_Introspector;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_reflect_ClassDescriptor, xrtl_reflect_PropertyList;

type
  TXRTLIntrospector = class(TInterfacedObject, IXRTLIntrospector)
  public
    procedure  DefineProperties(const Descriptor: IXRTLClassDescriptor;
                                const Properties: IXRTLPropertyList); virtual; abstract;
    procedure  GetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual; abstract;
    procedure  SetValues(const Obj: TObject; const Properties: IXRTLPropertyList); virtual; abstract;
  end;

implementation

{ TXRTLIntrospector }

end.

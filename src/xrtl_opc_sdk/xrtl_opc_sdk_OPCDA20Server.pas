unit xrtl_opc_sdk_OPCDA20Server;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, COMObj, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_opc_DA, 
  xrtl_opc_sdk_OPCServer, 
  xrtl_opc_sdk_OPCDAServer, xrtl_opc_sdk_OPCDA20Classes;

type
  TXRTLOPCDA20Server = class(TXRTLOPCDAServer, IOPCItemProperties,
                             IOPCBrowseServerAddressSpace)
  private
    FOPCItemProperties: TXRTLOPCDA20_IOPCItemProperties;
    FOPCBrowseServerAddressSpace: TXRTLOPCDA20_IOPCBrowseServerAddressSpace;
  protected
    function   CreateGroup: TXRTLOPCDAGroup; override;
  public
    procedure  Initialize; override;
    destructor Destroy; override;
    property   OPCItemProperties: TXRTLOPCDA20_IOPCItemProperties
               read FOPCItemProperties implements IOPCItemProperties;
    property   OPCBrowseServerAddressSpace: TXRTLOPCDA20_IOPCBrowseServerAddressSpace
               read FOPCBrowseServerAddressSpace implements IOPCBrowseServerAddressSpace; 
  end;

implementation

uses
  xrtl_opc_sdk_OPCDA20Group;

{ TXRTLOPCDA20Server }

procedure TXRTLOPCDA20Server.Initialize;
begin
  inherited;
  FOPCItemProperties:= TXRTLOPCDA20_IOPCItemProperties.Create(Self);
  FOPCBrowseServerAddressSpace:= TXRTLOPCDA20_IOPCBrowseServerAddressSpace.Create(Self);
end;

destructor TXRTLOPCDA20Server.Destroy;
begin
  FreeAndNil(FOPCBrowseServerAddressSpace);
  FreeAndNil(FOPCItemProperties);
  inherited;
end;

function TXRTLOPCDA20Server.CreateGroup: TXRTLOPCDAGroup;
begin
  Result:= TXRTLOPCDA20Group.Create;
end;

end.

unit xrtl_opc_sdk_OPCDA30Server;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, COMObj, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_opc_DA, 
  xrtl_opc_sdk_OPCServer, 
  xrtl_opc_sdk_OPCDAServer, xrtl_opc_sdk_OPCDA30Classes;

type
  TXRTLOPCDA30Server = class(TXRTLOPCDAServer,
                             IOPCItemIO)
  private
    FOPCItemIO: TXRTLOPCDA30_IOPCItemIO;
  public
    procedure  Initialize; override;
    destructor Destroy; override;
    property   OPCItemIO: TXRTLOPCDA30_IOPCItemIO read FOPCItemIO implements IOPCItemIO;
  end;

implementation

{ TXRTLOPCDA30Server }

procedure TXRTLOPCDA30Server.Initialize;
begin
  inherited;
  FOPCItemIO:= TXRTLOPCDA30_IOPCItemIO.Create(Self);
end;

destructor TXRTLOPCDA30Server.Destroy;
begin
  FreeAndNil(FOPCItemIO);
  inherited;
end;

end.

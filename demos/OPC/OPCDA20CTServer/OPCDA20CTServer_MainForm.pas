unit OPCDA20CTServer_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  xrtl_opc_sdk_OPCServerList, xrtl_opc_sdk_OPCDADataSource;

{$R *.dfm}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  XRTLOPCServerList.SendShutdownNotify('Shutdown from UI');
end;

end.

unit OPCDA20Server_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    ASyncTimer: TTimer;
    RefreshCacheTimer: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RefreshCacheTimer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  xrtl_opc_sdk_OPCServerList, xrtl_opc_sdk_OPCDADataSource;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  XRTLOPCDAUpdateCache;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  XRTLOPCServerList.SendShutdownNotify('Shutdown from UI');
end;

procedure TForm1.RefreshCacheTimer1Timer(Sender: TObject);
begin
  XRTLOPCDAUpdateCache;
end;

end.

program OPCDA20CTServer;

uses
  Forms,
  OPCDA20CTServer_MainForm in 'OPCDA20CTServer_MainForm.pas' {MainForm},
  OPCDA20CTServer_Device in 'OPCDA20CTServer_Device.pas',
  OPCDA20CTServer_MainObject in 'OPCDA20CTServer_MainObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'OPC DA 2.0 Compliance Test Server';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

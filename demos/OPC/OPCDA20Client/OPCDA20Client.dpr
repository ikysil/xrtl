program OPCDA20Client;

uses
  Forms,
  OPCDA20Client_MainForm in 'OPCDA20Client_MainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

program OPCDA20Server;

uses
  Forms,
  OPCDA20Server_MainObject in 'OPCDA20Server_MainObject.pas',
  OPCDA20Server_MainForm in 'OPCDA20Server_MainForm.pas' {Form1},
  OPCDA20Server_Device in 'OPCDA20Server_Device.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

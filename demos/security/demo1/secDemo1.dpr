program secDemo1;

uses
  Forms,
  secDemo1_MainFrm in 'secDemo1_MainFrm.pas' {secDemo_MainForm},
  secDemo1_Classes in 'secDemo1_Classes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TsecDemo_MainForm, secDemo_MainForm);
  Application.Run;
end.

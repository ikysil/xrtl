program secDemo2;

uses
  Forms,
  secDemo2_MainFrm in 'secDemo2_MainFrm.pas' {secDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TsecDemo_MainForm, secDemo_MainForm);
  Application.Run;
end.

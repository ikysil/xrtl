program zlibDemo2;

uses
  Forms,
  zlibDemo2_MainFrm in 'zlibDemo2_MainFrm.pas' {zlibDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TzlibDemo_MainForm, zlibDemo_MainForm);
  Application.Run;
end.

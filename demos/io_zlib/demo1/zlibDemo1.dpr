program zlibDemo1;

uses
  Forms,
  zlibDemo1_MainFrm in 'zlibDemo1_MainFrm.pas' {zlibDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TzlibDemo_MainForm, zlibDemo_MainForm);
  Application.Run;
end.

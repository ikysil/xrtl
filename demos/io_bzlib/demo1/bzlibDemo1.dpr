program bzlibDemo1;

uses
  Forms,
  bzlibDemo1_MainFrm in 'bzlibDemo1_MainFrm.pas' {bzlibDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TbzlibDemo_MainForm, bzlibDemo_MainForm);
  Application.Run;
end.

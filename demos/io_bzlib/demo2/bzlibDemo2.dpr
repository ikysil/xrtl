program bzlibDemo2;

uses
  Forms,
  bzlibDemo2_MainFrm in 'bzlibDemo2_MainFrm.pas' {bzlibDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TbzlibDemo_MainForm, bzlibDemo_MainForm);
  Application.Run;
end.

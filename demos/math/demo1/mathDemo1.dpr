program mathDemo1;

uses
  Forms,
  mathDemo1_MainFrm in 'mathDemo1_MainFrm.pas' {MainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

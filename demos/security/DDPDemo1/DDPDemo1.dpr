program DDPDemo1;

uses
  Forms,
  DDPD_MainForm in 'DDPD_MainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

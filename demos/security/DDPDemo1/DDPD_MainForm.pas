unit DDPD_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts;
  
type
  TForm1 = class(TForm)
    StartBtn: TButton;
    StopBtn: TButton;
    VLbl: TLabel;
    SLbl: TLabel;
    ApplicationEvents1: TApplicationEvents;
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
  private
    { Private declarations }
  public
    Canceled: Boolean;
  end;

var
  Form1: TForm1;

implementation

uses
  xrtl_util_CPUUtils,
  xrtl_crypt_DDP;

{$R *.dfm}

procedure TForm1.StartBtnClick(Sender: TObject);
const
  ACheck = $F6D4B290;
var
  A, B, V, Conversions: Cardinal;
  CW: PCardinalWords;
  StartDT, DT: TDateTime;
begin
  StartBtn.Enabled:= False;
  StopBtn.Enabled:= True;
  Canceled:= False;
  V:= $FFFFFFFF;
  CW:= @V;
  Conversions:= 0;
  StartDT:= Now;
  repeat
    B:= XRTLDDP3296Direct(ACheck, CW.Words[0], CW.Words[1],
                                  CW.Words[0], CW.Words[1],
                                  CW.Words[0], CW.Words[1]);
    A:= XRTLDDP3296Reverse(B,     CW.Words[0], CW.Words[1],
                                  CW.Words[0], CW.Words[1],
                                  CW.Words[0], CW.Words[1]);
    if A <> ACheck then
      raise Exception.CreateFmt('Invalid conversion'#13#10'ACheck=%.8x, A=%.8x, B=%.8x, V=%.8x',
                                [ACheck, A, B, V]);
    if (V and $3FFF) = 0 then
    begin
      DT:= Now - StartDT;
      if DT > 0 then
        SLbl.Caption:= Format('%f conv/sec', [Conversions / (DT * SecsPerDay)]);
      VLbl.Caption:= IntToHex(V, 8);
      Application.ProcessMessages;
    end;
    Inc(Conversions);
    Dec(V, $11);
  until (V = 0) or Canceled;
end;

procedure TForm1.StopBtnClick(Sender: TObject);
begin
  StartBtn.Enabled:= True;
  StopBtn.Enabled:= False;
  Canceled:= True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
  Canceled:= True;
end;

procedure TForm1.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
begin
  Application.ShowException(E);
  StopBtnClick(Sender);
end;

end.

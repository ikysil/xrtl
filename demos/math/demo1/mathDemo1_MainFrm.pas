unit mathDemo1_MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  xrtl_math_Integer, ComCtrls, StdCtrls, AppEvnts, ExtCtrls;

type
  TMainFrm = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    Pages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    AddBtn: TButton;
    MulBtn: TButton;
    NegBtn: TButton;
    AbsBtn: TButton;
    DivBtn: TButton;
    SubBtn: TButton;
    RCDLBtn: TButton;
    SADLBtn: TButton;
    SLDLBtn: TButton;
    RCBLBtn: TButton;
    SABLBtn: TButton;
    SLBLBtn: TButton;
    SLBRBtn: TButton;
    SABRBtn: TButton;
    RCBRBtn: TButton;
    SLDRBtn: TButton;
    SADRBtn: TButton;
    RCDR: TButton;
    SetBitBtn: TButton;
    ResetBitBtn: TButton;
    InvertBtn: TButton;
    ExpBtn: TButton;
    GetMSBitIndexBtn: TButton;
    NormalizeBtn: TButton;
    CompareBtn: TButton;
    SetDataBitsBtn: TButton;
    TabSheet5: TTabSheet;
    ClearBtn: TButton;
    SetValueBtn: TButton;
    TestExpBtn: TButton;
    TestExpModBtn: TButton;
    SQRBtn: TButton;
    SQRTBtn: TButton;
    GCDBtn: TButton;
    Panel1: TPanel;
    Memo: TMemo;
    ValueEdit: TEdit;
    RootBtn: TButton;
    FactBtn: TButton;
    FactModBtn: TButton;
    ShowBtn: TButton;
    RRCB: TCheckBox;
    procedure SetValueBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure SetBitBtnClick(Sender: TObject);
    procedure ResetBitBtnClick(Sender: TObject);
    procedure NormalizeBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure SubBtnClick(Sender: TObject);
    procedure InvertBtnClick(Sender: TObject);
    procedure NegBtnClick(Sender: TObject);
    procedure AbsBtnClick(Sender: TObject);
    procedure SetDataBitsBtnClick(Sender: TObject);
    procedure MulBtnClick(Sender: TObject);
    procedure CompareBtnClick(Sender: TObject);
    procedure RCDLBtnClick(Sender: TObject);
    procedure RCDRClick(Sender: TObject);
    procedure SADLBtnClick(Sender: TObject);
    procedure SLDLBtnClick(Sender: TObject);
    procedure SABLBtnClick(Sender: TObject);
    procedure SLBLBtnClick(Sender: TObject);
    procedure RCBLBtnClick(Sender: TObject);
    procedure GetMSBitIndexBtnClick(Sender: TObject);
    procedure SADRBtnClick(Sender: TObject);
    procedure SLDRBtnClick(Sender: TObject);
    procedure RCBRBtnClick(Sender: TObject);
    procedure SABRBtnClick(Sender: TObject);
    procedure SLBRBtnClick(Sender: TObject);
    procedure DivBtnClick(Sender: TObject);
    procedure ExpBtnClick(Sender: TObject);
    procedure TestExpBtnClick(Sender: TObject);
    procedure TestExpModBtnClick(Sender: TObject);
    procedure SQRBtnClick(Sender: TObject);
    procedure SQRTBtnClick(Sender: TObject);
    procedure GCDBtnClick(Sender: TObject);
    procedure RootBtnClick(Sender: TObject);
    procedure FactBtnClick(Sender: TObject);
    procedure FactModBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
  private
    procedure PrintLn(S: string = '');
    procedure ReportValue(Flag: Boolean = False); overload;
    procedure ReportValue(AInteger: TXRTLInteger; Name: string = ''; Flag: Boolean = False); overload;
  public
    AInt: TXRTLInteger;
    DTStart: TDateTime;
    function  GetRadix: Integer;
    procedure GetValue1(var AResult: TXRTLInteger);
    procedure StartTimer;
    procedure StopTimer(S: string);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.DFM}

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  Pages.ActivePageIndex:= 0;
  XRTLZero(AInt);
  ReportValue(True);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(AInt);
end;

procedure TMainFrm.StartTimer;
begin
  DTStart:= Now;
end;

procedure TMainFrm.StopTimer(S: string);
begin
  Memo.Lines.Add(Format('%s: %s', [S, FormatDateTime('hh:nn:ss.zzz', Now - DTStart)]));
end;

procedure TMainFrm.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  PrintLn(E.Message);
  PrintLn;
end;

procedure TMainFrm.ReportValue(Flag: Boolean = False);
begin
  ReportValue(AInt, '', Flag);
end;

procedure TMainFrm.ReportValue(AInteger: TXRTLInteger; Name: string = ''; Flag: Boolean = False);
var
  S: string;
begin
  if Flag or RRCB.Checked then
  begin
    StartTimer;
    S:= XRTLToString(AInteger, GetRadix);
    StopTimer('ToString');
    if Name <> '' then
      Memo.Lines.Add(Name);
    Memo.Lines.Add(Format('digits %d, bits %d, size %d', [Length(S), XRTLDataBits(AInteger), XRTLLength(AInteger)]));
    Memo.Lines.Add(S);
    Memo.Lines.Add('');
  end
  else
  begin
    Memo.Lines.Add(Format('bits %d, size %d', [XRTLDataBits(AInteger), XRTLLength(AInteger)]));
    Memo.Lines.Add('');
  end;
end;

procedure TMainFrm.PrintLn(S: string = '');
begin
  Memo.Lines.Add(S);
end;

function TMainFrm.GetRadix: Integer;
begin
  Result:= 16;
end;

procedure TMainFrm.GetValue1(var AResult: TXRTLInteger);
begin
  StartTimer;
  XRTLFromString(ValueEdit.Text, AResult, GetRadix);
  StopTimer('FromString');
end;

procedure TMainFrm.ClearBtnClick(Sender: TObject);
begin
  PrintLn('Clear');
  Memo.Lines.Clear;
  ReportValue;
end;

procedure TMainFrm.SetValueBtnClick(Sender: TObject);
begin
  PrintLn('Set value');
  GetValue1(AInt);
  ReportValue;
end;

procedure TMainFrm.SetBitBtnClick(Sender: TObject);
begin
  PrintLn('Set bit');
  StartTimer;
  XRTLBitSet(AInt, StrToIntDef(ValueEdit.Text, 0));
  StopTimer('Set bit');
  ReportValue;
end;

procedure TMainFrm.ResetBitBtnClick(Sender: TObject);
begin
  PrintLn('Reset bit');
  StartTimer;
  XRTLBitReset(AInt, StrToIntDef(ValueEdit.Text, 0));
  StopTimer('Reset bit');
  ReportValue;
end;

procedure TMainFrm.NormalizeBtnClick(Sender: TObject);
begin
  PrintLn('SignStrip');
  StartTimer;
  XRTLSignStrip(AInt, AInt);
  StopTimer('SignStrip');
  ReportValue;
end;

procedure TMainFrm.AddBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Add');
  GetValue1(AI);
  StartTimer;
  XRTLAdd(AInt, AI, AInt);
  StopTimer('Add');
  ReportValue;
end;

procedure TMainFrm.SubBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Sub');
  GetValue1(AI);
  StartTimer;
  XRTLSub(AInt, AI, AInt);
  StopTimer('Sub');
  ReportValue;
end;

procedure TMainFrm.MulBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Mul');
  GetValue1(AI);
  StartTimer;
  XRTLMul(AInt, AI, AInt);
  StopTimer('Mul');
  ReportValue;
end;

procedure TMainFrm.DivBtnClick(Sender: TObject);
var
  AI, RInt: TXRTLInteger;
begin
  PrintLn('Div');
  GetValue1(AI);
  StartTimer;
  XRTLDivMod(AInt, AI, AInt, RInt);
  StopTimer('Div');
  ReportValue;
  ReportValue(RInt, 'Remainder');
end;

procedure TMainFrm.InvertBtnClick(Sender: TObject);
begin
  PrintLn('Invert');
  StartTimer;
  XRTLNot(AInt, AInt);
  StopTimer('Invert');
  ReportValue;
end;

procedure TMainFrm.NegBtnClick(Sender: TObject);
begin
  PrintLn('Neg');
  StartTimer;
  XRTLNeg(AInt, AInt);
  StopTimer('Neg');
  ReportValue;
end;

procedure TMainFrm.AbsBtnClick(Sender: TObject);
begin
  PrintLn('Abs');
  StartTimer;
  XRTLAbs(AInt, AInt);
  StopTimer('Abs');
  ReportValue;
end;

procedure TMainFrm.SetDataBitsBtnClick(Sender: TObject);
begin
  PrintLn('SetDataBits');
  StartTimer;
  XRTLZeroExtend(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SetDataBits');
  ReportValue;
end;

procedure TMainFrm.CompareBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Compare');
  GetValue1(AI);
  StartTimer;
  PrintLn(IntToStr(XRTLCompare(AInt, AI)));
  StopTimer('Compare');
  PrintLn;
end;

procedure TMainFrm.RCDLBtnClick(Sender: TObject);
begin
  PrintLn('RCDL');
  StartTimer;
  XRTLRCDL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('RCDL');
  ReportValue;
end;

procedure TMainFrm.SADLBtnClick(Sender: TObject);
begin
  PrintLn('SADL');
  StartTimer;
  XRTLSADL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SADL');
  ReportValue;
end;

procedure TMainFrm.SLDLBtnClick(Sender: TObject);
begin
  PrintLn('SLDL');
  StartTimer;
  XRTLSLDL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SLDL');
  ReportValue;
end;

procedure TMainFrm.RCBLBtnClick(Sender: TObject);
begin
  PrintLn('RCBL');
  StartTimer;
  XRTLRCBL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('RCBL');
  ReportValue;
end;

procedure TMainFrm.SABLBtnClick(Sender: TObject);
begin
  PrintLn('SABL');
  StartTimer;
  XRTLSABL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SABL');
  ReportValue;
end;

procedure TMainFrm.SLBLBtnClick(Sender: TObject);
begin
  PrintLn('SLBL');
  StartTimer;
  XRTLSLBL(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SLBL');
  ReportValue;
end;

procedure TMainFrm.GetMSBitIndexBtnClick(Sender: TObject);
begin
  PrintLn('GetMSBitIndex');
  StartTimer;
  PrintLn(IntToStr(XRTLGetMSBitIndex(AInt)));
  StopTimer('GetMSBitIndex');
  PrintLn;
end;

procedure TMainFrm.RCDRClick(Sender: TObject);
begin
  PrintLn('RCDR');
  StartTimer;
  XRTLRCDR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('RCDR');
  ReportValue;
end;

procedure TMainFrm.SADRBtnClick(Sender: TObject);
begin
  PrintLn('SADR');
  StartTimer;
  XRTLSADR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SADR');
  ReportValue;
end;

procedure TMainFrm.SLDRBtnClick(Sender: TObject);
begin
  PrintLn('SLDR');
  StartTimer;
  XRTLSLDR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SLDR');
  ReportValue;
end;

procedure TMainFrm.RCBRBtnClick(Sender: TObject);
begin
  PrintLn('RCBR');
  StartTimer;
  XRTLRCBR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('RCBR');
  ReportValue;
end;

procedure TMainFrm.SABRBtnClick(Sender: TObject);
begin
  PrintLn('SABR');
  StartTimer;
  XRTLSABR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SABR');
  ReportValue;
end;

procedure TMainFrm.SLBRBtnClick(Sender: TObject);
begin
  PrintLn('SLBR');
  StartTimer;
  XRTLSLBR(AInt, StrToInt64Def(ValueEdit.Text, 0), AInt);
  StopTimer('SLBR');
  ReportValue;
end;

procedure TMainFrm.ExpBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Exp');
  GetValue1(AI);
  StartTimer;
  XRTLExp(AInt, AI, AInt);
  StopTimer('Exp');
  ReportValue;
end;

procedure TMainFrm.TestExpBtnClick(Sender: TObject);
var
  AI, AI1, AExp1, AExp2, AOne: TXRTLInteger;
begin
  PrintLn('TestExp');
  GetValue1(AI);
  XRTLExp(AI, AI, AExp1);
  ReportValue(AExp1, 'XRTLExp result');
  XRTLOne(AOne);
  XRTLOne(AExp2);
  XRTLAssign(AI, AI1);
  while XRTLSign(AI) > 0 do
  begin
    XRTLMul(AExp2, AI1, AExp2);
    XRTLSub(AI, AOne, AI);
  end;
  ReportValue(AExp2, 'XRTLMul result');
  PrintLn(Format('Compare %d', [XRTLCompare(AExp1, AExp2)]));
  PrintLn;
end;

procedure TMainFrm.TestExpModBtnClick(Sender: TObject);
var
  AI, AI1, AExp1, AExp2, AOne, AExpMod: TXRTLInteger;
begin
  PrintLn('TestExp');
  GetValue1(AI);
  XRTLAssign(Int64($FFFFFFFFFFFF)+1, AExpMod);
  XRTLExpMod(AI, AI, AExpMod, AExp1);
  ReportValue(AExp1, 'XRTLExpMod result');
  XRTLOne(AOne);
  XRTLOne(AExp2);
  XRTLAssign(AI, AI1);
  while XRTLSign(AI) > 0 do
  begin
    XRTLMul(AExp2, AI1, AExp2);
    XRTLSub(AI, AOne, AI);
  end;
  XRTLDivMod(AExp2, AExpMod, AI, AExp2);
  ReportValue(AExp2, 'XRTLMul result');
  PrintLn(Format('Compare %d', [XRTLCompare(AExp1, AExp2)]));
  PrintLn;
end;

procedure TMainFrm.SQRBtnClick(Sender: TObject);
begin
  PrintLn('Sqr');
  StartTimer;
  XRTLSqr(AInt, AInt);
  StopTimer('Sqr');
  ReportValue;
end;

procedure TMainFrm.SQRTBtnClick(Sender: TObject);
begin
  PrintLn('Sqrt');
  StartTimer;
  XRTLSqrt(AInt, AInt);
  StopTimer('Sqrt');
  ReportValue;
end;

procedure TMainFrm.GCDBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('GCD');
  GetValue1(AI);
  StartTimer;
  XRTLGCD(AInt, AI, AInt);
  StopTimer('Sqrt');
  ReportValue;
end;

procedure TMainFrm.RootBtnClick(Sender: TObject);
var
  AI: TXRTLInteger;
begin
  PrintLn('Root');
  GetValue1(AI);
  StartTimer;
  XRTLRoot(AInt, AI, AInt);
  StopTimer('Root');
  ReportValue;
end;

procedure TMainFrm.FactBtnClick(Sender: TObject);
begin
  PrintLn('Factorial');
  StartTimer;
  XRTLFactorial(AInt, AInt);
  StopTimer('Factorial');
  ReportValue;
end;

procedure TMainFrm.FactModBtnClick(Sender: TObject);
var
  AFact: TXRTLInteger;
begin
  PrintLn('FactMod');
  GetValue1(AFact);
  StartTimer;
  XRTLFactorialMod(AFact, AInt, AInt);
  StopTimer('FactMod');
  ReportValue;
end;

procedure TMainFrm.ShowBtnClick(Sender: TObject);
begin
  PrintLn('ShowValue');
  ReportValue(True);
end;

end.

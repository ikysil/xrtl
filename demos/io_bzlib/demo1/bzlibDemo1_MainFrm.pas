unit bzlibDemo1_MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts;

type
  TbzlibDemo_MainForm = class(TForm)
    TestBtn: TButton;
    LogMemo: TMemo;
    ApplicationEvents1: TApplicationEvents;
    procedure TestBtnClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
  private
    { Private declarations }
  public
    procedure  WriteLn(S: string);
  end;

var
  bzlibDemo_MainForm: TbzlibDemo_MainForm;

implementation

{$R *.dfm}

uses
  xrtl_io_bzlib_bzlibStream, xrtl_io_FileStream;

{ TbzlibDemo_MainForm }

procedure TbzlibDemo_MainForm.WriteLn(S: string);
begin
  LogMemo.Lines.Add(S);
  Application.ProcessMessages;
end;

procedure TbzlibDemo_MainForm.TestBtnClick(Sender: TObject);
const
  DataLength = 200000;
var
  TFOS: TXRTLTempFileOutputStream;
  ZOS: TXRTLbzlibOutputStream;
  FIS: TXRTLFileInputStream;
  ZIS: TXRTLbzlibInputStream;
  I, RData, Data: Integer;
begin
  TFOS:= nil;
  ZOS:= nil;
  FIS:= nil;
  ZIS:= nil;
  try
    WriteLn('Creating TOSbzlibOutputStream');
    TFOS:= TXRTLTempFileOutputStream.Create('', False);
    ZOS:= TXRTLbzlibOutputStream.Create(TFOS, True);
    WriteLn('Writing data');
    Data:= 0;
    for I:= 0 to DataLength - 1 do
    begin
      ZOS.WriteBuffer(I, SizeOf(I));
      Inc(Data);
    end;
    WriteLn('Data written, closing stream');
    ZOS.Close;
    WriteLn('Creating TOSbzlibInputStream');
    ZIS:= TXRTLbzlibInputStream.Create(TXRTLFileInputStream.Create(TFOS.FilePath, True));
    WriteLn('Reading data');
    Data:= 0;
    I:= -1;
    repeat
      Inc(I);
      Data:= ZIS.ReadBuffer(RData, SizeOf(RData));
      if (Data > 0) and (RData <> I) then
        WriteLn(Format('Data = %8.x, RData = %8.x', [I, RData]));
    until Data < 0;
    if I <> DataLength then
      WriteLn(Format('Data = %8.x, DataLength = %8.x', [I, DataLength]));
    WriteLn('Data read, closing stream');
    ZIS.Close;
  finally
    FreeAndNil(ZIS);
    FreeAndNil(ZOS);
  end;
end;

procedure TbzlibDemo_MainForm.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
begin
  WriteLn(Format('Exception %s: %s', [E.ClassName, E.Message]));
end;

end.

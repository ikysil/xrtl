unit zlibDemo2_MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts;

type
  TzlibDemo_MainForm = class(TForm)
    WriteBtn: TButton;
    LogMemo: TMemo;
    ApplicationEvents1: TApplicationEvents;
    ReadBtn: TButton;
    procedure WriteBtnClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure ReadBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure  WriteLn(S: string);
  end;

var
  zlibDemo_MainForm: TzlibDemo_MainForm;

implementation

{$R *.dfm}

uses
  xrtl_io_FileStream,
  xrtl_io_ProcessingStream, xrtl_io_zlib_StreamProcessor;

const
  DataLength = 200000;
  FName = 'c:\temp\zlibTest';

{ TzlibDemo_MainForm }

procedure TzlibDemo_MainForm.WriteLn(S: string);
begin
  LogMemo.Lines.Add(S);
  Application.ProcessMessages;
end;

procedure TzlibDemo_MainForm.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
begin
  WriteLn(Format('Exception %s: %s', [E.ClassName, E.Message]));
end;

procedure TzlibDemo_MainForm.WriteBtnClick(Sender: TObject);
var
  POS2: TXRTLProcessingOutputStream;
  ZSP2: TXRTLzlibStreamProcessor;
  PIS2: TXRTLProcessingInputStream;
  I: Integer;
begin
  POS2:= nil;
  PIS2:= nil;
  try
    WriteLn('Creating TXRTLProcessingOutputStream');
    ZSP2:= TXRTLzlibStreamProcessor.Create;
    POS2:= TXRTLProcessingOutputStream.Create(TXRTLFileOutputStream.Create(FName, False), ZSP2, True, False);
    ZSP2.InitCompress;
    WriteLn('Writing data');
    for I:= 0 to DataLength - 1 do
    begin
      POS2.WriteBuffer(I, SizeOf(I));
    end;
    WriteLn('Data written, closing stream');
    POS2.Close;
  finally
    FreeAndNil(PIS2);
    FreeAndNil(POS2);
  end;
end;

procedure TzlibDemo_MainForm.ReadBtnClick(Sender: TObject);
var
  POS2: TXRTLProcessingOutputStream;
  ZSP2: TXRTLzlibStreamProcessor;
  PIS2: TXRTLProcessingInputStream;
  I, RData, Data: Integer;
begin
  POS2:= nil;
  PIS2:= nil;
  try
    WriteLn('Creating TXRTLProcessingInputStream');
    ZSP2:= TXRTLzlibStreamProcessor.Create;
    PIS2:= TXRTLProcessingInputStream.Create(TXRTLFileInputStream.Create(FName, False), ZSP2, True, False);
    ZSP2.InitDecompress;
    WriteLn('Reading data');
    I:= -1;
    repeat
      Inc(I);
      Data:= PIS2.ReadBuffer(RData, SizeOf(RData));
      if (Data > 0) and (RData <> I) then
        WriteLn(Format('Data = %8.x, RData = %8.x', [I, RData]));
    until Data < 0;
    if I <> DataLength then
      WriteLn(Format('Data = %8.x, DataLength = %8.x', [I, DataLength]));
    WriteLn('Data read, closing stream');
    PIS2.Close;
  finally
    FreeAndNil(PIS2);
    FreeAndNil(POS2);
  end;
end;

end.

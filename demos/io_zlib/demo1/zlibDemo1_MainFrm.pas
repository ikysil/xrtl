unit zlibDemo1_MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts;

type
  TzlibDemo_MainForm = class(TForm)
    TestBtn: TButton;
    LogMemo: TMemo;
    ApplicationEvents1: TApplicationEvents;
    Test2Btn: TButton;
    Test3Btn: TButton;
    procedure TestBtnClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure Test2BtnClick(Sender: TObject);
    procedure Test3BtnClick(Sender: TObject);
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
  xrtl_io_zlib_zlibStream, xrtl_io_FileStream,
  xrtl_io_ProcessingStream, xrtl_io_zlib_StreamProcessor;

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

procedure TzlibDemo_MainForm.TestBtnClick(Sender: TObject);
const
  DataLength = 200;//000;
var
  TFOS: TXRTLTempFileOutputStream;
  ZOS: TXRTLzlibOutputStream;
  FIS: TXRTLFileInputStream;
  ZIS: TXRTLzlibInputStream;
  I, RData, Data: Integer;
begin
  TFOS:= nil;
  ZOS:= nil;
  FIS:= nil;
  ZIS:= nil;
  try
    WriteLn('Creating TOSzlibOutputStream');
    TFOS:= TXRTLTempFileOutputStream.Create('', False);
    ZOS:= TXRTLzlibOutputStream.Create(TFOS, True, 1024);
    WriteLn('Writing data');
    Data:= 0;
    for I:= 0 to DataLength - 1 do
    begin
      ZOS.WriteBuffer(I, SizeOf(I));
      Inc(Data);
    end;
    WriteLn('Data written, closing stream');
    ZOS.Close;
    WriteLn('Creating TOSzlibInputStream');
    ZIS:= TXRTLzlibInputStream.Create(TXRTLFileInputStream.Create(TFOS.FilePath, True));
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

procedure TzlibDemo_MainForm.Test2BtnClick(Sender: TObject);
const
  DataLength = 200000;
var
  TFOS: TXRTLTempFileOutputStream;
  POS: TXRTLProcessingOutputStream;
  ZSP: TXRTLzlibStreamProcessor;
  FIS: TXRTLFileInputStream;
  PIS: TXRTLProcessingInputStream;
  I, RData, Data: Integer;
begin
  TFOS:= nil;
  POS:= nil;
  FIS:= nil;
  PIS:= nil;
  try
    WriteLn('Creating TXRTLProcessingOutputStream');
    TFOS:= TXRTLTempFileOutputStream.Create('', False);
    ZSP:= TXRTLzlibStreamProcessor.Create;
    POS:= TXRTLProcessingOutputStream.Create(TFOS, ZSP, True, False);
    ZSP.InitCompress;
    WriteLn('Writing data');
    Data:= 0;
    for I:= 0 to DataLength - 1 do
    begin
      POS.WriteBuffer(I, SizeOf(I));
      Inc(Data);
    end;
    WriteLn('Data written, closing stream');
    POS.Close;
    WriteLn('Creating TXRTLProcessingInputStream');
    PIS:= TXRTLProcessingInputStream.Create(TXRTLFileInputStream.Create(TFOS.FilePath, True), ZSP);
    ZSP.InitDecompress;
    WriteLn('Reading data');
    Data:= 0;
    I:= -1;
    repeat
      Inc(I);
      Data:= PIS.ReadBuffer(RData, SizeOf(RData));
      if (Data > 0) and (RData <> I) then
        WriteLn(Format('Data = %8.x, RData = %8.x', [I, RData]));
    until Data < 0;
    if I <> DataLength then
      WriteLn(Format('Data = %8.x, DataLength = %8.x', [I, DataLength]));
    WriteLn('Data read, closing stream');
    PIS.Close;
  finally
    FreeAndNil(PIS);
    FreeAndNil(POS);
  end;
end;

procedure TzlibDemo_MainForm.Test3BtnClick(Sender: TObject);
const
  DataLength = 200000;
var
  TFOS: TXRTLTempFileOutputStream;
  POS1, POS2: TXRTLProcessingOutputStream;
  ZSP1, ZSP2: TXRTLzlibStreamProcessor;
  FIS: TXRTLFileInputStream;
  PIS1, PIS2: TXRTLProcessingInputStream;
  I, RData, Data: Integer;
begin
  TFOS:= nil;
  POS1:= nil;
  POS2:= nil;
  FIS:= nil;
  PIS1:= nil;
  PIS2:= nil;
  try
    WriteLn('Creating TXRTLProcessingOutputStream');
    TFOS:= TXRTLTempFileOutputStream.Create('', False);
    ZSP1:= TXRTLzlibStreamProcessor.Create;
    ZSP2:= TXRTLzlibStreamProcessor.Create;
    POS1:= TXRTLProcessingOutputStream.Create(TFOS, ZSP1, True, False);
    POS2:= TXRTLProcessingOutputStream.Create(POS1, ZSP2, True, False);
    ZSP1.InitDecompress;
    ZSP2.InitCompress;
    WriteLn('Writing data');
    Data:= 0;
    for I:= 0 to DataLength - 1 do
    begin
      POS2.WriteBuffer(I, SizeOf(I));
      Inc(Data);
    end;
    WriteLn('Data written, closing stream');
    POS2.Close;
    WriteLn('Creating TXRTLProcessingInputStream');
    PIS1:= TXRTLProcessingInputStream.Create(TXRTLFileInputStream.Create(TFOS.FilePath, True), ZSP1);
    PIS2:= TXRTLProcessingInputStream.Create(PIS1, ZSP2, True, False);
    ZSP1.InitCompress;
    ZSP2.InitDecompress;
    WriteLn('Reading data');
    Data:= 0;
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

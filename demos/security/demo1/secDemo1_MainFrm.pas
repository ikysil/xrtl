unit secDemo1_MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts;

type
  TsecDemo_MainForm = class(TForm)
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
  secDemo_MainForm: TsecDemo_MainForm;

implementation

{$R *.dfm}

uses
  xrtl_io_FileStream,
  xrtl_io_ProcessingStream,
  secDemo1_Classes,
  xrtl_crypt_cipher_Blowfish;

const
  DataLength = 20000;
  FName = 'c:\temp\secTest1';

{ TbzlibDemo_MainForm }

procedure TsecDemo_MainForm.WriteLn(S: string);
begin
  LogMemo.Lines.Add(S);
  Application.ProcessMessages;
end;

procedure TsecDemo_MainForm.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
begin
  WriteLn(Format('Exception %s: %s', [E.ClassName, E.Message]));
end;

procedure TsecDemo_MainForm.WriteBtnClick(Sender: TObject);
var
  POS2: TXRTLProcessingOutputStream;
  ZSP2: TXRTLIdentityBlockCipher;
  PIS2: TXRTLProcessingInputStream;
  I: Integer;
begin
  POS2:= nil;
  PIS2:= nil;
  try
    WriteLn('Creating TXRTLProcessingOutputStream');
    ZSP2:= TXRTLIdentityBlockCipher.Create;
    POS2:= TXRTLProcessingOutputStream.Create(TXRTLFileOutputStream.Create(FName, False), ZSP2, True, False);
    ZSP2.InitEncipher;
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

procedure TsecDemo_MainForm.ReadBtnClick(Sender: TObject);
var
  POS2: TXRTLProcessingOutputStream;
  ZSP2: TXRTLIdentityBlockCipher;
  PIS2: TXRTLProcessingInputStream;
  I, RData, Data: Integer;
begin
  POS2:= nil;
  PIS2:= nil;
  try
    WriteLn('Creating TXRTLProcessingInputStream');
    ZSP2:= TXRTLIdentityBlockCipher.Create;
    PIS2:= TXRTLProcessingInputStream.Create(TXRTLFileInputStream.Create(FName, False), ZSP2, True, False);
    ZSP2.InitDecipher;
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

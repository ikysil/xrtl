unit secDemo2_MainFrm;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, AppEvnts, Menus,
  xrtl_crypt_Cipher, xrtl_crypt_BlockCipherPadding, xrtl_crypt_BlockCipher,
  xrtl_crypt_BlockCipherMode;

type
  TsecDemo_MainForm = class(TForm)
    WriteBtn: TButton;
    LogMemo: TMemo;
    ApplicationEvents1: TApplicationEvents;
    ReadBtn: TButton;
    GroupBox1: TGroupBox;
    CipherCB: TComboBox;
    Label2: TLabel;
    Label1: TLabel;
    CipherPaddingCB: TComboBox;
    Label3: TLabel;
    CipherModeCB: TComboBox;
    WriteFileCB: TCheckBox;
    procedure WriteBtnClick(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure ReadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CipherCBChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function   GetCipher: TXRTLCipher;
    function   GetCipherPadding: TXRTLBlockCipherPadding;
    function   GetCipherMode: TXRTLBlockCipherMode;
  public
    procedure  WriteLn(S: string);
  end;

var
  secDemo_MainForm: TsecDemo_MainForm;

implementation

{$R *.dfm}

uses
  xrtl_io_Stream, xrtl_io_FileStream, xrtl_io_ProcessingStream, xrtl_io_BufferedStream,
  xrtl_crypt_cipher_Blowfish, xrtl_crypt_cipher_Twofish, xrtl_crypt_cipher_Rijndael,
  xrtl_crypt_cipher_AES, xrtl_crypt_cipher_Anubis, xrtl_crypt_cipher_Khazad;

const
  DataLength = 16 * 1024 * 1024;//3 * 1021 * 1027;
  BufferSize = 4096;
  FName = 'c:\temp\secTest2';
  DelimiterLength = 20;
  Key = '0123456789ABCDEF';

{ TsecDemo_MainForm }

procedure TsecDemo_MainForm.FormCreate(Sender: TObject);

  procedure AddCipher(ACipher: TXRTLCipher);
  begin
    CipherCB.Items.AddObject(ACipher.GetDisplayName, ACipher);
  end;

  procedure AddCipherPadding(ACipherPadding: TXRTLBlockCipherPadding);
  begin
    CipherPaddingCB.Items.AddObject(ACipherPadding.GetDisplayName, ACipherPadding);
  end;

  procedure AddCipherMode(ACipherMode: TXRTLBlockCipherMode);
  begin
    CipherModeCB.Items.AddObject(ACipherMode.GetDisplayName, ACipherMode);
  end;

begin
// fill ciphers list
  CipherCB.Items.Clear;
  AddCipher(TXRTLBlowfishCipher.Create);
  AddCipher(TXRTLTwofishCipher.Create);
  AddCipher(TXRTLRijndael128Cipher.Create);
  AddCipher(TXRTLRijndael160Cipher.Create);
  AddCipher(TXRTLRijndael192Cipher.Create);
  AddCipher(TXRTLRijndael224Cipher.Create);
  AddCipher(TXRTLRijndael256Cipher.Create);
  AddCipher(TXRTLAESCipher.Create);
  AddCipher(TXRTLAnubisCipher.Create);
  AddCipher(TXRTLKhazadCipher.Create);
// fill cipher paddings list
  CipherPaddingCB.Items.Clear;
  CipherPaddingCB.Items.AddObject('(no padding)', nil);
  AddCipherPadding(TXRTLOneAndZeroesBlockCipherPadding.Create);
  AddCipherPadding(TXRTLPKCS5BlockCipherPadding.Create);
  AddCipherPadding(TXRTLPKCS7BlockCipherPadding.Create);
// fill cipher modes list
  CipherModeCB.Items.Clear;
  CipherModeCB.Items.AddObject('(no mode)', nil);
  AddCipherMode(TXRTLECBCipherMode.Create);
  AddCipherMode(TXRTLCBCCipherMode.Create);
  AddCipherMode(TXRTLOFB8BitCipherMode.Create);
  AddCipherMode(TXRTLOFBBlockCipherMode.Create);
  AddCipherMode(TXRTLCFB8BitCipherMode.Create);
  AddCipherMode(TXRTLCFBBlockCipherMode.Create);
  AddCipherMode(TXRTLBCCipherMode.Create);
  AddCipherMode(TXRTLPCBCCipherMode.Create);
  AddCipherMode(TXRTLCBCCCipherMode.Create);
end;

procedure TsecDemo_MainForm.FormDestroy(Sender: TObject);
begin
  while CipherCB.Items.Count > 0 do
  begin
    CipherCB.Items.Objects[0].Free;
    CipherCB.Items.Delete(0);
  end;
end;

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
  BOS2: TXRTLBufferedOutputStream;
  COS2: TXRTLCountOutputStream;
  POS2: TXRTLProcessingOutputStream;
  ZSP2: TXRTLCipher;
  I: Integer;
  DT: TDateTime;
  BSec: Double;
  PS: TXRTLOutputStream;
begin
  BOS2:= nil;
  try
    WriteLn(StringOfChar('-', DelimiterLength));
    ZSP2:= GetCipher;
    if ZSP2 is TXRTLBlockCipher then
    begin
      (ZSP2 as TXRTLBlockCipher).SetCipherPadding(GetCipherPadding, False);
      (ZSP2 as TXRTLBlockCipher).SetCipherMode(GetCipherMode, False);
    end;
    ZSP2.SetKey(Key);
    ZSP2.InitEncipher;
    if WriteFileCB.Checked then
      PS:= TXRTLFileOutputStream.Create(FName, False)
    else
      PS:= TXRTLNullOutputStream.Create;
    COS2:= TXRTLCountOutputStream.Create(PS);
    POS2:= TXRTLProcessingOutputStream.Create(COS2, ZSP2, True, False);
    BOS2:= TXRTLBufferedOutputStream.Create(POS2, True, BufferSize);
    WriteLn('Writing data');
    DT:= Now;
    for I:= 0 to DataLength - 1 do
    begin
      BOS2.WriteBuffer(I, SizeOf(Byte));//SizeOf(I));
    end;
    WriteLn('Data written, closing stream');
    BOS2.Close;
    Sleep(10);
    BSec:= COS2.Position / ((Now - DT) * SecsPerDay);
    WriteLn(Format('%.3f KBytes, %f KB/sec, %f MB/sec',
                   [COS2.Position / 1024, BSec / 1024, BSec / 1024 / 1024]));
    ReadBtn.Enabled:= WriteFileCB.Checked;
  finally
    FreeAndNil(BOS2);
  end;
end;

procedure TsecDemo_MainForm.ReadBtnClick(Sender: TObject);
var
  BIS2: TXRTLBufferedInputStream;
  CIS2: TXRTLCountInputStream;
  ZSP2: TXRTLCipher;
  PIS2: TXRTLProcessingInputStream;
  RData: Byte;
  I, Data: Integer;
  DT: TDateTime;
  BSec: Double;
begin
  BIS2:= nil;
  try
    WriteLn(StringOfChar('-', DelimiterLength));
    ZSP2:= GetCipher;
    if ZSP2 is TXRTLBlockCipher then
    begin
      (ZSP2 as TXRTLBlockCipher).SetCipherPadding(GetCipherPadding, False);
      (ZSP2 as TXRTLBlockCipher).SetCipherMode(GetCipherMode, False);
    end;
    ZSP2.SetKey(Key);
    ZSP2.InitDecipher;
    CIS2:= TXRTLCountInputStream.Create(TXRTLFileInputStream.Create(FName, False));
    PIS2:= TXRTLProcessingInputStream.Create(CIS2, ZSP2, True, False);
    BIS2:= TXRTLBufferedInputStream.Create(PIS2, True, BufferSize);
    WriteLn('Reading data');
    DT:= Now;
    I:= -1;
    repeat
      Inc(I);
      RData:= 0;
      Data:= BIS2.ReadBuffer(RData, SizeOf(RData));
      if (Data > 0) and (RData <> Byte(I)) then
        Break;//        WriteLn(Format('Data = %8.x, RData = %8.x', [I, RData]));
    until Data < 0;
    if I <> DataLength then
      WriteLn(Format('Data = %8.x, DataLength = %8.x', [I, DataLength]));
    WriteLn('Data read, closing stream');
    BIS2.Close;
    Sleep(10);
    BSec:= CIS2.Position / ((Now - DT) * SecsPerDay);
    WriteLn(Format('%.3f KBytes, %f KB/sec, %f MB/sec',
                   [CIS2.Position / 1024, BSec / 1024, BSec / 1024 / 1024]));
  finally
    FreeAndNil(BIS2);
  end;
end;

procedure TsecDemo_MainForm.CipherCBChange(Sender: TObject);
begin
  ReadBtn.Enabled:= False;
end;

function TsecDemo_MainForm.GetCipher: TXRTLCipher;
begin
  if CipherCB.ItemIndex < 0 then
    raise Exception.Create('Select cipher first...');
  Result:= CipherCB.Items.Objects[CipherCB.ItemIndex] as TXRTLCipher;
  WriteLn(Format('Cipher: %s', [Result.GetDisplayName]));
end;

function TsecDemo_MainForm.GetCipherPadding: TXRTLBlockCipherPadding;
begin
  Result:= nil;
  if CipherPaddingCB.ItemIndex < 0 then
    Exit;
  Result:= CipherPaddingCB.Items.Objects[CipherPaddingCB.ItemIndex] as TXRTLBlockCipherPadding;
  if Assigned(Result) then
    WriteLn(Format('Padding: %s', [Result.GetDisplayName]));
end;

function TsecDemo_MainForm.GetCipherMode: TXRTLBlockCipherMode;
begin
  Result:= nil;
  if CipherModeCB.ItemIndex < 0 then
    Exit;
  Result:= CipherModeCB.Items.Objects[CipherModeCB.ItemIndex] as TXRTLBlockCipherMode;
  if Assigned(Result) then
    WriteLn(Format('Mode: %s', [Result.GetDisplayName]));
end;

end.

object MainForm: TMainForm
  Left = 445
  Top = 523
  Width = 604
  Height = 436
  Caption = 'OPC DA 2.0 Client Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ConnectBtn: TButton
    Left = 10
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Connect'
    TabOrder = 0
    OnClick = ConnectBtnClick
  end
end

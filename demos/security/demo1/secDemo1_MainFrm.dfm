object secDemo_MainForm: TsecDemo_MainForm
  Left = 517
  Top = 351
  BorderStyle = bsSingle
  Caption = 'Security demo 1'
  ClientHeight = 303
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object WriteBtn: TButton
    Left = 12
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Write data'
    TabOrder = 0
    OnClick = WriteBtnClick
  end
  object LogMemo: TMemo
    Left = 6
    Top = 40
    Width = 429
    Height = 257
    TabOrder = 2
  end
  object ReadBtn: TButton
    Left = 92
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Read data'
    TabOrder = 1
    OnClick = ReadBtnClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 408
    Top = 6
  end
end

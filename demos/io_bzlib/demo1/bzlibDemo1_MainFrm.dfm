object bzlibDemo_MainForm: TbzlibDemo_MainForm
  Left = 517
  Top = 351
  BorderStyle = bsSingle
  Caption = 'bzlib demo 1'
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
  object TestBtn: TButton
    Left = 12
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = TestBtnClick
  end
  object LogMemo: TMemo
    Left = 6
    Top = 40
    Width = 429
    Height = 257
    TabOrder = 1
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 106
    Top = 4
  end
end

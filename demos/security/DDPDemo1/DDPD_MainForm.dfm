object Form1: TForm1
  Left = 494
  Top = 222
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Data Dependent Permutations demo'
  ClientHeight = 68
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object VLbl: TLabel
    Left = 102
    Top = 8
    Width = 131
    Height = 15
    AutoSize = False
    Caption = 'VLbl'
  end
  object SLbl: TLabel
    Left = 102
    Top = 27
    Width = 131
    Height = 15
    AutoSize = False
    Caption = 'SLbl'
  end
  object StartBtn: TButton
    Left = 6
    Top = 4
    Width = 75
    Height = 25
    Caption = '&Start'
    Default = True
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object StopBtn: TButton
    Left = 6
    Top = 36
    Width = 75
    Height = 25
    Caption = '&Stop'
    Enabled = False
    TabOrder = 1
    OnClick = StopBtnClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 296
    Top = 34
  end
end

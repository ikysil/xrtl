object Form1: TForm1
  Left = 563
  Top = 327
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 31
  ClientWidth = 104
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMinimized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ASyncTimer: TTimer
    Interval = 250
    Left = 2
    Top = 2
  end
  object RefreshCacheTimer: TTimer
    Interval = 250
    OnTimer = RefreshCacheTimer1Timer
    Left = 32
    Top = 2
  end
end

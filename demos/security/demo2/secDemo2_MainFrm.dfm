object secDemo_MainForm: TsecDemo_MainForm
  Left = 354
  Top = 258
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Security demo 2'
  ClientHeight = 386
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    553
    386)
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 5
    Top = 106
    Width = 543
    Height = 273
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object WriteBtn: TButton
    Left = 446
    Top = 12
    Width = 75
    Height = 25
    Caption = '&Write data'
    TabOrder = 1
    OnClick = WriteBtnClick
  end
  object ReadBtn: TButton
    Left = 446
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Read data'
    Enabled = False
    TabOrder = 2
    OnClick = ReadBtnClick
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 407
    Height = 95
    Caption = 'Options'
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 19
      Width = 33
      Height = 13
      Caption = '&Cipher:'
      FocusControl = CipherCB
    end
    object Label1: TLabel
      Left = 8
      Top = 44
      Width = 42
      Height = 13
      Caption = '&Padding:'
      FocusControl = CipherPaddingCB
    end
    object Label3: TLabel
      Left = 8
      Top = 69
      Width = 30
      Height = 13
      Caption = '&Mode:'
      FocusControl = CipherModeCB
    end
    object CipherCB: TComboBox
      Left = 55
      Top = 15
      Width = 344
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = CipherCBChange
    end
    object CipherPaddingCB: TComboBox
      Left = 55
      Top = 40
      Width = 344
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = CipherCBChange
    end
    object CipherModeCB: TComboBox
      Left = 55
      Top = 65
      Width = 344
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = CipherCBChange
    end
  end
  object WriteFileCB: TCheckBox
    Left = 446
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Write to file'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 494
    Top = 128
  end
end

object MainFrm: TMainFrm
  Left = 304
  Top = 242
  ActiveControl = ValueEdit
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'xrtl_math demo 1'
  ClientHeight = 697
  ClientWidth = 854
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
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 125
    Height = 697
    ActivePage = TabSheet5
    Align = alLeft
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    TabPosition = tpLeft
    object TabSheet5: TTabSheet
      Caption = 'Value'
      ImageIndex = 4
      DesignSize = (
        98
        689)
      object ClearBtn: TButton
        Left = 4
        Top = 4
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'C'
        TabOrder = 0
        OnClick = ClearBtnClick
      end
      object SetValueBtn: TButton
        Left = 4
        Top = 34
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = ':='
        TabOrder = 1
        OnClick = SetValueBtnClick
      end
      object ShowBtn: TButton
        Left = 4
        Top = 64
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show value'
        TabOrder = 2
        OnClick = ShowBtnClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Arithmetics'
      DesignSize = (
        98
        689)
      object AddBtn: TButton
        Left = 4
        Top = 4
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '+'
        TabOrder = 0
        OnClick = AddBtnClick
      end
      object MulBtn: TButton
        Left = 4
        Top = 64
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '*'
        TabOrder = 2
        OnClick = MulBtnClick
      end
      object NegBtn: TButton
        Left = 4
        Top = 124
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '-x'
        TabOrder = 4
        OnClick = NegBtnClick
      end
      object AbsBtn: TButton
        Left = 4
        Top = 154
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '|x|'
        TabOrder = 5
        OnClick = AbsBtnClick
      end
      object DivBtn: TButton
        Left = 4
        Top = 94
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '/'
        TabOrder = 3
        OnClick = DivBtnClick
      end
      object SubBtn: TButton
        Left = 4
        Top = 34
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '-'
        TabOrder = 1
        OnClick = SubBtnClick
      end
      object ExpBtn: TButton
        Left = 4
        Top = 184
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Exp'
        TabOrder = 6
        OnClick = ExpBtnClick
      end
      object SQRBtn: TButton
        Left = 4
        Top = 214
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Sqr'
        TabOrder = 7
        OnClick = SQRBtnClick
      end
      object SQRTBtn: TButton
        Left = 4
        Top = 244
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Sqrt'
        TabOrder = 8
        OnClick = SQRTBtnClick
      end
      object GCDBtn: TButton
        Left = 4
        Top = 304
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GCD'
        TabOrder = 9
        OnClick = GCDBtnClick
      end
      object RootBtn: TButton
        Left = 4
        Top = 274
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Root'
        TabOrder = 10
        OnClick = RootBtnClick
      end
      object FactBtn: TButton
        Left = 4
        Top = 334
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'x!'
        TabOrder = 11
        OnClick = FactBtnClick
      end
      object FactModBtn: TButton
        Left = 4
        Top = 364
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'x!mod'
        TabOrder = 12
        OnClick = FactModBtnClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Bit'
      ImageIndex = 1
      DesignSize = (
        98
        689)
      object SetBitBtn: TButton
        Left = 4
        Top = 4
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SB'
        TabOrder = 0
        OnClick = SetBitBtnClick
      end
      object ResetBitBtn: TButton
        Left = 4
        Top = 34
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RB'
        TabOrder = 1
        OnClick = ResetBitBtnClick
      end
      object InvertBtn: TButton
        Left = 4
        Top = 64
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'not'
        TabOrder = 2
        OnClick = InvertBtnClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Shifts'
      ImageIndex = 2
      DesignSize = (
        98
        689)
      object RCDLBtn: TButton
        Left = 4
        Top = 4
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RCDL'
        TabOrder = 0
        OnClick = RCDLBtnClick
      end
      object SADLBtn: TButton
        Left = 4
        Top = 34
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SADL'
        TabOrder = 1
        OnClick = SADLBtnClick
      end
      object SLDLBtn: TButton
        Left = 4
        Top = 64
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SLDL'
        TabOrder = 2
        OnClick = SLDLBtnClick
      end
      object RCBLBtn: TButton
        Left = 4
        Top = 94
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RCBL'
        TabOrder = 3
        OnClick = RCBLBtnClick
      end
      object SABLBtn: TButton
        Left = 4
        Top = 124
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SABL'
        TabOrder = 4
        OnClick = SABLBtnClick
      end
      object SLBLBtn: TButton
        Left = 4
        Top = 154
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SLBL'
        TabOrder = 5
        OnClick = SLBLBtnClick
      end
      object SLBRBtn: TButton
        Left = 4
        Top = 334
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SLBR'
        TabOrder = 11
        OnClick = SLBRBtnClick
      end
      object SABRBtn: TButton
        Left = 4
        Top = 304
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SABR'
        TabOrder = 10
        OnClick = SABRBtnClick
      end
      object RCBRBtn: TButton
        Left = 4
        Top = 274
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RCBR'
        TabOrder = 9
        OnClick = RCBRBtnClick
      end
      object SLDRBtn: TButton
        Left = 4
        Top = 244
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SLDR'
        TabOrder = 8
        OnClick = SLDRBtnClick
      end
      object SADRBtn: TButton
        Left = 4
        Top = 214
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SADR'
        TabOrder = 7
        OnClick = SADRBtnClick
      end
      object RCDR: TButton
        Left = 4
        Top = 184
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RCDR'
        TabOrder = 6
        OnClick = RCDRClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Misc'
      ImageIndex = 3
      DesignSize = (
        98
        689)
      object GetMSBitIndexBtn: TButton
        Left = 4
        Top = 4
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'MSBI'
        TabOrder = 0
        OnClick = GetMSBitIndexBtnClick
      end
      object NormalizeBtn: TButton
        Left = 4
        Top = 34
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Strip'
        TabOrder = 1
        OnClick = NormalizeBtnClick
      end
      object CompareBtn: TButton
        Left = 4
        Top = 94
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = '<?>'
        TabOrder = 3
        OnClick = CompareBtnClick
      end
      object SetDataBitsBtn: TButton
        Left = 4
        Top = 64
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Bits'
        TabOrder = 2
        OnClick = SetDataBitsBtnClick
      end
      object TestExpBtn: TButton
        Left = 4
        Top = 124
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'TestExp'
        TabOrder = 4
        OnClick = TestExpBtnClick
      end
      object TestExpModBtn: TButton
        Left = 4
        Top = 154
        Width = 90
        Height = 30
        Anchors = [akLeft, akTop, akRight]
        Caption = 'TestExpMod'
        TabOrder = 5
        OnClick = TestExpModBtnClick
      end
    end
  end
  object Panel1: TPanel
    Left = 125
    Top = 0
    Width = 729
    Height = 697
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Panel1'
    TabOrder = 1
    object Memo: TMemo
      Left = 4
      Top = 68
      Width = 721
      Height = 625
      Align = alBottom
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object ValueEdit: TEdit
      Left = 4
      Top = 12
      Width = 719
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object RRCB: TCheckBox
      Left = 6
      Top = 42
      Width = 163
      Height = 17
      Caption = 'Report each result'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 370
    Top = 342
  end
end

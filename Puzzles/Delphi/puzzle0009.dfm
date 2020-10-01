object frmPuzzle0009: TfrmPuzzle0009
  Left = 156
  Top = 73
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmPuzzle0009'
  ClientHeight = 795
  ClientWidth = 980
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object gbSettings: TGroupBox
    Left = 10
    Top = 5
    Width = 760
    Height = 100
    Caption = 'Settings'
    TabOrder = 0
    object lblTotalItems: TLabel
      Left = 15
      Top = 25
      Width = 66
      Height = 16
      Caption = 'Total items'
    end
    object lblGroupSize: TLabel
      Left = 15
      Top = 60
      Width = 64
      Height = 16
      Caption = 'Group size'
    end
    object ebTotalItems: TEdit
      Left = 100
      Top = 20
      Width = 40
      Height = 28
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = '18'
    end
    object ebGroupSize: TEdit
      Left = 100
      Top = 55
      Width = 40
      Height = 28
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      Text = '3'
    end
    object btnStart: TButton
      Left = 460
      Top = 50
      Width = 100
      Height = 35
      Caption = 'Start !'
      Default = True
      TabOrder = 6
      OnClick = btnStartClick
    end
    object trackerTotalItems: TTrackBar
      Left = 145
      Top = 20
      Width = 610
      Height = 28
      Max = 120
      Min = 3
      Position = 18
      TabOrder = 1
      ThumbLength = 25
      TickStyle = tsNone
      OnChange = OnTotalItemsChange
    end
    object trackerGroupSize: TTrackBar
      Left = 145
      Top = 55
      Width = 120
      Height = 28
      Max = 6
      Min = 2
      Position = 3
      TabOrder = 3
      ThumbLength = 25
      TickStyle = tsNone
      OnChange = OnGroupSizeChange
    end
    object tbSortGroupsInternal: TCheckBox
      Left = 275
      Top = 50
      Width = 160
      Height = 17
      Caption = 'Sort groups (internal) ?'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object tbSortGroupsExternal: TCheckBox
      Left = 275
      Top = 70
      Width = 160
      Height = 17
      Caption = 'Sort between groups ?'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object btnExit: TButton
    Left = 835
    Top = 755
    Width = 140
    Height = 35
    Caption = 'Exit puzzle 0009'
    TabOrder = 2
    OnClick = btnExitClick
  end
  object gbResults: TGroupBox
    Left = 10
    Top = 110
    Width = 960
    Height = 640
    Caption = 'Results'
    TabOrder = 1
    object lblTimeTitle: TLabel
      Left = 15
      Top = 22
      Width = 31
      Height = 16
      Caption = 'Time'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblTime: TLabel
      Left = 100
      Top = 22
      Width = 41
      Height = 16
      Caption = '0.000 s'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object memoResults: TMemo
      Left = 5
      Top = 45
      Width = 950
      Height = 590
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object pbProgress: TProgressBar
      Left = 155
      Top = 23
      Width = 795
      Height = 12
      TabOrder = 0
      Visible = False
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 150
    OnTimer = OnUpdateTimer
    Left = 800
    Top = 760
  end
end

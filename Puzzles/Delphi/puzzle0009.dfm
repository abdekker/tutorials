object frmPuzzle0009: TfrmPuzzle0009
  Left = 254
  Top = 46
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmPuzzle0009'
  ClientHeight = 715
  ClientWidth = 680
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
    Width = 660
    Height = 140
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
      Top = 65
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
      Top = 60
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
      Left = 100
      Top = 95
      Width = 80
      Height = 35
      Caption = 'Start !'
      TabOrder = 4
      OnClick = btnStartClick
    end
    object trackerTotalItems: TTrackBar
      Left = 145
      Top = 20
      Width = 510
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
      Top = 60
      Width = 510
      Height = 28
      Max = 6
      Min = 2
      Position = 3
      TabOrder = 3
      ThumbLength = 25
      TickStyle = tsNone
      OnChange = OnGroupSizeChange
    end
  end
  object btnExit: TButton
    Left = 535
    Top = 675
    Width = 140
    Height = 35
    Caption = 'Exit puzzle 0009'
    TabOrder = 1
    OnClick = btnExitClick
  end
  object gbResults: TGroupBox
    Left = 10
    Top = 150
    Width = 660
    Height = 525
    Caption = 'Results'
    TabOrder = 2
    object lblTimeTitle: TLabel
      Left = 15
      Top = 25
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
      Top = 25
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
      Width = 650
      Height = 475
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 150
    OnTimer = OnUpdateTimer
    Left = 500
    Top = 680
  end
end

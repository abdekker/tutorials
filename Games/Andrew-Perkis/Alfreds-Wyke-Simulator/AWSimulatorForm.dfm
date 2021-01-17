object frmAWSimulator: TfrmAWSimulator
  Left = 861
  Top = 257
  BorderStyle = bsSingle
  Caption = 'Alfred'#39's Wyke Simulator; Copyright Andrew Perkis'
  ClientHeight = 410
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object btnOk: TButton
    Left = 435
    Top = 365
    Width = 120
    Height = 40
    Caption = 'Ok'
    TabOrder = 0
    OnClick = btnOkClick
  end
  object gbSettings: TGroupBox
    Left = 5
    Top = 5
    Width = 265
    Height = 115
    Caption = 'Settings'
    TabOrder = 1
    object lblBoardSize: TLabel
      Left = 15
      Top = 25
      Width = 66
      Height = 16
      Caption = 'Board Size'
    end
    object ddlBoardSize: TComboBox
      Left = 15
      Top = 45
      Width = 145
      Height = 28
      Style = csDropDownList
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 20
      ParentFont = False
      TabOrder = 0
      OnCloseUp = ddlBoardSizeCloseUp
      OnDropDown = ddlBoardSizeDropDown
    end
    object tbAlwaysTake11111: TCheckBox
      Left = 15
      Top = 80
      Width = 225
      Height = 17
      Caption = 'Always take 1-1-1-1-1 (if available)'
      TabOrder = 1
    end
  end
  object gbResults: TGroupBox
    Left = 5
    Top = 125
    Width = 550
    Height = 235
    Caption = 'Results'
    TabOrder = 2
    object lblNumberOfGamesTitle: TLabel
      Left = 15
      Top = 70
      Width = 121
      Height = 16
      Caption = 'Total games played'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblNumberOfGames: TLabel
      Left = 190
      Top = 70
      Width = 7
      Height = 16
      Caption = '0'
    end
    object lblWinsForPlayerTitle: TLabel
      Left = 15
      Top = 90
      Width = 122
      Height = 16
      Caption = 'Wins for each player'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblWinsForPlayer: TLabel
      Left = 190
      Top = 90
      Width = 215
      Height = 16
      Caption = 'B = 1234 (52.56%); D = 1197 (47.44%)'
    end
    object lblWinByTypeTitle: TLabel
      Left = 15
      Top = 154
      Width = 70
      Height = 16
      Caption = 'Win by type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblLine: TLabel
      Left = 190
      Top = 154
      Width = 55
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '9876543'
    end
    object lblDiagonal: TLabel
      Left = 260
      Top = 154
      Width = 55
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '234'
    end
    object lblBlock: TLabel
      Left = 330
      Top = 154
      Width = 55
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '345'
    end
    object lblCount: TLabel
      Left = 400
      Top = 154
      Width = 55
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '456'
    end
    object lblDifference: TLabel
      Left = 470
      Top = 154
      Width = 55
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'NA'
    end
    object lblGameLengthTitle: TLabel
      Left = 15
      Top = 110
      Width = 105
      Height = 16
      Caption = 'Game length (ply)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblGameLength: TLabel
      Left = 190
      Top = 110
      Width = 99
      Height = 16
      Caption = 'Min = 5; Max = 72'
    end
    object lblTilesRequiredTitle: TLabel
      Left = 15
      Top = 197
      Width = 137
      Height = 16
      Caption = 'Maximum tiles required'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblTile0Title: TLabel
      Left = 190
      Top = 180
      Width = 7
      Height = 16
      Caption = '0'
    end
    object lblTile1Title: TLabel
      Left = 230
      Top = 180
      Width = 7
      Height = 16
      Caption = '1'
    end
    object lblTile2Title: TLabel
      Left = 270
      Top = 180
      Width = 7
      Height = 16
      Caption = '2'
    end
    object lblTile3Title: TLabel
      Left = 310
      Top = 180
      Width = 7
      Height = 16
      Caption = '3'
    end
    object lblTitle4Title: TLabel
      Left = 350
      Top = 180
      Width = 7
      Height = 16
      Caption = '4'
    end
    object lblTitle5Title: TLabel
      Left = 390
      Top = 180
      Width = 7
      Height = 16
      Caption = '5'
    end
    object lblTitle6Title: TLabel
      Left = 430
      Top = 180
      Width = 7
      Height = 16
      Caption = '6'
    end
    object lblTitle7Title: TLabel
      Left = 470
      Top = 180
      Width = 7
      Height = 16
      Caption = '7'
    end
    object lblTitle8Title: TLabel
      Left = 510
      Top = 180
      Width = 7
      Height = 16
      Caption = '8'
    end
    object lblTile0: TLabel
      Left = 190
      Top = 197
      Width = 14
      Height = 16
      Caption = '12'
    end
    object lblTile1: TLabel
      Left = 230
      Top = 197
      Width = 7
      Height = 16
      Caption = '9'
    end
    object lblTile2: TLabel
      Left = 270
      Top = 197
      Width = 14
      Height = 16
      Caption = '14'
    end
    object lblTile3: TLabel
      Left = 310
      Top = 197
      Width = 14
      Height = 16
      Caption = '15'
    end
    object lblTile4: TLabel
      Left = 350
      Top = 197
      Width = 14
      Height = 16
      Caption = '36'
    end
    object lblTile5: TLabel
      Left = 390
      Top = 197
      Width = 14
      Height = 16
      Caption = '15'
    end
    object lblTile6: TLabel
      Left = 430
      Top = 197
      Width = 14
      Height = 16
      Caption = '14'
    end
    object lblTile7: TLabel
      Left = 470
      Top = 197
      Width = 7
      Height = 16
      Caption = '9'
    end
    object lblTile8: TLabel
      Left = 510
      Top = 197
      Width = 14
      Height = 16
      Caption = '12'
    end
    object btnStartAnalysis: TButton
      Left = 15
      Top = 25
      Width = 150
      Height = 40
      Caption = 'Start Analysis'
      TabOrder = 0
      OnClick = btnStartAnalysisClick
    end
    object btnStopAnalysis: TButton
      Left = 190
      Top = 25
      Width = 150
      Height = 40
      Caption = 'Stop Analysis'
      TabOrder = 1
      OnClick = btnStopAnalysisClick
    end
    object lblLineTitle: TStaticText
      Left = 190
      Top = 135
      Width = 55
      Height = 19
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Line'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object lblBlockTitle: TStaticText
      Left = 330
      Top = 135
      Width = 55
      Height = 19
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Block'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object lblCountTitle: TStaticText
      Left = 400
      Top = 135
      Width = 55
      Height = 19
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Count'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object lblDifferenceTitle: TStaticText
      Left = 470
      Top = 135
      Width = 55
      Height = 19
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Diff'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object lblDiagonalTitle: TStaticText
      Left = 260
      Top = 135
      Width = 55
      Height = 19
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Diag'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
  end
  object UpdateTimer: TTimer
    Interval = 500
    OnTimer = OnUpdateTimerTick
    Left = 400
    Top = 365
  end
end

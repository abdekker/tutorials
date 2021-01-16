object frmAWSimulator: TfrmAWSimulator
  Left = 494
  Top = 356
  BorderStyle = bsSingle
  Caption = 'Alfred'#39's Wyke Simulator; Copyright Andrew Perkis'
  ClientHeight = 270
  ClientWidth = 890
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object btnOk: TButton
    Left = 765
    Top = 225
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
    Height = 215
    Caption = 'Settings'
    TabOrder = 1
    object lblBoardSize: TLabel
      Left = 20
      Top = 30
      Width = 66
      Height = 16
      Caption = 'Board Size'
    end
    object ddlBoardSize: TComboBox
      Left = 20
      Top = 50
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
    object tbFirstMove11111: TCheckBox
      Left = 20
      Top = 85
      Width = 220
      Height = 17
      Caption = 'Builder takes 1-1-1-1-1 first move'
      TabOrder = 1
    end
    object tbSecondMove11111: TCheckBox
      Left = 20
      Top = 105
      Width = 235
      Height = 17
      Caption = 'Destroyer takes 1-1-1-1-1 first move'
      TabOrder = 2
    end
  end
  object gbResults: TGroupBox
    Left = 280
    Top = 5
    Width = 605
    Height = 215
    Caption = 'Results'
    TabOrder = 2
    object lblNumberOfGamesTitle: TLabel
      Left = 20
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
      Left = 200
      Top = 70
      Width = 7
      Height = 16
      Caption = '0'
    end
    object lblWinsForPlayerTitle: TLabel
      Left = 20
      Top = 87
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
      Left = 200
      Top = 87
      Width = 215
      Height = 16
      Caption = 'B = 1234 (52.56%); D = 1197 (47.44%)'
    end
    object lblWinByTypeTitle: TLabel
      Left = 20
      Top = 124
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
      Left = 200
      Top = 124
      Width = 21
      Height = 16
      Caption = '123'
    end
    object lblLineTitle: TLabel
      Left = 200
      Top = 107
      Width = 25
      Height = 16
      Caption = 'Line'
    end
    object lblDiagonalTitle: TLabel
      Left = 280
      Top = 107
      Width = 29
      Height = 16
      Caption = 'Diag'
    end
    object lblDiagonal: TLabel
      Left = 280
      Top = 124
      Width = 21
      Height = 16
      Caption = '234'
    end
    object lblBlockTitle: TLabel
      Left = 360
      Top = 107
      Width = 34
      Height = 16
      Caption = 'Block'
    end
    object lblBlock: TLabel
      Left = 360
      Top = 124
      Width = 21
      Height = 16
      Caption = '345'
    end
    object lblCountTitle: TLabel
      Left = 440
      Top = 107
      Width = 34
      Height = 16
      Caption = 'Count'
    end
    object lblCount: TLabel
      Left = 440
      Top = 124
      Width = 21
      Height = 16
      Caption = '456'
    end
    object lblDifferenceTitle: TLabel
      Left = 520
      Top = 107
      Width = 19
      Height = 16
      Caption = 'Diff'
    end
    object lblDifference: TLabel
      Left = 520
      Top = 124
      Width = 19
      Height = 16
      Caption = 'NA'
    end
    object lblGameLengthTitle: TLabel
      Left = 20
      Top = 141
      Width = 76
      Height = 16
      Caption = 'Game length'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblGameLength: TLabel
      Left = 200
      Top = 141
      Width = 99
      Height = 16
      Caption = 'Min = 5; Max = 72'
    end
    object lblTilesRequiredTitle: TLabel
      Left = 20
      Top = 178
      Width = 119
      Height = 16
      Caption = 'Tiles required (max)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblTile0Title: TLabel
      Left = 200
      Top = 161
      Width = 7
      Height = 16
      Caption = '0'
    end
    object lblTile1Title: TLabel
      Left = 240
      Top = 161
      Width = 7
      Height = 16
      Caption = '1'
    end
    object lblTile2Title: TLabel
      Left = 280
      Top = 161
      Width = 7
      Height = 16
      Caption = '2'
    end
    object lblTile3Title: TLabel
      Left = 320
      Top = 161
      Width = 7
      Height = 16
      Caption = '3'
    end
    object lblTitle4Title: TLabel
      Left = 360
      Top = 161
      Width = 7
      Height = 16
      Caption = '4'
    end
    object lblTitle5Title: TLabel
      Left = 400
      Top = 161
      Width = 7
      Height = 16
      Caption = '5'
    end
    object lblTitle6Title: TLabel
      Left = 440
      Top = 161
      Width = 7
      Height = 16
      Caption = '6'
    end
    object lblTitle7Title: TLabel
      Left = 480
      Top = 161
      Width = 7
      Height = 16
      Caption = '7'
    end
    object lblTitle8Title: TLabel
      Left = 520
      Top = 161
      Width = 7
      Height = 16
      Caption = '8'
    end
    object lblTile0: TLabel
      Left = 200
      Top = 178
      Width = 14
      Height = 16
      Caption = '12'
    end
    object lblTile1: TLabel
      Left = 240
      Top = 178
      Width = 7
      Height = 16
      Caption = '9'
    end
    object lblTile2: TLabel
      Left = 280
      Top = 178
      Width = 14
      Height = 16
      Caption = '14'
    end
    object lblTile3: TLabel
      Left = 320
      Top = 178
      Width = 14
      Height = 16
      Caption = '15'
    end
    object lblTile4: TLabel
      Left = 360
      Top = 178
      Width = 14
      Height = 16
      Caption = '36'
    end
    object lblTile5: TLabel
      Left = 400
      Top = 178
      Width = 14
      Height = 16
      Caption = '15'
    end
    object lblTile6: TLabel
      Left = 440
      Top = 178
      Width = 14
      Height = 16
      Caption = '14'
    end
    object lblTile7: TLabel
      Left = 480
      Top = 178
      Width = 7
      Height = 16
      Caption = '9'
    end
    object lblTile8: TLabel
      Left = 520
      Top = 178
      Width = 14
      Height = 16
      Caption = '12'
    end
    object btnStartAnalysis: TButton
      Left = 20
      Top = 25
      Width = 160
      Height = 40
      Caption = 'Start Analysis'
      TabOrder = 0
      OnClick = btnStartAnalysisClick
    end
    object btnStopAnalysis: TButton
      Left = 200
      Top = 25
      Width = 160
      Height = 40
      Caption = 'Stop Analysis'
      TabOrder = 1
      OnClick = btnStopAnalysisClick
    end
  end
  object UpdateTimer: TTimer
    Interval = 500
    OnTimer = OnUpdateTimerTick
    Left = 730
    Top = 225
  end
end

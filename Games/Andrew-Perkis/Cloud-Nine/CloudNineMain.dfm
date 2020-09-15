object frmCloudNineMain: TfrmCloudNineMain
  Left = 450
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Cloud Nine ('#169' Andrew Perkis, software '#169' Alain Dekker)'
  ClientHeight = 370
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnExit: TButton
    Left = 510
    Top = 320
    Width = 80
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
  object pageCloudNine: TPageControl
    Left = 10
    Top = 10
    Width = 580
    Height = 305
    ActivePage = AnalysisSheet
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabHeight = 35
    TabOrder = 0
    TabWidth = 180
    object GameSheet: TTabSheet
      Caption = 'Game'
      object gbGame: TGroupBox
        Left = 5
        Top = 5
        Width = 560
        Height = 250
        Caption = 'Game'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lblSource: TLabel
          Left = 15
          Top = 25
          Width = 43
          Height = 16
          Caption = 'Source'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ebSource: TEdit
          Left = 75
          Top = 20
          Width = 475
          Height = 24
          Color = clInactiveBorder
          Enabled = False
          TabOrder = 0
          Text = 'CloudNineAnalysis.txt'
        end
        object btnLoadSource: TButton
          Left = 16
          Top = 56
          Width = 80
          Height = 40
          Caption = 'Load'
          TabOrder = 1
        end
      end
    end
    object AnalysisSheet: TTabSheet
      Caption = 'Analysis'
      ImageIndex = 1
      object gbAnalysis: TGroupBox
        Left = 5
        Top = 5
        Width = 560
        Height = 250
        Caption = 'Analysis'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lblCandidatesReviewedTitle: TLabel
          Left = 15
          Top = 45
          Width = 135
          Height = 16
          Caption = 'Candidates (reviewed)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCandidatesReviewed: TLabel
          Left = 200
          Top = 45
          Width = 7
          Height = 16
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCompletedToursTitle: TLabel
          Left = 15
          Top = 85
          Width = 98
          Height = 16
          Caption = 'Completed tours'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCompletedTours: TLabel
          Left = 200
          Top = 85
          Width = 7
          Height = 16
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object shpAnalysis: TShape
          Left = 15
          Top = 160
          Width = 30
          Height = 30
          Brush.Color = clGray
        end
        object lblElapsed: TLabel
          Left = 200
          Top = 160
          Width = 48
          Height = 16
          Caption = '00:00:00'
          Visible = False
        end
        object lblCandidatesRejectedTitle: TLabel
          Left = 15
          Top = 65
          Width = 129
          Height = 16
          Caption = 'Candidates (rejected)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCandidatesRejected: TLabel
          Left = 200
          Top = 65
          Width = 7
          Height = 16
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblElapsedTitle: TLabel
          Left = 125
          Top = 160
          Width = 51
          Height = 16
          Caption = 'Elapsed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object lblBaseCandidatesTitle: TLabel
          Left = 15
          Top = 25
          Width = 102
          Height = 16
          Caption = 'Base candidates'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblBaseCandidates: TLabel
          Left = 200
          Top = 25
          Width = 7
          Height = 16
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblLastRouteAnalysed: TLabel
          Left = 60
          Top = 180
          Width = 13
          Height = 13
          Caption = 'A4'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object lblLastCompletedTour: TLabel
          Left = 60
          Top = 135
          Width = 41
          Height = 13
          Caption = 'none yet'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblClosedToursTitle: TLabel
          Left = 60
          Top = 105
          Width = 58
          Height = 13
          Caption = 'Closed tours'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblClosedTours: TLabel
          Left = 200
          Top = 105
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblOpenToursTitle: TLabel
          Left = 60
          Top = 120
          Width = 52
          Height = 13
          Caption = 'Open tours'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblOpenTours: TLabel
          Left = 200
          Top = 120
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object btnAnalyse: TButton
          Left = 15
          Top = 200
          Width = 80
          Height = 40
          Caption = 'Analyse'
          TabOrder = 0
          OnClick = btnAnalyseClick
        end
      end
    end
  end
  object AnalysisTimer: TTimer
    Interval = 10
    OnTimer = OnAnalysisTimerTick
    Left = 440
    Top = 320
  end
  object UpdateTimer: TTimer
    OnTimer = OnUpdateTimerTick
    Left = 475
    Top = 320
  end
end

object frmPuzzlesMain: TfrmPuzzlesMain
  Left = 337
  Top = 133
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Programming puzzles solved in Delphi'
  ClientHeight = 110
  ClientWidth = 355
  Color = clYellow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object gbPuzzle: TGroupBox
    Left = 10
    Top = 5
    Width = 335
    Height = 65
    Caption = 'Puzzle'
    TabOrder = 0
    object lblPuzzleNumber: TLabel
      Left = 15
      Top = 25
      Width = 101
      Height = 16
      Caption = 'Puzzle number'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ebPuzzleNumber: TEdit
      Left = 130
      Top = 20
      Width = 60
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '1'
    end
    object btnRunPuzzle: TButton
      Left = 200
      Top = 20
      Width = 120
      Height = 30
      Caption = 'Run puzzle...'
      TabOrder = 1
      OnClick = btnRunPuzzleClick
    end
  end
  object btnExit: TButton
    Left = 270
    Top = 75
    Width = 75
    Height = 30
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

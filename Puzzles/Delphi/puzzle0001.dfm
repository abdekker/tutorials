object frmPuzzle0001: TfrmPuzzle0001
  Left = 349
  Top = 202
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmPuzzle0001'
  ClientHeight = 590
  ClientWidth = 760
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
  object lblTarget: TLabel
    Left = 15
    Top = 20
    Width = 40
    Height = 16
    Caption = 'Target'
  end
  object btnExit: TButton
    Left = 610
    Top = 13
    Width = 140
    Height = 30
    Caption = 'Exit puzzle 0001'
    TabOrder = 0
    OnClick = btnExitClick
  end
  object btnStart: TButton
    Left = 145
    Top = 13
    Width = 75
    Height = 30
    Caption = 'Start !'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object ebTarget: TEdit
    Left = 80
    Top = 13
    Width = 60
    Height = 28
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '1000'
  end
  object stResults: TStaticText
    Left = 5
    Top = 50
    Width = 750
    Height = 535
    AutoSize = False
    BorderStyle = sbsSingle
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
end

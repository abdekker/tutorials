object frmSampleApplication: TfrmSampleApplication
  Left = 255
  Top = 130
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Sample Application for Testing'
  ClientHeight = 650
  ClientWidth = 850
  Color = clBtnFace
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
  object gbSettings: TGroupBox
    Left = 10
    Top = 5
    Width = 835
    Height = 600
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lblSample1: TLabel
      Left = 15
      Top = 85
      Width = 61
      Height = 16
      Caption = 'Variable 1'
    end
    object lblCategory: TLabel
      Left = 15
      Top = 25
      Width = 55
      Height = 16
      Caption = 'Category'
    end
    object lblAction: TLabel
      Left = 15
      Top = 55
      Width = 37
      Height = 16
      Caption = 'Action'
    end
    object lblSample2: TLabel
      Left = 15
      Top = 115
      Width = 61
      Height = 16
      Caption = 'Variable 2'
    end
    object lblSample3: TLabel
      Left = 15
      Top = 145
      Width = 61
      Height = 16
      Caption = 'Variable 3'
    end
    object ebSample1: TEdit
      Left = 135
      Top = 80
      Width = 400
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = 'sample text 1'
    end
    object btnProcess: TButton
      Left = 545
      Top = 45
      Width = 100
      Height = 35
      Caption = 'Process...'
      TabOrder = 1
      OnClick = btnProcessClick
    end
    object ddlCategory: TComboBox
      Left = 135
      Top = 20
      Width = 250
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
      TabOrder = 2
      OnChange = ddlCategoryChange
    end
    object ddlAction: TComboBox
      Left = 135
      Top = 50
      Width = 400
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
      TabOrder = 3
      OnClick = ddlActionClick
    end
    object listOutput: TListBox
      Left = 5
      Top = 175
      Width = 825
      Height = 420
      Color = clInfoBk
      ItemHeight = 16
      TabOrder = 4
    end
    object ebSample2: TEdit
      Left = 135
      Top = 110
      Width = 400
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      Text = 'sample text 2'
    end
    object ebSample3: TEdit
      Left = 135
      Top = 140
      Width = 400
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      Text = 'sample text 3'
    end
    object lblExplanationText: TStaticText
      Left = 545
      Top = 85
      Width = 280
      Height = 85
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = 'Explanation text...'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 7
      Visible = False
    end
  end
  object btnExit: TButton
    Left = 770
    Top = 610
    Width = 75
    Height = 35
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
  object btnClearOutput: TButton
    Left = 650
    Top = 610
    Width = 110
    Height = 35
    Caption = 'Clear Output'
    TabOrder = 2
    OnClick = btnClearOutputClick
  end
  object UpdateTimer: TTimer
    Interval = 100
    OnTimer = OnUpdateTimer
    Left = 10
    Top = 610
  end
end

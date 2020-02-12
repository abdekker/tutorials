object frmDelphiMain: TfrmDelphiMain
  Left = 443
  Top = 228
  BorderStyle = bsDialog
  Caption = 'Delphi application to use external DLL'
  ClientHeight = 247
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object gbSettings: TGroupBox
    Left = 10
    Top = 10
    Width = 400
    Height = 185
    Caption = 'Enter two integers'
    TabOrder = 0
    object lblNumberOne: TLabel
      Left = 15
      Top = 30
      Width = 58
      Height = 16
      Caption = 'Number 1'
    end
    object lblNumberTwo: TLabel
      Left = 205
      Top = 30
      Width = 58
      Height = 16
      Caption = 'Number 2'
    end
    object ebNumberOne: TEdit
      Left = 100
      Top = 25
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
      Text = '23'
    end
    object ebNumberTwo: TEdit
      Left = 290
      Top = 25
      Width = 60
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '75'
    end
    object btnUseInternalMethods: TButton
      Left = 15
      Top = 60
      Width = 180
      Height = 40
      Caption = 'Internal functions'
      TabOrder = 2
      OnClick = btnUseInternalMethodsClick
    end
    object btnUseExternalMethods: TButton
      Left = 205
      Top = 60
      Width = 180
      Height = 40
      Caption = 'External (DLL) functions'
      TabOrder = 3
      OnClick = btnUseExternalMethodsClick
    end
    object lblOutput: TStaticText
      Left = 10
      Top = 115
      Width = 380
      Height = 60
      AutoSize = False
      BevelKind = bkFlat
      BorderStyle = sbsSingle
      Caption = 'Click one of the buttons to compare the integers...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object btnExit: TButton
    Left = 335
    Top = 200
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

object frmDelphiClient: TfrmDelphiClient
  Left = 360
  Top = 118
  BorderStyle = bsDialog
  Caption = 'Delphi app using an external DLL written in Visual Studio 2019'
  ClientHeight = 585
  ClientWidth = 510
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
    Width = 490
    Height = 530
    TabOrder = 0
    object lblFunctionMethod: TLabel
      Left = 15
      Top = 55
      Width = 98
      Height = 16
      Caption = 'Function method'
    end
    object lblFibonacciBitDepth: TLabel
      Left = 15
      Top = 85
      Width = 113
      Height = 16
      Caption = 'Fibonacci bit depth'
    end
    object lblFunctionClass: TLabel
      Left = 16
      Top = 25
      Width = 107
      Height = 16
      Caption = 'DLL function class'
    end
    object lblDllNameTitle: TLabel
      Left = 365
      Top = 115
      Width = 115
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'DLL name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDllName: TLabel
      Left = 365
      Top = 130
      Width = 115
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'SimpleDLL.dll'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btnUseFunctionsFromDLL: TButton
      Left = 10
      Top = 110
      Width = 350
      Height = 40
      Caption = 'Use Methods from DLL'
      TabOrder = 0
      OnClick = btnUseFunctionsFromDLLClick
    end
    object memoOutput: TMemo
      Left = 5
      Top = 155
      Width = 480
      Height = 370
      Color = clInfoBk
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object ddlFunctionMethod: TComboBox
      Left = 150
      Top = 50
      Width = 325
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
      OnChange = OnControlChange
    end
    object rbFibonacciBitDepth32: TRadioButton
      Left = 150
      Top = 80
      Width = 45
      Height = 30
      Caption = '32'
      TabOrder = 3
      OnClick = OnControlChange
    end
    object rbFibonacciBitDepth64: TRadioButton
      Left = 200
      Top = 80
      Width = 45
      Height = 30
      Caption = '64'
      TabOrder = 4
      OnClick = OnControlChange
    end
    object ddlFunctionClass: TComboBox
      Left = 151
      Top = 20
      Width = 325
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
      TabOrder = 5
      OnChange = OnControlChange
    end
  end
  object btnExit: TButton
    Left = 425
    Top = 540
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

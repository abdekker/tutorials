object frmDelphiClient: TfrmDelphiClient
  Left = 360
  Top = 118
  BorderStyle = bsDialog
  Caption = 'Delphi app using an external DLL written in Visual Studio 2019'
  ClientHeight = 550
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
    Height = 495
    TabOrder = 0
    object lblFunctionMethod: TLabel
      Left = 15
      Top = 25
      Width = 98
      Height = 16
      Caption = 'Function method'
    end
    object lblFibonacciBitDepth: TLabel
      Left = 15
      Top = 60
      Width = 113
      Height = 16
      Caption = 'Fibonacci bit depth'
    end
    object btnUseTestMethods: TButton
      Left = 10
      Top = 90
      Width = 180
      Height = 40
      Caption = 'Test Methods'
      TabOrder = 0
      OnClick = btnUseTestMethodsClick
    end
    object btnUseFibonacciMethods: TButton
      Left = 200
      Top = 90
      Width = 220
      Height = 40
      Caption = 'Fibonacci (32-bit) methods'
      TabOrder = 1
      OnClick = btnUseFibonacciMethodsClick
    end
    object memoOutput: TMemo
      Left = 5
      Top = 140
      Width = 480
      Height = 350
      Color = clInfoBk
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object ddlFunctionMethod: TComboBox
      Left = 150
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
      TabOrder = 3
    end
    object rbFibonacciBitDepth32: TRadioButton
      Left = 150
      Top = 55
      Width = 45
      Height = 30
      Caption = '32'
      TabOrder = 4
      OnClick = rbFibonacciBitDepthClick
    end
    object rbFibonacciBitDepth64: TRadioButton
      Left = 200
      Top = 55
      Width = 45
      Height = 30
      Caption = '64'
      TabOrder = 5
      OnClick = rbFibonacciBitDepthClick
    end
  end
  object btnExit: TButton
    Left = 425
    Top = 505
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

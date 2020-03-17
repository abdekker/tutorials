object frmDelphiClient: TfrmDelphiClient
  Left = 443
  Top = 228
  BorderStyle = bsDialog
  Caption = 
    'Delphi app which uses an external DLL written in Visual Studio 2' +
    '019'
  ClientHeight = 425
  ClientWidth = 510
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
    Top = 5
    Width = 490
    Height = 370
    TabOrder = 0
    object btnUseTestMethods: TButton
      Left = 10
      Top = 20
      Width = 180
      Height = 40
      Caption = 'Test Methods'
      TabOrder = 0
      OnClick = btnUseTestMethodsClick
    end
    object btnUseFibonacciMethods: TButton
      Left = 200
      Top = 20
      Width = 180
      Height = 40
      Caption = 'Fibonacci (32-bit)'
      TabOrder = 1
      OnClick = btnUseFibonacciMethodsClick
    end
    object memoOutput: TMemo
      Left = 10
      Top = 70
      Width = 470
      Height = 291
      Color = clInfoBk
      ScrollBars = ssVertical
      TabOrder = 2
    end
  end
  object btnExit: TButton
    Left = 425
    Top = 380
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

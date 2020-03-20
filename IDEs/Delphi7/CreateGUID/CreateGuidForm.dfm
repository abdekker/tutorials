object frmCreateGUID: TfrmCreateGUID
  Left = 356
  Top = 147
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Create GUID in Delphi'
  ClientHeight = 200
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 20
  object gbGUID: TGroupBox
    Left = 10
    Top = 5
    Width = 410
    Height = 140
    TabOrder = 0
    object lblCopiedToClipboard: TLabel
      Left = 105
      Top = 72
      Width = 200
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '(and copy to the Windows clipboard)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btnCreateGUID: TButton
      Left = 125
      Top = 20
      Width = 160
      Height = 50
      Caption = 'Create GUID!'
      TabOrder = 0
      OnClick = btnCreateGUIDClick
    end
    object pnlGUID: TPanel
      Left = 10
      Top = 90
      Width = 390
      Height = 40
      Color = clInfoBk
      TabOrder = 1
    end
  end
  object btnExit: TButton
    Left = 340
    Top = 150
    Width = 80
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

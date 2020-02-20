object frmConditional: TfrmConditional
  Left = 366
  Top = 134
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Conditional compilation in Delphi'
  ClientHeight = 200
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbOutput: TGroupBox
    Left = 5
    Top = 5
    Width = 370
    Height = 145
    TabOrder = 0
    object lblOutput1: TLabel
      Left = 15
      Top = 37
      Width = 190
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Both symbols defined (using $IF)'
    end
    object Label1: TLabel
      Left = 15
      Top = 58
      Width = 190
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Both symbols defined (using $IFDEF)'
    end
    object Label2: TLabel
      Left = 15
      Top = 79
      Width = 190
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Only 1st symbol defined (using $IF)'
    end
    object Label3: TLabel
      Left = 15
      Top = 100
      Width = 190
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Only 2nd symbol defined (using $IF)'
    end
    object Label4: TLabel
      Left = 15
      Top = 121
      Width = 190
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Neither symbol defined (using $IF)'
    end
    object gridDefined: TStringGrid
      Left = 210
      Top = 10
      Width = 157
      Height = 130
      ColCount = 3
      DefaultColWidth = 50
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnDrawCell = gridDefinedDrawCell
    end
  end
  object btnExit: TButton
    Left = 300
    Top = 155
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end

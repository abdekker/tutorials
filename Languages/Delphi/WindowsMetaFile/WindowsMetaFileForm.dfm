object frmWindowsMetaFile: TfrmWindowsMetaFile
  Left = 433
  Top = 284
  Width = 516
  Height = 319
  BorderIcons = [biSystemMenu]
  Caption = 'Windows Metal File Loader'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object gbImageFile: TGroupBox
    Left = 5
    Top = 5
    Width = 490
    Height = 270
    TabOrder = 0
    object lblImageFile: TLabel
      Left = 15
      Top = 25
      Width = 58
      Height = 16
      Caption = 'Image file'
    end
    object lblDiagResizeEvents: TLabel
      Left = 340
      Top = 52
      Width = 79
      Height = 13
      Caption = 'Resize events: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object ebImageFile: TEdit
      Left = 90
      Top = 21
      Width = 330
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 430
      Top = 20
      Width = 45
      Height = 30
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object tbShowBorder: TCheckBox
      Left = 15
      Top = 55
      Width = 65
      Height = 17
      Caption = 'Border'
      TabOrder = 2
      OnClick = tbShowBorderClick
    end
    object tbFullScreen: TCheckBox
      Left = 90
      Top = 55
      Width = 90
      Height = 17
      Caption = 'Full screen'
      TabOrder = 3
      OnClick = tbFullScreenClick
    end
    object btnSaveImage: TButton
      Left = 190
      Top = 52
      Width = 75
      Height = 22
      Caption = 'Save'
      TabOrder = 4
      OnClick = btnSaveImageClick
    end
  end
end

object frmSampleApplication: TfrmSampleApplication
  Left = 404
  Top = 159
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Sample Application for Testing'
  ClientHeight = 645
  ClientWidth = 875
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object gbSettings: TGroupBox
    Left = 10
    Top = -5
    Width = 865
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
      Left = 150
      Top = 80
      Width = 410
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = 'sample text 1'
    end
    object ddlCategory: TComboBox
      Left = 150
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
      TabOrder = 0
      OnChange = ddlCategoryChange
    end
    object ddlAction: TComboBox
      Left = 150
      Top = 50
      Width = 410
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
      TabOrder = 1
      OnClick = ddlActionClick
    end
    object listOutput: TListBox
      Left = 5
      Top = 175
      Width = 556
      Height = 420
      Color = clInfoBk
      ItemHeight = 16
      TabOrder = 7
    end
    object ebSample2: TEdit
      Left = 150
      Top = 110
      Width = 410
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = 'sample text 2'
    end
    object ebSample3: TEdit
      Left = 150
      Top = 140
      Width = 410
      Height = 28
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      Text = 'sample text 3'
    end
    object lblExplanationText: TStaticText
      Left = 570
      Top = 85
      Width = 285
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
      TabOrder = 8
      Visible = False
    end
    object gbSampleControls: TGroupBox
      Left = 570
      Top = 180
      Width = 285
      Height = 410
      Caption = 'Sample controls (Controls category only)'
      TabOrder = 9
      Visible = False
      object lblChildControls: TLabel
        Left = 15
        Top = 25
        Width = 255
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'There are N child controls in this group'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblSingleLine: TLabel
        Left = 15
        Top = 140
        Width = 84
        Height = 16
        Caption = 'label (TLabel)'
      end
      object lblMultiLine: TLabel
        Left = 15
        Top = 165
        Width = 255
        Height = 64
        AutoSize = False
        Caption = 'multi-line label (TLabel with WordWrap set to True)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object ebEditBox: TEdit
        Left = 15
        Top = 50
        Width = 255
        Height = 28
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'edit box (TEdit)'
      end
      object cbComboBox: TComboBox
        Left = 15
        Top = 81
        Width = 255
        Height = 28
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'combo box'
        OnClick = ddlActionClick
        Items.Strings = (
          'combo box')
      end
      object lblStatic: TStaticText
        Left = 15
        Top = 115
        Width = 113
        Height = 20
        BorderStyle = sbsSingle
        Caption = 'label (TStaticText)'
        TabOrder = 2
      end
    end
    object btnProcess: TBitBtn
      Left = 570
      Top = 24
      Width = 120
      Height = 48
      Caption = 'Process...'
      TabOrder = 5
      OnClick = btnProcessClick
      Glyph.Data = {
        360C0000424D360C000000000000360000002800000020000000200000000100
        180000000000000C000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FF866465D0B0AAFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFA99192A78C8CA88E8EA88E8EA78C8CA7
        8C8CA88E8EA78C8CA78C8CA88E8EA88D8DA88D8DB39898FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FF5F3D3E937472C8B7B89D86867452507C5A5CFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFB3989899989796948896948899948B9A968D9D
        958D9D958D9D958D9A968D99948B9694889694889A9697B39898FF00FFFF00FF
        A39B95916D6E957F7EE1D7D8FDFCFCFCEEEFFCEAEBFCEFEFBBA7A7886566653B
        3DFF00FFFF00FFFF00FFFF00FFA59393969488CABD55CAC56AC9BD54C9BD54C8
        C368C4BC57C8C368C9BD54C9BD54CAC56ACABD55969488A59393FF00FF927375
        A28889EBE5E5FDFCFCFDFCFCFDFCFCEFDFDFEFD9D9EFD9D9FCDEDEF0E7E7AD94
        95856768FF00FFFF00FFFF00FFA5939394948FD8D291E0D5A4D4CD87D4CD87E0
        D5A4D7CF84C3BEA47484B5A4A993E4D9A7D8D29194948FA59393FF00FF7C5A5C
        E2D4D4FEF6F6FEF6F6FEF8F8FDF4F4DECBCBDFCDCDE1D1D1E7D0D1E8D5D59070
        6DFF00FFFF00FFFF00FFFF00FFA5939395948CD9D183DED799D5CE7FD5CE7FDD
        D6989C97918A8CB5D6D085989BA56074ADDBD28595948CA59393FF00FF7E6267
        D8C5C6FCE9E9FCE9E9FCE9E9FCE4E5D5BEBCD8C2C0DAC3C1DAC5C3EBD2D28E6A
        6CFF00FFFF00FFFF00FFFF00FFA5939394948FE0D99AE8E0B8DCD596DCD596D9
        D2B87578A6EBE2BADCD596DCD596BABDC28A8DB398928EA59393FF00FF896E76
        CDB2B1EFDBDBEFD9D9EFD9D9E7D2D2D4B3B2D5B4B5E0C2C0CAB1AFAF8A8C724F
        507952548766654A24234A2724978081888A90C7C294EBE2BADCD596E2D9A16E
        72BED9D08EE8E0B8DCD596DCD596E8E0B8888E938D888FA59393FF00FFA28784
        B59A9CDFCDCDDECACADECACADCC6C5CEAEAD957272805D5BA99191DFD2D2C7BC
        B995787896817DB6A2A4C4B3B5AE9B9C9B98916875979398B78E93A16974AEDF
        D3A0D3CC7FE5DDA0DCD48CDCD48CEAE1A4DCD48C8B8A8CA59393FF00FFAF9A9B
        AC8D8ED9C4C2D7C1C0DBC5C3A88F8D865D5CDDBCBDFDE4E3E0CBCC7E6360BBA9
        A8FDF6F6FCF1F1FCE9E9FBE6E7D0BCBC94948FEAE1A4E2DABCCBC3AAEAE1A4F8
        EDC4D9D08CF5EBC2E9E0A6E9E0A6F8EDC4E9E0A694948FA59393FF00FFD7BEBD
        A37E7ED3B5B5D4B5B6876365BDA1A0FDDEDFFDD6D3BB97949D8182FDF6F6FEF2
        F2FDF1F1FDF1F1FCEDEDFCE9E9D0BCBC9B9694DDD58AEAE1A4DDD58ADDD58AE8
        DFA2D3CC7FE8DFA2DDD58ADDD58AEAE1A4DDD58A9B9694A59393FF00FFFF00FF
        967271D4B5B1845D5CDDCCCCFCDCDDFCDADAAC8B8BC4AEAEFDFCFCFEF6F6FEF4
        F4FEF4F4FEF2F2FCEDEDFCE9E9D7BEBE9A96979D9890A19D999F9693A19999A4
        9F9EA19292A49F9EA199999F9693A19D999D98909A9697B39898FF00FFFF00FF
        8D6C6D886163DCCCCDFBE4E5FCE4E5AD9091CAB5B5FDFCFCFDFCFCFEF4F4FEF4
        F4F6EEEED8C5C5ECDBDBFCE8E9FBE4E5DDC1C2DEC1C1DFBDBCDEBFBEAF8C8DC1
        9D9CC19D9CC19D9CBE9E9EC19D9CC19D9CC19D9CBF9E9EFF00FFFF00FFFF00FF
        704B4BBCA7A7FCEDEDFCEAEBC9ACACBAA4A4FDFCFCFDFCFCFDFCFCFEF4F4FEF8
        F8A583847E56578B6868F1E1E1FBE4E5FBE4E5FBE4E5FCDCDCFCE0E1967576FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        835F61FDF4F4FDEFEFE7DEDE92777AFDFAFAFEF6F6FEF6F6FEF6F6FEF4F4FEF8
        F8724E4DE4CDCBDCC4C4AB8E8FFCECEDFBE4E5FBE2E3FCDCDCFCDCDC9C7C7CFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAD8E91
        BFABABFDFAFAFEF8F88C6E6CF7F0F0FEF4F4FEF4F4FEF4F4FEF4F4FEF4F4FDF4
        F4CDB9B9A08082FCE6E79E8081EBD9DAFBE4E5FBE2E3FCDCDCFCDCDC9E7F80FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF967374
        ECE0E1FDFAFAC4B4B3C1AFAEFEF2F2FEF4F4FEF4F4FEF4F4FEF4F4FEF2F2FDEF
        EFFCF1F1A98A8CE0CCCEE5CDCDA98889FCEAEBFCDCDCFCDCDCFCDCDC9D7E7EFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF99807B
        FEF8F8FDFCFC876563FDF5F5FCEDEDFCEDEDFDEFEFFDF1F1FDEFEFFCEDEDFCED
        EDFCEBEBF6E8E8917172FDF1F1A38384E1D1D1FCDCDCFCDADAFCDCDD957272FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA3878A
        FEF8F8CDBCBCB8AAA9FCE9E9FCEDEDFCEDEDFDEFEFFCEDEDFCEDEDFCEBEBFCE9
        E9FBE6E7FCE8E9BEA8A9CAB1B2E8DADB9B7D7EFCE0E1FCDADAFCDEDE917374FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB09494
        FEF8F8977B79FCEAEBFBE6E7FCE9E9EED7D6C9B3B4F4E6E5FDEFEFFBE6E7FBE4
        E5FBE4E5FBE4E5F6E7E7937374F5EDEDA68789E0C9C9FDD4D5F2D2D2552C30FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB19494
        FDF4F4987676FBE6E7FBE4E5F2DFDF785252AF8E8F937374C0A8A9F6E8E9FBE6
        E7FBE4E5FBE4E5FBE2E3D6BEBEB49698F3EAEA967777FCDADBB0939384676DFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAE8E8F
        DBCBCABC9D9FFBE4E5FBE4E5E1C9CA7D5455F4E2E2FBE2E3C7AFAD907172C4AE
        B0FDEAEAFCE0E1FCDCDCFCE2E39D8081EBDFE0A99091E1CACA846461FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9F7F80
        B7A2A3DBC1C0FCE0E1FCE0E1FCE0E1B2908E876665B29799F5E2E3F6E7E7C5AB
        AA8D6B6DCEB6B7FDE6E6FCDADAEDD3D19A7C7CF7F0F07959576F4D4CFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF97797B
        B08D8EE8D0D0FCDCDCFCDCDCFCDCDCFCE0E1FBE4E5D5BCBC907072A28A89F5E8
        E8F7EBEBBFA6A7886966D3BCBDFDE2E3AD9192D6C7C7AF9B9B765052FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9F7C7E
        9C7B7CE9CECDFDD4D5FCDADAFCDCDCFCDCDCFCDCDCFCDCDCFCE2E3E6CDCC8E72
        72907573ECE3E3FDF4F4B19B9B8B6566E1CDCE8A6E69FDFCFC684341FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF937779
        7E5859E9C8C9FDD4D5FDD4D5FDD6D7FDD4D5FDD4D5FDD4D5FDD6D7FDD6D7FDDA
        DBF1D9D9997E7E937373DACECEFDF6F6A38C8B5D3434B39E9CB6A6A65A2D2DFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        654340D4B6B5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4
        D5FDD4D3FDD6D3FDDEDBAD8E8D917070C5B1B2EFE6E6968081F5EFEF8C6E6DFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        95777AA17C7CFDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4D5FDD4
        D3FDD3CFFDD3CDFDD3CDF5D3D19F8082764E517C5D5FB0989AEBDFDFCBB5B5A2
        86888B7C70BDA7A8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        E2D5D56F504BFDD6D3FDD3CDFDD3CDFDD3CFFDD3CDFDD3CDFDD3CDFDD3CDFDD3
        CDFDD3CDFDD3CDF2D0CF9475787C5A5CFF00FFFF00FF9C817C8B6A6C9C808261
        5619E4A4086A5826D3C3C4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFA48889977979FDDDD9FDD3CDFDD3CDFDD3CDFDD3CDFDD3CDFDD3CDFDD3
        CDFDDED6E2CDC89371718B6C71FF00FFFF00FFFF00FFFF00FFFF00FFAD90908B
        7414E4A408A67B34C4B0AEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFA992919E8580FCF0F1FCE8DDFDE8DDFDE6E6FCE2D7FCF1E1F4EB
        E1A18483866263DCC8C7FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE2D5D578
        6461966A16907C84E2D5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFAF98998A6B699C8986BFACADC1AEAFA48F8E957877795A
        57CCBBBBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE2
        D5D5E2D5D5E2D5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFDFCCCCA9918FA28784E5C9CAFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    end
    object btnBrowse: TButton
      Left = 700
      Top = 24
      Width = 100
      Height = 48
      Caption = 'Browse...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      Visible = False
      OnClick = btnBrowseClick
    end
  end
  object btnExit: TButton
    Left = 795
    Top = 600
    Width = 75
    Height = 40
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
  object btnClearOutput: TButton
    Left = 5
    Top = 600
    Width = 110
    Height = 40
    Caption = 'Clear Output'
    TabOrder = 2
    OnClick = btnClearOutputClick
  end
  object UpdateTimer: TTimer
    Interval = 100
    OnTimer = OnUpdateTimer
    Left = 760
    Top = 600
  end
end

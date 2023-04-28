object Form1: TForm1
  Left = 196
  Top = 97
  Width = 696
  Height = 464
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 18
  object VBox: TPaintBox
    Left = 8
    Top = 8
    Width = 321
    Height = 388
    OnPaint = VBoxPaint
  end
  object Box1: TListBox
    Left = 337
    Top = 8
    Width = 337
    Height = 217
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 0
    OnClick = Box1Click
  end
  object Edit1: TEdit
    Left = 553
    Top = 232
    Width = 121
    Height = 26
    TabOrder = 1
    Text = 'Edit1'
  end
  object BDelete: TButton
    Left = 337
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = BDeleteClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 407
    Width = 680
    Height = 19
    Panels = <>
  end
  object BCrcCalc: TButton
    Left = 433
    Top = 232
    Width = 113
    Height = 25
    Caption = 'Crc Calc'
    TabOrder = 4
    OnClick = BCrcCalcClick
  end
  object GroupBox1: TGroupBox
    Left = 337
    Top = 280
    Width = 281
    Height = 113
    Caption = 'Operations:'
    TabOrder = 5
    object BClose: TButton
      Left = 166
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = BCloseClick
    end
    object BLoad: TButton
      Left = 86
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = BLoadClick
    end
    object Binit: TButton
      Left = 6
      Top = 32
      Width = 75
      Height = 25
      Caption = 'New'
      TabOrder = 2
      OnClick = BinitClick
    end
    object BAdd: TButton
      Left = 6
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 3
      OnClick = BmemClick
    end
    object BCompact: TButton
      Left = 88
      Top = 72
      Width = 97
      Height = 25
      Caption = 'Compact'
      TabOrder = 4
      OnClick = BCompactClick
    end
  end
end

object Form1: TForm1
  Left = 191
  Top = 102
  Width = 789
  Height = 442
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
  object Edit1: TEdit
    Left = 16
    Top = 8
    Width = 273
    Height = 26
    TabOrder = 0
    Text = ':04001E00FF010000DE'
  end
  object Button2: TButton
    Left = 296
    Top = 8
    Width = 89
    Height = 25
    Caption = 'checksum'
    TabOrder = 1
    OnClick = Button2Click
  end
  object BParse: TButton
    Left = 128
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 2
    OnClick = BParseClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 48
    Width = 369
    Height = 297
    Caption = 'input Hex text '
    TabOrder = 3
    object Memo1: TMemo
      Left = 2
      Top = 20
      Width = 365
      Height = 275
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object GroupBox2: TGroupBox
    Left = 392
    Top = 56
    Width = 337
    Height = 289
    Caption = 'Output pic16 opcodes listing'
    TabOrder = 4
    object Memo2: TMemo
      Left = 2
      Top = 20
      Width = 333
      Height = 267
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 385
    Width = 773
    Height = 19
    Panels = <>
  end
end

object Form1: TForm1
  Left = 307
  Top = 156
  Width = 386
  Height = 238
  BorderIcons = [biSystemMenu, biMinimize]
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar1: TStatusBar
    Left = 0
    Top = 181
    Width = 370
    Height = 19
    Panels = <>
  end
  object BStart: TButton
    Left = 80
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = BStartClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 57
    Caption = 'Programmer'
    TabOrder = 2
    object VirtualPort: TEdit
      Left = 24
      Top = 16
      Width = 121
      Height = 24
      TabOrder = 0
      Text = 'COM4'
    end
    object VirBaud: TComboBox
      Left = 176
      Top = 16
      Width = 145
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      Text = '9600'
      Items.Strings = (
        '300'
        '600'
        '1200'
        '2400'
        '4800'
        '9600'
        '14400'
        '19200'
        '28800'
        '31250'
        '38400'
        '56000'
        '57600'
        '115200'
        '128000'
        '256000')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 72
    Width = 337
    Height = 49
    Caption = 'Pic'
    TabOrder = 3
    object PhyPort: TEdit
      Left = 24
      Top = 16
      Width = 121
      Height = 24
      TabOrder = 0
      Text = 'COM2'
    end
    object PhyBaud: TComboBox
      Left = 176
      Top = 16
      Width = 145
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      Text = '9600'
      Items.Strings = (
        '300'
        '600'
        '1200'
        '2400'
        '4800'
        '9600'
        '14400'
        '19200'
        '28800'
        '31250'
        '38400'
        '56000'
        '57600'
        '115200'
        '128000'
        '256000')
    end
  end
  object BCancel: TButton
    Left = 160
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 4
    OnClick = BCancelClick
  end
  object ActionList1: TActionList
    Left = 248
    Top = 112
    object Action1: TAction
      Caption = 'Action1'
      ShortCut = 46
    end
  end
end

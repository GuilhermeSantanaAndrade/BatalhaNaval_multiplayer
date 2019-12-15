object frmConfig: TfrmConfig
  Left = 378
  Top = 231
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frmConfig'
  ClientHeight = 128
  ClientWidth = 274
  Color = 16312248
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 274
    Height = 128
    Align = alClient
    Brush.Color = 16312248
  end
  object Label1: TLabel
    Left = 3
    Top = 46
    Width = 65
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'IP Servidor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 3
    Top = 70
    Width = 65
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Porta'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 3
    Top = 22
    Width = 65
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Nome'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 1
    Top = 1
    Width = 272
    Height = 18
    Align = alCustom
    AutoSize = False
    Caption = ' Configura'#231#245'es'
    Color = clAqua
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object edtServidor: TEdit
    Left = 73
    Top = 46
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object edtPorta: TEdit
    Left = 73
    Top = 70
    Width = 65
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 48
    Top = 98
    Width = 75
    Height = 25
    Caption = 'Confirmar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 98
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button2Click
  end
  object edtNome: TEdit
    Left = 73
    Top = 22
    Width = 193
    Height = 21
    TabOrder = 4
  end
end

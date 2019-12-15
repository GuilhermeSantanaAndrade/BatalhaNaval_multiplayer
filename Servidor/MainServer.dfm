object frmServer: TfrmServer
  Left = 325
  Top = 164
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Servidor - Batalha Naval'
  ClientHeight = 496
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 44
    Width = 25
    Height = 13
    Caption = 'Porta'
  end
  object lblTitulo: TLabel
    Left = 0
    Top = 0
    Width = 435
    Height = 35
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Servidor'
    Color = clSilver
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 3
    Top = 181
    Width = 208
    Height = 14
    Alignment = taCenter
    AutoSize = False
    Caption = 'Equipe A'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 219
    Top = 181
    Width = 208
    Height = 14
    Alignment = taCenter
    AutoSize = False
    Caption = 'Equipe B'
    Color = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 0
    Top = 311
    Width = 435
    Height = 14
    Align = alBottom
    AutoSize = False
    Caption = 'Log'
    Color = clSilver
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label5: TLabel
    Left = 3
    Top = 69
    Width = 424
    Height = 14
    Alignment = taCenter
    AutoSize = False
    Caption = 'Jogadores Sem Equipe'
    Color = clSilver
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object edtPort: TEdit
    Left = 40
    Top = 41
    Width = 41
    Height = 21
    TabOrder = 0
    Text = '8110'
  end
  object btnIniciar: TButton
    Left = 334
    Top = 40
    Width = 96
    Height = 25
    Caption = 'Iniciar Servidor'
    TabOrder = 1
    OnClick = btnIniciarClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 325
    Width = 435
    Height = 171
    Align = alBottom
    TabOrder = 2
  end
  object lstTeamA: TListBox
    Left = 3
    Top = 195
    Width = 209
    Height = 113
    ItemHeight = 13
    TabOrder = 3
  end
  object lstTeamB: TListBox
    Left = 219
    Top = 195
    Width = 209
    Height = 113
    ItemHeight = 13
    TabOrder = 4
  end
  object lstJogadores: TListBox
    Left = 3
    Top = 83
    Width = 425
    Height = 95
    ItemHeight = 13
    TabOrder = 5
  end
  object btnIniciarPartida: TButton
    Left = 200
    Top = 40
    Width = 105
    Height = 25
    Caption = 'Iniciar partida'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnIniciarPartidaClick
  end
end

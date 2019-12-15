object frmBatalha: TfrmBatalha
  Left = 54
  Top = 23
  Width = 963
  Height = 652
  Caption = 'frmBatalha'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitulo: TLabel
    Left = 0
    Top = 0
    Width = 947
    Height = 35
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'lblTitulo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object grpBottom: TGroupBox
    Left = 9
    Top = 480
    Width = 937
    Height = 121
    TabOrder = 4
    object Label10: TLabel
      Left = 812
      Top = 11
      Width = 62
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Seu Nome:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label11: TLabel
      Left = 812
      Top = 35
      Width = 62
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'IP:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSeuNome: TLabel
      Left = 874
      Top = 11
      Width = 53
      Height = 13
      AutoSize = False
      Caption = '________'
    end
    object lblSeuIP: TLabel
      Left = 874
      Top = 35
      Width = 53
      Height = 13
      AutoSize = False
      Caption = '________'
    end
    object GroupBox1: TGroupBox
      Left = 198
      Top = 0
      Width = 211
      Height = 121
      Caption = ' Conex'#227'o com o Servidor '
      TabOrder = 0
      object Label9: TLabel
        Left = 11
        Top = 19
        Width = 69
        Height = 20
        Caption = 'Endere'#231'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblIPServer: TLabel
        Left = 107
        Top = 19
        Width = 99
        Height = 20
        AutoSize = False
        Caption = '127.0.0.1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStatusServer: TLabel
        Left = 107
        Top = 43
        Width = 99
        Height = 20
        AutoSize = False
        Caption = 'Desconectado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label12: TLabel
        Left = 11
        Top = 43
        Width = 47
        Height = 20
        Caption = 'Status'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object btnConectar: TSpeedButton
        Left = 7
        Top = 87
        Width = 186
        Height = 25
        Caption = 'Conectar'
        OnClick = btnConectarClick
      end
      object rgModoNormal: TRadioButton
        Left = 11
        Top = 68
        Width = 85
        Height = 17
        Caption = 'Modo Normal'
        TabOrder = 0
        OnClick = rbNaoClick
      end
      object rgModoTestes: TRadioButton
        Left = 101
        Top = 69
        Width = 92
        Height = 17
        Caption = 'Modo testes'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rbSimClick
      end
    end
    object grpOpcoesPartida: TGroupBox
      Left = 407
      Top = 0
      Width = 208
      Height = 121
      Caption = ' Op'#231#227'o da partida '
      TabOrder = 1
      Visible = False
      object btnComecar: TSpeedButton
        Left = 6
        Top = 22
        Width = 186
        Height = 25
        Caption = 'Entrar em partida'
        OnClick = btnComecarClick
      end
      object grpDisparos: TGroupBox
        Left = 0
        Top = 66
        Width = 208
        Height = 55
        Caption = ' Permitir Disparar livremente (para testes)'
        TabOrder = 0
        Visible = False
        object rbNao: TRadioButton
          Left = 33
          Top = 28
          Width = 41
          Height = 17
          Caption = 'N'#227'o'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbNaoClick
        end
        object rbSim: TRadioButton
          Left = 101
          Top = 29
          Width = 41
          Height = 17
          Caption = 'Sim'
          TabOrder = 1
          OnClick = rbSimClick
        end
      end
    end
    object grpTestes: TGroupBox
      Left = 613
      Top = 0
      Width = 198
      Height = 121
      Caption = ' Bot'#245'es para Teste '
      TabOrder = 2
      Visible = False
      object Button5: TButton
        Left = 12
        Top = 88
        Width = 178
        Height = 25
        Caption = 'Adicionar Jogador ao Time B'
        TabOrder = 0
        OnClick = Button5Click
      end
      object Button4: TButton
        Left = 11
        Top = 59
        Width = 179
        Height = 25
        Caption = 'Adicionar Jogador ao Team A'
        TabOrder = 1
        OnClick = Button4Click
      end
      object btnDistribuirBarcos: TButton
        Left = 10
        Top = 30
        Width = 180
        Height = 25
        Caption = 'Distribuir Barcos Aleatoriamente'
        TabOrder = 2
        OnClick = btnDistribuirBarcosClick
      end
    end
  end
  object pnlFundo: TPanel
    Left = 0
    Top = 35
    Width = 947
    Height = 313
    Align = alTop
    Color = 14145495
    TabOrder = 0
    object pnlTEAM_A: TPanel
      Left = 1
      Top = 1
      Width = 432
      Height = 311
      Align = alLeft
      Color = 14145495
      TabOrder = 0
      Visible = False
      OnResize = pnlTEAM_AResize
    end
    object pnlTEAM_B: TPanel
      Left = 514
      Top = 1
      Width = 432
      Height = 311
      Align = alRight
      Color = 14145495
      TabOrder = 1
      Visible = False
      OnResize = pnlTEAM_BResize
    end
    object pnlDIV: TPanel
      Left = 433
      Top = 1
      Width = 81
      Height = 311
      Align = alClient
      TabOrder = 2
      Visible = False
    end
  end
  object grpPontuacao: TGroupBox
    Left = 8
    Top = 480
    Width = 201
    Height = 121
    Caption = ' Pontua'#231#227'o '
    TabOrder = 1
    object Label7: TLabel
      Left = 11
      Top = 19
      Width = 118
      Height = 20
      Caption = 'Nav'#237'o coura'#231'ado'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 11
      Top = 43
      Width = 77
      Height = 20
      Caption = 'Submarino'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 147
      Top = 19
      Width = 46
      Height = 20
      Caption = '50pts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 147
      Top = 43
      Width = 46
      Height = 20
      Caption = '35pts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 11
      Top = 67
      Width = 87
      Height = 20
      Caption = 'Porta avi'#245'es'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 11
      Top = 91
      Width = 115
      Height = 20
      Caption = 'Nav'#237'o torpedeiro'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 147
      Top = 91
      Width = 46
      Height = 20
      Caption = '15pts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 147
      Top = 67
      Width = 46
      Height = 20
      Caption = '20pts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlTeamNames: TPanel
    Left = 0
    Top = 348
    Width = 947
    Height = 20
    Align = alTop
    TabOrder = 2
    object lblTeamA: TLabel
      Left = 1
      Top = 1
      Width = 432
      Height = 18
      Align = alLeft
      Alignment = taCenter
      AutoSize = False
      Caption = 'Equipe A - PONTUA'#199#195'O: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblTeamB: TLabel
      Left = 515
      Top = 1
      Width = 431
      Height = 18
      Align = alRight
      Alignment = taCenter
      AutoSize = False
      Caption = 'Equipe B - PONTUA'#199#195'O: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
  end
  object pnlJogadores: TPanel
    Left = 0
    Top = 368
    Width = 947
    Height = 66
    Align = alTop
    TabOrder = 3
    object lstTeamA: TListBox
      Left = 1
      Top = 1
      Width = 432
      Height = 64
      Align = alLeft
      ItemHeight = 13
      TabOrder = 0
    end
    object lstTeamB: TListBox
      Left = 514
      Top = 1
      Width = 432
      Height = 64
      Align = alRight
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 434
    Width = 947
    Height = 47
    Align = alTop
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 5
  end
  object TimerTitulo: TTimer
    Interval = 500
    OnTimer = TimerTituloTimer
    Left = 400
    Top = 376
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 440
    Top = 376
  end
end

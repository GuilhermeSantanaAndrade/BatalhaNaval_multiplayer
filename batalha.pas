unit batalha;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, grelha, Buttons, geral, SyncObjs, ClientRequests,
  INIFiles, IdBaseComponent, IdComponent, IdIpWatch, WinSock, uSockets, uThread, uList,
  AppStructs, ComCtrls;

type
  TfrmBatalha = class(TForm)
    pnlFundo: TPanel;
    pnlTEAM_A: TPanel;
    pnlTEAM_B: TPanel;
    pnlDIV: TPanel;
    grpPontuacao: TGroupBox;
    Label7: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    pnlTeamNames: TPanel;
    lblTeamA: TLabel;
    lblTeamB: TLabel;
    lblTitulo: TLabel;
    TimerTitulo: TTimer;
    pnlJogadores: TPanel;
    lstTeamA: TListBox;
    lstTeamB: TListBox;
    grpBottom: TGroupBox;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    lblIPServer: TLabel;
    lblStatusServer: TLabel;
    Label12: TLabel;
    btnConectar: TSpeedButton;
    grpOpcoesPartida: TGroupBox;
    btnComecar: TSpeedButton;
    grpTestes: TGroupBox;
    Button5: TButton;
    Button4: TButton;
    btnDistribuirBarcos: TButton;
    grpDisparos: TGroupBox;
    rbNao: TRadioButton;
    rbSim: TRadioButton;
    rgModoNormal: TRadioButton;
    rgModoTestes: TRadioButton;
    RichEdit1: TRichEdit;
    Label10: TLabel;
    Label11: TLabel;
    lblSeuNome: TLabel;
    lblSeuIP: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnComecarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlTEAM_AResize(Sender: TObject);
    procedure pnlTEAM_BResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnDistribuirBarcosClick(Sender: TObject);
    procedure TimerTituloTimer(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnConectarClick(Sender: TObject);
    procedure rbNaoClick(Sender: TObject);
    procedure rbSimClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    frmGrelha_A: TfrmGrelha;
    frmGrelha_B: TfrmGrelha;
    frmGrelhaEnemy: ^TfrmGrelha;
    frmGrelhaAlly:  ^TfrmGrelha;
    ListaJogadoresTeamA:TStringList;
    ListaJogadoresTeamB:TStringList;

    FreeMode:Boolean;

    TurnoJogador:String;
    GameStatus:TGameState;
    CriticalSection:TCriticalSection;
    Client: TTCPClientConnection;

    // infos local
    //------------------------------
    SeuIP, SeuNome, IPServer:String;
    FSeuTeam:TTeam;
    PortServer:Integer;
    JogadorEU:PJogador;
    // -----------------------------
    procedure AtualizaTurno(Turno:TTeam);
    procedure PrepareFormToGame(GameState:TGameState ; TurnTeam:TTeam);
    procedure ClientConnect(Client: TTCPConnection);
    procedure ClientDisconnect(Client: TTCPConnection);
    procedure ClientExecute(Thread: TTCPConnectionThread);
    procedure ClientError(Socket: TTCPSocket);
    procedure AtualizaStatusConnection;
    procedure AddLog(Msg: String ; Cor:TColor=clBlack ; Sty:TFontStyles=[]);
  public
    { Public declarations }
    function AddJogador(Player:PJogador; IsEnemy:Boolean):Boolean; overload;
    function AddJogador(Player:PJogador; Team:TTeam):Boolean; overload;
    procedure RefreshListTeam(EnemyList:Boolean); overload;
    procedure RefreshListTeam(Team:TTeam); overload;
    property SeuTeam:TTeam read FSeuTeam write FSeuTeam;
  end;

var
  frmBatalha: TfrmBatalha;
  ObjLog:^TRichEdit;

implementation

uses Math;

{$R *.dfm}

procedure TfrmBatalha.AddLog(Msg: String ; Cor:TColor=clBlack ; Sty:TFontStyles=[]);
Var
  sStart: word;
  sHora:String;
Const
  cnst_HoraTam = 8;
begin
    CriticalSection.Acquire;
    Try
      sHora := FormatDateTime('hh:nn:ss',Now()) + ' - ';

      sStart := Length(ObjLog.Text);
      ObjLog.Lines.Add( sHora + Msg);

      // Cor Horario
      ObjLog.SelStart            := sStart;
      ObjLog.SelLength           := cnst_HoraTam;
      ObjLog.SelAttributes.Color := clGray;
      ObjLog.SelAttributes.Style := [fsBold];

      // Cor Texto
      ObjLog.SelStart            := sStart + Length(sHora);
      ObjLog.SelLength           := Length(ObjLog.Text);
      ObjLog.SelAttributes.Color := Cor;
      ObjLog.SelAttributes.Style := Sty;
      Application.Processmessages;

      ObjLog.SelStart            := Length(ObjLog.Text);
      ObjLog.SelLength           := 0;
    finally
      CriticalSection.Release;
    end;
end;

procedure TfrmBatalha.ClientConnect(Client: TTCPConnection);
begin
  AddLog('Conectado com sucesso',clGreen);
  Client.Detach;
end;

procedure TfrmBatalha.ClientDisconnect(Client: TTCPConnection);
begin
  AddLog('Desconectado do servidor');
end;

procedure TfrmBatalha.ClientError(Socket: TTCPSocket);
begin
  if Socket.LastError <> WSAECONNRESET then
  begin
    AddLog(Format('Error #%d: %s', [Socket.LastError, RemoveEOL(Socket.LastErrorMessage)]), clRed);
  end;
end;

procedure TfrmBatalha.FormCreate(Sender: TObject);
Var
  ArqINI:TIniFile;
  IPGetter:TIdIPWatch;
begin
    ArqINI := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
    IPGetter := TIdIPWatch.Create(nil);
    Try
       IPServer   := ArqINI.ReadString('CONFIG','SERVER','');
       SeuNome    := Trim(ArqINI.ReadString('CONFIG','NAME',''));
       SeuIP      := IPGetter.LocalIP;
       lblSeuNome.Caption := SeuNome;
       lblSeuIP.Caption   := SeuIP;
       SeuTeam    := TeamNone;
       PortServer := ArqINI.ReadInteger('CONFIG','PORT',-1);

       If (IPServer='') or (IsWrongIP(IPServer)) then
          raise Exception.Create('IP do servidor configurado incorretamente.');

       If (SeuIP='') or (IsWrongIP(SeuIP)) then
          raise Exception.Create('Houve um problema ao tentar fazer leitura do IP Local.');

       If SeuNome = '' Then
          raise Exception.Create('Nome não configurado.');

       If PortServer < 0 Then
          raise Exception.Create('Porta de conexão inválida.');

       JogadorEU := New(PJogador);
       JogadorEU.Name := SeuNome;
       JogadorEU.IP   := SeuIP;
    finally
       FreeAndNil(ArqINI);
       IPGetter.Free;
    end;

    ObjLog  := @RichEdit1; // ObjLog Recebe posição da memória de memLog
    ObjLog.Clear;

    GameStatus        := gsNone;
    lblTitulo.Caption := '';
    lblTitulo.Color   := clBtnFace;

    CriticalSection := TCriticalSection.Create;

    ListaJogadoresTeamA := TStringList.Create;
    ListaJogadoresTeamA.Sorted := True;
    ListaJogadoresTeamB := TStringList.Create;
    ListaJogadoresTeamB.Sorted := True;

    Self.DoubleBuffered := True;

    frmGrelha_A          := TfrmGrelha.Create(Application, True);
    frmGrelha_A.ListTeam := ListaJogadoresTeamA;
    frmGrelha_A.Parent   := pnlTEAM_A;
    frmGrelha_A.Left     := 0;
    frmGrelha_A.Top      := 0;
    frmGrelha_A.SetColor(clRed);
    frmGrelha_A.GameStatus := gsNone;
    lblTeamA.Caption     := 'Equipe A';
    lblTeamA.Color       := clRed;
    lblTeamA.Font.Color  := clWhite;
    frmGrelhaEnemy       := Pointer(frmGrelha_A);
    //frmGrelha_A.Show;

    frmGrelha_B          := TfrmGrelha.Create(Application, False);
    frmGrelha_B.ListTeam := ListaJogadoresTeamA;
    frmGrelhaAlly := Pointer(frmGrelha_B);
    frmGrelha_B.Parent   := pnlTEAM_B;
    frmGrelha_B.Left     := 0;
    frmGrelha_B.Top      := 0;
    frmGrelha_B.SetColor(clBlue);
    frmGrelha_B.GameStatus := gsNone;
    lblTeamB.Caption     := 'Equipe B';
    lblTeamB.Color       := clBlue;
    lblTeamA.Font.Color  := clWhite;
    //frmGrelha_B.Show;
    FormResize(Self);
    lstTeamB.Items.Clear;

    Client := TTCPClientConnection.Create;
    With Client Do
    begin
      OnConnect    := ClientConnect;
      OnDisconnect := ClientDisconnect;
      OnExecute    := ClientExecute;
      OnError      := ClientError;
    end;
end;

procedure TfrmBatalha.FormDestroy(Sender: TObject);
var
  x:Integer;
begin
    If Assigned(frmGrelha_A) Then
       frmGrelha_A.Close;
    If Assigned(frmGrelha_B) Then
       frmGrelha_B.Close;

    For x:=0 To Pred(ListaJogadoresTeamA.Count) Do
    Begin
        Dispose(PJogador(ListaJogadoresTeamA.Objects[x]));
    end;
    FreeAndNil(ListaJogadoresTeamA);

    For x:=0 To Pred(ListaJogadoresTeamB.Count) Do
    Begin
        Dispose(PJogador(ListaJogadoresTeamB.Objects[x]));
    end;

    if Client.Connected then
      Client.Disconnect;
    Client.Free;

    FreeAndNil(ListaJogadoresTeamB);
    FreeAndNil(CriticalSection);
end;

procedure TfrmBatalha.btnComecarClick(Sender: TObject);
var
  GameState:TGameState;
  Jogador:PJogador;
  sktReceive:TSocketData;
begin
    if FreeMode Then
    begin
       AddLog('Nenhuma partida encontrada.');
       If MessageDlg('Não há partida para entrar'+#13+'Deseja iniciar uma nova?', mtInformation, [mbYes, mbNo], 0) = mrYes Then
       Begin
          AddLog('Inicializando partida no servidor');
          If True Then
          Begin
             sktReceive.Team := TeamA;
             Self.SeuTeam :=sktReceive.Team;
             AddLog('Aguardando Jogadores !!');
             GameStatus := gsWaitingPlayers;
             frmGrelha_A.GameStatus := gsWaitingPlayers;
             frmGrelha_B.GameStatus := gsWaitingPlayers;
             rbSim.Checked := True;
             btnDistribuirBarcosClick(nil);
             frmGrelha_A.Show;
             frmGrelha_B.Show;

             // ***** Para teste *****
             grpTestes.Visible   := True;
             grpDisparos.Visible := True;

             PrepareFormToGame(gsWaitingPlayers, sktReceive.Team);
          end else
          If sktReceive.ErrorMsg <> '' Then
          begin
             AddLog(sktReceive.ErrorMsg, clRed);
          end;
       end;
    end else
    begin
        AddLog('Verificando partida no servidor...');
        If TClientRequest.CheckGame(GameState) Then
        Begin
            If GameState in [gsInProgress] Then
               AddLog('Desculpe, a guerra já começou.')
            else
            If GameState in [gsNoGame] Then
            begin
               AddLog('Nenhuma partida encontrada.');
               If MessageDlg('Não há partida para entrar'+#13+'Deseja iniciar uma nova?', mtInformation, [mbYes, mbNo], 0) = mrYes Then
               Begin
                  AddLog('Inicializando partida no servidor');
                  If TClientRequest.StartNewGame(sktReceive) Then
                  Begin
                     Self.SeuTeam :=sktReceive.Team;
                     AddLog('Aguardando Jogadores !!');
                     GameStatus := gsWaitingPlayers;
                     frmGrelha_A.GameStatus := gsWaitingPlayers;
                     frmGrelha_B.GameStatus := gsWaitingPlayers;

                     // ***** Para teste *****
                     grpTestes.Visible   := True;
                     grpDisparos.Visible := True;

                     PrepareFormToGame(gsWaitingPlayers, sktReceive.Team);
                  end else
                  If sktReceive.ErrorMsg <> '' Then
                  begin
                     AddLog(sktReceive.ErrorMsg, clRed);
                  end;
               end;
            end else
            if GameState in [gsWaitingPlayers] then
            begin
               AddLog('Detectado partida em aberto.');
               If MessageDlg('Existe uma partida em aberto. Deseja unir-se?', mtInformation, [mbYes, mbNo], 0) = mrYes Then
               Begin
                  AddLog('Conectando à partida');
                  If TClientRequest.JoinGame(sktReceive) Then
                  Begin
                     Self.SeuTeam :=sktReceive.Team;
                     AddLog('Aguardando Jogadores !!');
                     GameStatus := gsWaitingPlayers;
                     frmGrelha_A.GameStatus := gsWaitingPlayers;
                     frmGrelha_B.GameStatus := gsWaitingPlayers;

                     PrepareFormToGame(gsWaitingPlayers,sktReceive.Team);
                  end else
                  If sktReceive.ErrorMsg <> '' Then
                  begin
                     AddLog(sktReceive.ErrorMsg, clRed);
                  end;
               end;
            end;
        End;
    end;
end;

procedure TfrmBatalha.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    frmBatalha := nil;
    Action := caFree;
end;

procedure TfrmBatalha.pnlTEAM_AResize(Sender: TObject);
begin
    if Assigned(frmGrelha_A) Then
    Begin
       frmGrelha_A.Height := pnlTEAM_A.Height;
       frmGrelha_A.Width  := pnlTEAM_A.Width;
    End;
end;

procedure TfrmBatalha.pnlTEAM_BResize(Sender: TObject);
begin
    if Assigned(frmGrelha_B) Then
    Begin
       frmGrelha_B.Height := pnlTEAM_B.Height;
       frmGrelha_B.Width  := pnlTEAM_B.Width;
    End;
end;

procedure TfrmBatalha.FormResize(Sender: TObject);
begin
    pnlTEAM_A.Width := Trunc(0.49 * pnlFundo.Width);
    pnlTEAM_B.Width := Trunc(0.49 * pnlFundo.Width);
    pnlDIV.Width    := Trunc(0.01 * pnlFundo.Width);

    lblTeamA.Width   := pnlTEAM_A.Width;
    lblTeamB.Width   := pnlTeamNames.Width - pnlTEAM_A.Width - pnlDIV.Width;

    grpPontuacao.Left := 8;
    grpPontuacao.Top  := Self.Height - grpPontuacao.Height - (grpPontuacao.Height div 2) + 15 ;
    grpBottom.Top     := grpPontuacao.Top;
    grpBottom.Width   := self.Width - 32;

    ObjLog.Height := (grpPontuacao.Top - ObjLog.Top ) - 15;

    lstTeamA.Width    := lblTeamA.Width;
    lstTeamB.Width    := lblTeamB.Width;
end;

procedure TfrmBatalha.btnDistribuirBarcosClick(Sender: TObject);
var
  ship:TShip;
  x:Integer;
  ListaNavios, lstResults:TStringList;
begin
    ListaNavios := TStringList.Create;
    Try
      For x:=0 To Pred(3) Do
      Begin
          ship := TCouracado.GetInstance;
          ListaNavios.AddObject('couracado'+IntToStr(x), Pointer(ship));
      end;

      For x:=0 To Pred(3) Do
      Begin
          ship := Tsubmarino.GetInstance;
          ListaNavios.AddObject('submarino'+IntToStr(x), Pointer(ship));
      end;

      For x:=0 To Pred(5) Do
      Begin
          ship := TPortaAvioes.GetInstance;
          ListaNavios.AddObject('portaavioes'+IntToStr(x), Pointer(ship));
      end;

      For x:=0 To Pred(10) Do
      Begin
          ship := TTorpedeiro.GetInstance;
          ListaNavios.AddObject('torpedeiros'+IntToStr(x), Pointer(ship));
      end;

      AddLog('Distribuindo Barcos. Aguarde ....');
      Application.ProcessMessages;
      RandomAlocate(ListaNavios, frmGrelha_B.GetMemMatriz, lstResults);
      frmGrelha_B.RefreshScreen;
      RandomAlocate(ListaNavios, frmGrelha_A.GetMemMatriz, lstResults);
      frmGrelha_A.RefreshScreen;
      AddLog('Distribuição concluída.');
    Finally
      For x:= 0 To Pred(ListaNavios.Count) do
          TShip(ListaNavios.Objects[x]).Free;
      FreeAndNil(ListaNavios);
    End;
end;

procedure TfrmBatalha.AtualizaTurno(Turno: TTeam);
begin
    If Turno = SeuTeam  Then
       TurnoAtualClient := trMyTurn
    else
       TurnoAtualClient := trEnemyTurn;

    TimerTituloTimer(nil);
end;

procedure TfrmBatalha.TimerTituloTimer(Sender: TObject);
begin
    TimerTitulo.Enabled := False;
    Try
      If FreeMode Then
      Begin
         lblTitulo.Caption    := 'Modo Livre (Ligado))';

         If lblTitulo.Tag = 0 Then
         Begin
             lblTitulo.Color      := clGreen;
             lblTitulo.Font.Color := clWhite;
             lblTitulo.Tag := 1;
         End else
         begin
             lblTitulo.Color      := clWhite;
             lblTitulo.Font.Color := clGreen;
             lblTitulo.Tag := 0;
         end;
      end else
      If GameStatus = gsInProgress Then
      Begin
          If TurnoAtualClient = trMyTurn Then
          Begin
              If TurnoJogador = SeuNome Then
                 lblTitulo.Caption    := 'Sua vez'
              else
                 lblTitulo.Caption    := 'Vez da sua equipe(Jogador:'+TurnoJogador+')';

              If lblTitulo.Tag = 0 Then
              Begin
                  lblTitulo.Color      := clWhite;
                  lblTitulo.Font.Color := clBlue;
                  lblTitulo.Tag := 1;
              End else
              begin
                  lblTitulo.Color      := clBlue;
                  lblTitulo.Font.Color := clWhite;
                  lblTitulo.Tag := 0;
              end;
          End else
          Begin
              lblTitulo.Caption    := 'Vez dos adversários(Jogador:'+TurnoJogador+')';

              If lblTitulo.Tag = 0 Then
              Begin
                  lblTitulo.Color      := clRed;
                  lblTitulo.Font.Color := clWhite;
                  lblTitulo.Tag := 1;
              End else
              begin
                  lblTitulo.Color      := clWhite;
                  lblTitulo.Font.Color := clRed;
                  lblTitulo.Tag := 0;
              end;
          end;
      end else
      If GameStatus = gsWaitingPlayers Then
      begin
          lblTitulo.Tag     := 0;
          lblTitulo.Caption := 'Aguardando Jogadores';
          lblTitulo.Color   := clYellow;
          lblTitulo.Font.Color := clBlack;
      end else
      begin
          lblTitulo.Tag     := 0;
          lblTitulo.Caption := 'Batalha Naval';
          lblTitulo.Color   := clBtnFace;
          lblTitulo.Font.Color := clBlack;
      end;
    Finally
      TimerTitulo.Enabled := True;
    End;
end;

function TfrmBatalha.AddJogador(Player: PJogador ; IsEnemy:Boolean): Boolean;
begin
    Result := False;
    CriticalSection.Acquire;
    Try
       If IsEnemy Then
       Begin
          If frmGrelha_A.IsEnemy Then
             ListaJogadoresTeamA.AddObject(Player.IP, Pointer(Player))
          else
             ListaJogadoresTeamB.AddObject(Player.IP, Pointer(Player));
       end else
       begin
          If not frmGrelha_A.IsEnemy Then
             ListaJogadoresTeamA.AddObject(Player.IP, Pointer(Player))
          else
             ListaJogadoresTeamB.AddObject(Player.IP, Pointer(Player));
       end;
       Result := True;
    finally
       CriticalSection.Release;
       RefreshListTeam(IsEnemy);
    end;
end;

procedure TfrmBatalha.PrepareFormToGame(GameState:TGameState ; TurnTeam:TTeam);
begin
    Case GameState Of
      gsNoGame:
      Begin

      end;
      gsWaitingPlayers:
      Begin
        pnlTEAM_A.Visible := True;
        pnlTEAM_B.Visible := True;
        pnlDIV.Visible    := True;
        Self.Resize;
        AtualizaTurno(turnTeam);
      end;
      gsInProgress:
      Begin
        pnlTEAM_A.Visible := True;
        pnlTEAM_B.Visible := True;
        pnlDIV.Visible    := True;
        Self.Resize;
        AtualizaTurno(turnTeam);
      end;
    end;
end;

procedure TfrmBatalha.RefreshListTeam(EnemyList: Boolean);
Var
  ListaBox:TListBox;
  Lista:TStringList;
  x:Integer;
  s:String;
begin
    CriticalSection.Acquire;
    Try
       If EnemyList Then
       Begin
          If frmGrelha_A.IsEnemy Then
          begin
             ListaBox := lstTeamA;
             Lista    := ListaJogadoresTeamA;
          end else
          begin
             ListaBox := lstTeamB;
             Lista    := ListaJogadoresTeamB;
          end;
       end else
       begin
          If not frmGrelha_A.IsEnemy Then
          begin
             ListaBox := lstTeamA;
             Lista    := ListaJogadoresTeamA;
          end else
          begin
             ListaBox := lstTeamB;
             Lista    := ListaJogadoresTeamB;             
          end;
       end;

       ListaBox.Items.Clear;
       For x:=0 To Pred(Lista.Count) Do
       Begin
           s := '['+ IntToStr(x) +']: "'+PJogador(Lista.Objects[x]).Name +'" ('+PJogador(Lista.Objects[x]).IP+')';
           If PJogador(Lista.Objects[x]).IP = SeuIP Then
              s := s + ' **você**';
           ListaBox.Items.Add(s);
       end;
    finally
       CriticalSection.Release;
    end;
end;

procedure TfrmBatalha.Button4Click(Sender: TObject);
var
  Jogador:PJogador;
begin
    Jogador := JogadorGetInstace(FormatDateTime('yyyymmddhhnnss',Now()),SeuIP);
    ListaJogadoresTeamA.AddObject(Jogador.Name, Pointer(Jogador));
    RefreshListTeam(frmGrelha_A.IsEnemy);
end;

procedure TfrmBatalha.Button5Click(Sender: TObject);
var
  Jogador:PJogador;
begin
    Jogador := JogadorGetInstace(FormatDateTime('yyyymmddhhnnss',Now()),SeuIP);
    ListaJogadoresTeamB.AddObject(Jogador.Name, Pointer(Jogador));
    RefreshListTeam(frmGrelha_B.IsEnemy);
end;

procedure TfrmBatalha.btnConectarClick(Sender: TObject);
var
  sktData:TSocketData;
begin
    FreeMode := rgModoTestes.Checked;
    frmGrelha_A.FreeMode := FreeMode;
    frmGrelha_B.FreeMode := FreeMode;

    If Not FreeMode Then
    begin
      if not Client.Connected then
      begin
        If Client.Connect(IPServer + ':' + IntToStr(PortServer)) then
        begin
           sktData.Command := _request_AlterName;
           sktData.Value   := SeuNome;
           Client.WriteBuffer(sktData, SizeOf(SktData));
           ClientRequests.Client := Self.Client;

        end else
           AddLog('Conexão falou.');
      end;
    end else
    begin
      AddLog('Conectado com sucesso (FREE MODE ON)');
      rbSimClick(nil);
    end;

    rgModoNormal.Enabled := False;
    rgModoTestes.Enabled := False;

    AtualizaStatusConnection
end;

procedure TfrmBatalha.AtualizaStatusConnection;
begin
    lblIPServer.Caption := IPServer;
    If FreeMode or Client.Connected Then
    begin
       lblStatusServer.Caption  := 'Conectado';
       btnConectar.Visible      := False;
       grpOpcoesPartida.Visible := True;
    end else
    begin
       lblStatusServer.Caption  := 'Desconectado';
       btnConectar.Visible      := True;
       grpOpcoesPartida.Visible := False;
    end;
end;

procedure TfrmBatalha.rbNaoClick(Sender: TObject);
begin
    frmGrelha_A.FreeMode := False;
    frmGrelha_B.FreeMode := False;
end;

procedure TfrmBatalha.rbSimClick(Sender: TObject);
begin
    frmGrelha_A.FreeMode := True;
    frmGrelha_B.FreeMode := True;
end;

procedure TfrmBatalha.ClientExecute(Thread: TTCPConnectionThread);
var
  sktReceive:TSocketData;
  iTentativa:Integer;
  PPlayer:PJogador;
begin
  Try
    Thread.Connection.ReadBuffer(sktReceive, SizeOf(sktReceive));

    if Thread.Connection.Connected then
    begin
       If sktReceive.NotifyClient Then
       Begin
           Thread.SetResponse(sktReceive);
       end else
       If sktReceive.Command = _request_NewPlayer  Then
       Begin
           iTentativa := 0;
           repeat
             inc(iTentativa);
             Sleep(100);
           until (Self.SeuTeam <> TeamNone) or (iTentativa > 5);

           if iTentativa > 5 then
           begin
              raise Exception.Create('Falha em Request_NewPlayer. Variável Team inválida.');
           end;

           PPlayer    := New(PJogador);
           With PPlayer^ Do
           Begin
               Index  := sktReceive.Player.Index;
               Name   := sktReceive.Player.Name;
               IP     := sktReceive.Player.IP;
               Team   := sktReceive.Player.Team;
               Client := sktReceive.Player.Client;
           end;

           AddJogador(PPlayer, sktReceive.Team);
       end else
       if sktReceive.Command = _request_GameStart then
       begin
          GameStatus             := gsInProgress;
          frmGrelha_A.GameStatus := gsInProgress;
          frmGrelha_B.GameStatus := gsInProgress;
          If sktReceive.Team = TeamA Then
          begin
            frmGrelha_A.IsEnemy := False;
            frmGrelha_B.IsEnemy := True;
          end else
          begin
            frmGrelha_A.IsEnemy := True;
            frmGrelha_B.IsEnemy := False;
          end;

          TurnoJogador   := sktReceive.Player.Name;
          PrepareFormToGame(gsInProgress, sktReceive.Team);
          Timer1.Enabled := True;
       end;
    end;
  Except
    on E : Exception Do
    Begin
       AddLog('Error: '+ e.Message, clRed);
    end;
  end;
end;

function TfrmBatalha.AddJogador(Player: PJogador; Team: TTeam): Boolean;
begin
    Result := False;
    CriticalSection.Acquire;
    Try
       If Team = TeamA Then
          ListaJogadoresTeamA.AddObject(Player.IP, Pointer(Player))
       else
          ListaJogadoresTeamB.AddObject(Player.IP, Pointer(Player));
       Result := True;
    finally
       CriticalSection.Release;
       RefreshListTeam(Team);
    end;
end;

procedure TfrmBatalha.RefreshListTeam(Team: TTeam);
Var
  ListaBox:TListBox;
  Lista:TStringList;
  x:Integer;
  s:String;
begin
    CriticalSection.Acquire;
    Try
       If Team = TeamA Then
       begin
          ListaBox := lstTeamA;
          Lista    := ListaJogadoresTeamA;
       end else
       begin
          ListaBox := lstTeamB;
          Lista    := ListaJogadoresTeamB;
       end;

       ListaBox.Items.Clear;
       For x:=0 To Pred(Lista.Count) Do
       Begin
           s := '['+ FormatFloat('00',x) +']: "'+PJogador(Lista.Objects[x]).Name +'" ('+PJogador(Lista.Objects[x]).IP+')';
           If PJogador(Lista.Objects[x]).IP = SeuIP Then
              s := s + ' **você**';
           ListaBox.Items.Add(s);
       end;
    finally
       CriticalSection.Release;
    end;
end;

procedure TfrmBatalha.Timer1Timer(Sender: TObject);
begin
    Timer1.Enabled := False;
    frmGrelha_A.RefreshScreen;
    frmGrelha_B.RefreshScreen;
    btnDistribuirBarcosClick(nil);
    frmGrelha_A.Show;
    frmGrelha_B.Show;

    AddLog('*** Jogo iniciado!!!!! ***',clGreen,[fsBold]);
end;

end.

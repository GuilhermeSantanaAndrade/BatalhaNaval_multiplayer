unit MainServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSockets, StdCtrls, geral, WinSock, SyncObjs, Math, AppStructs;

type
  TfrmServer = class(TForm)
    Label1: TLabel;
    lblTitulo: TLabel;
    edtPort: TEdit;
    btnIniciar: TButton;
    Memo1: TMemo;
    lstTeamA: TListBox;
    lstTeamB: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lstJogadores: TListBox;
    Label5: TLabel;
    btnIniciarPartida: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnIniciarClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIniciarPartidaClick(Sender: TObject);
  private
    { Private declarations }
    CriticalSection1:TCriticalSection;
    CriticalSection2:TCriticalSection;

    ListaJogadores:TStringList;

    GameStatus:TGameState;

    function  AddPlayer(Player: PJogador):Integer;
    function  DeletePlayer(Player: PJogador):Boolean;
    function  AlterPlayer(Player:PJogador):Boolean;
    procedure RefreshListTeam;
    procedure BroadCast(sktSent:TSocketData ; Team:TTeam);
  public
    ClientCounter: Integer;
    Server: TTCPServer;
    procedure ServerConnect(Client: TTCPConnection);
    procedure ServerDisconnect(Client: TTCPConnection);
    procedure ServerExecute(Thread: TTCPConnectionThread);
    procedure ServerError(Socket: TTCPSocket);
  end;

  TClientData = class
  public
    Jogador:PJogador;
  end;

var
  frmServer: TfrmServer;

implementation

{$R *.dfm}

procedure TfrmServer.FormCreate(Sender: TObject);
begin
   ClientCounter := 0;
   GameStatus    := gsNoGame;
   //btnIniciarPartida.Visible := False;
   Server := TTCPServer.Create;

   CriticalSection1 := TCriticalSection.Create;
   CriticalSection2 := TCriticalSection.Create;

   ListaJogadores             := TStringList.Create;
   ListaJogadores.Sorted      := True;

   With Server do
   begin
     OnConnect := ServerConnect;
     OnDisconnect := ServerDisconnect;
     OnExecute := ServerExecute;
     OnError := ServerError;
   end;
end;

procedure TfrmServer.ServerConnect(Client: TTCPConnection);
Var
  Idx:Integer;
  sktData:TSocketData;
begin
  Try
    Inc(ClientCounter);

    Client.Data := TClientData.Create;
    TClientData(Client.Data).Jogador        := New(PJogador);
    TClientData(Client.Data).Jogador.IP     := Client.PeerIP;
    TClientData(Client.Data).Jogador.Client := TTCPConnection(Client);
    TClientData(Client.Data).Jogador.Team   := TeamNone;

    With TClientData(Client.Data) Do
    Begin
        Idx := AddPlayer(Jogador);
        If Idx = -1 Then
           raise Exception.Create('erro em AddPlayer (idx inválido)');
        TClientData(Client.Data).Jogador.Index := Idx;
        Memo1.Lines.Add(Format(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'CONECTADO %s (%s)', [Jogador.Name, Client.PeerIP]));
    end;

    Client.Detach;
  except
    on E : Exception Do
    Begin
       Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Error: '+ e.Message);
       Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Serviço finalizado devido à erro inexperado.');
       Server.Listen := False;
       btnIniciar.Caption := 'Iniciar Servidor';
    end;
  end;
end;

procedure TfrmServer.ServerDisconnect(Client: TTCPConnection);
var Idx:Integer;
begin
  Try
    Dec(ClientCounter);
    with TClientData(Client.Data) do
    begin
      If Jogador.Team = TeamA Then
         Memo1.Lines.Add(Format(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'DESCONECTADO %s (%s) (TeamA)', [Jogador.Name, Jogador.IP]))
      else
      if Jogador.Team = TeamB Then
         Memo1.Lines.Add(Format(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'DESCONECTADO %s (%s) (TeamB)', [Jogador.Name, Jogador.IP]))
      else
         Memo1.Lines.Add(Format(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'DESCONECTADO %s (%s) (Sem Equipe)', [Jogador.Name, Jogador.IP]));

      DeletePlayer(Jogador);
      RefreshListTeam;
      Free;
    end;
  except
    on E : Exception Do
    Begin
       Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Error: '+ e.Message);
    end;
  end;
end;


procedure TfrmServer.ServerError(Socket: TTCPSocket);
begin
  if Socket.LastError <> WSAECONNRESET then
    Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' + Format('Error #%d: %s', [Socket.LastError, RemoveEOL(Socket.LastErrorMessage)]));
end;

procedure TfrmServer.btnIniciarClick(Sender: TObject);
begin
    If not IsSomenteNumeros(edtPort.Text) Then
       Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Porta inválida.');

    Server.AddBinding(TTCPBinding.Create('localhost:'+ edtPort.Text));
    Server.Listen := not Server.Listen;
    if Server.Listen then
    begin
      Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' + 'Serviço iniciado');
      btnIniciar.Caption := 'Parar Servidor';
    end
    else
    begin
      Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Serviço parado');
      btnIniciar.Caption := 'Iniciar Servidor';
    end;
end;

procedure TfrmServer.FormDestroy(Sender: TObject);
begin
  Server.Listen := False;
  Server.Free;
  FreeAndNil(ListaJogadores);
  FreeAndNil(CriticalSection1);
  FreeAndNil(CriticalSection2);
end;

function TfrmServer.AddPlayer(Player: PJogador):Integer;
begin
    Result := -1;
    CriticalSection1.Acquire;
    Try
       Result := ListaJogadores.AddObject(IntToStr(TTCPConnection(PLayer.Client).NumSocket), Pointer(Player));
    finally
       CriticalSection1.Release;
       RefreshListTeam;
    end;
end;

procedure TfrmServer.RefreshListTeam;
Var
  ListaBox:TListBox;
  Lista:TStringList;
  x:Integer;
  s:String;
begin
    CriticalSection1.Acquire;
    Try
       lstJogadores.Items.Clear;
       lstTeamA.Items.Clear;
       lstTeamB.Items.Clear;

       For x:=0 To Pred(ListaJogadores.Count) Do
       Begin
           s := '['+ FormatFloat('00', x) +'] - '+
                        PJogador(ListaJogadores.Objects[x]).Name +' ('+
                        PJogador(ListaJogadores.Objects[x]).IP + ')';
           If PJogador(ListaJogadores.Objects[x]).Team = TeamNone Then
           Begin
              lstJogadores.Items.Add(s);
           end else
           If PJogador(ListaJogadores.Objects[x]).Team = TeamA Then
           Begin
              lstTeamA.Items.Add(s);
           end else
           If PJogador(ListaJogadores.Objects[x]).Team = TeamB Then
           Begin
              lstTeamB.Items.Add(s);
           end;
       end;
    finally
       CriticalSection1.Release;
    end;
end;

function TfrmServer.DeletePlayer(Player: PJogador): Boolean;
var
  idx:Integer;
  PPlayer:PJogador;
begin
    Idx := -1;
    CriticalSection1.Acquire;
    Try
      if PJogador(ListaJogadores.Objects[Player.Index]).IP = Player.IP Then
      begin
         PPlayer := PJogador(ListaJogadores.Objects[Player.Index]);
         ListaJogadores.Delete(Player.Index);
         Dispose(PPlayer);
         Result := True;
      end else
      if ListaJogadores.Find(IntToStr(TTCPConnection(PLayer.Client).NumSocket) , idx) then
      begin
         PPlayer := PJogador(ListaJogadores.Objects[idx]);
         ListaJogadores.Delete(idx);
         Dispose(PPlayer);
         Result := True;
      end;

      For idx := 0 To Pred(ListaJogadores.Count) Do
      Begin
         PJogador(ListaJogadores.Objects[idx]).Index := idx;
      end;
    Finally
      CriticalSection1.Release;
    end;
end;

function TfrmServer.AlterPlayer(Player: PJogador): Boolean;
var
  idx:Integer;
begin
    idx := -1;
    if PJogador(ListaJogadores.Objects[Player.Index]).IP = Player.IP Then
    begin
        PJogador(ListaJogadores.Objects[Player.Index]).Name := Player.Name;
        PJogador(ListaJogadores.Objects[Player.Index]).Team := Player.Team;
    end else
    if ListaJogadores.Find(IntToStr(TTCPConnection(PLayer.Client).NumSocket), idx) then
    begin
        PJogador(ListaJogadores.Objects[idx]).Name := Player.Name;
        PJogador(ListaJogadores.Objects[idx]).Team := Player.Team;
    end;

    Result := (Idx <> -1);    
end;

procedure TfrmServer.ServerExecute(Thread: TTCPConnectionThread);
var
  sktInput:TSocketData;
  sktSent:TSocketData;
  JogadorInfos:PJogador;
  x:Integer;
  EnviaTodos:Boolean;

  procedure InsertOnTeam(Player:PJogador ; Team:TTeam=TeamNone);
  begin
     If Team = TeamNone Then
     Begin
        If (ClientCounter div 2) = 0 Then
            Player.Team := TeamA
        else
            Player.Team := TeamB;
     end else
     begin
        Player.Team := Team;
     end;
  end;

  procedure EnviaPlayersConectados;
  var
    x1:Integer;
    sktSent1:TSocketData;
  begin
     CriticalSection1.Acquire;
     Try
       For x1:=0 To Pred(ListaJogadores.Count) Do
       Begin
           If PJogador(ListaJogadores.Objects[x1]).Index <> JogadorInfos^.Index Then
           Begin
               sktSent1.Command        := _request_NewPlayer;
               sktSent1.CodTransaction := '';
               sktSent1.Value          := '';
               sktSent1.ErrorMsg       := '';
               sktSent1.GameState      := GameStatus;
               sktSent1.Team           := PJogador(ListaJogadores.Objects[x1]).Team;
               sktSent1.Player.Index   := PJogador(ListaJogadores.Objects[x1]).Index;
               sktSent1.Player.Name    := PJogador(ListaJogadores.Objects[x1]).Name;
               sktSent1.Player.IP      := PJogador(ListaJogadores.Objects[x1]).IP;
               sktSent1.Player.Team    := PJogador(ListaJogadores.Objects[x1]).Team;
               sktSent1.Player.Client  := PJogador(ListaJogadores.Objects[x1]).Client;
               sktSent1.NotifyClient   := False;
               TTCPConnection(JogadorInfos.Client).WriteBuffer(sktSent1, SizeOf(sktSent1));
           end;
       end;
     Finally
       CriticalSection1.Release;
     End;
  end;
begin
  Try
    Thread.Connection.ReadBuffer(sktInput, SizeOf(sktInput));
    EnviaTodos := False;

    if Thread.Connection.Connected then
    begin
       JogadorInfos := TClientData(Thread.Connection.Data).Jogador;

       If sktInput.Command = _request_AlterName Then
       Begin
          Thread.Lock;
          try
            JogadorInfos.Name := sktInput.Value;
            RefreshListTeam;
          finally
            Thread.Unlock;
          end;
       end;

       If sktInput.Command = _request_CheckGame Then
       Begin
          Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +JogadorInfos.Name +' ['+ _request_CheckGame +']');
          sktSent.Command        := _request_CheckGame;
          sktSent.CodTransaction := sktInput.CodTransaction;
          sktSent.GameState      := GameStatus;
          sktSent.NotifyClient   := True;
          sktSent.ErrorMsg       := '';
          If sktSent.CodTransaction <> '' Then
             Thread.Connection.WriteBuffer(sktSent, SizeOf(sktSent));
       end;

       If sktInput.Command = _request_StartNewGame Then
       Begin
          Try
            Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +JogadorInfos.Name +' ['+ _request_StartNewGame +']');
            If GameStatus = gsNoGame Then
            begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Partida iniciada. Aguardando jogadores');
                GameStatus                := gsWaitingPlayers;
                btnIniciarPartida.Visible := True;

                InsertOnTeam(JogadorInfos);

                // Altera o Input para também entrar no Request_NewPlayer
                sktInput.Command        := _request_NewPlayer;
                sktInput.Team           := JogadorInfos.Team;

                RefreshListTeam;
                sktSent.Command        := _response_OK;
                sktSent.GameState      := GameStatus;
                sktSent.Value          := '';
                sktSent.ErrorMsg       := '';
                sktSent.CodTransaction := sktInput.CodTransaction;
                sktSent.Team           := JogadorInfos.Team;
            end else
            If GameStatus in [gsInProgress, gsWaitingPlayers] then
            begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Tentativa de iniciar jogo com partida em aberto.');
                sktSent.Command        := _response_ALREADY;
                sktSent.GameState      := GameStatus;
                sktSent.Value          := '';
                sktSent.ErrorMsg       := '';
                sktSent.CodTransaction := sktInput.CodTransaction;
                sktSent.Team           := TeamNone;
            end else
                raise Exception.Create('GameStatus de Tipo inválido');
          except
            On E : Exception Do
            Begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Error: '+ e.message);
                sktSent.Command  := _response_Err;
                sktSent.ErrorMsg := Copy(e.message,1,255);
                sktSent.CodTransaction := sktInput.CodTransaction;
            end;
          end;

          sktSent.NotifyClient   := True;
          If sktSent.CodTransaction <> '' Then
             Thread.Connection.WriteBuffer(sktSent, SizeOf(sktSent));
       end;

       If sktInput.Command = _request_JoinGame Then
       Begin
          Try
            Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +JogadorInfos.Name +' ['+ _request_JoinGame +']');
            If JogadorInfos.Team <> TeamNone then
            begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Jogador já está no game.');
                sktSent.Command        := _response_ALREADY;
                sktSent.GameState      := GameStatus;
                sktSent.Value          := '';
                sktSent.ErrorMsg       := 'Jogador já está no game.';
                sktSent.CodTransaction := sktInput.CodTransaction;
                sktSent.Team           := JogadorInfos.Team;
            end else
            If GameStatus = gsWaitingPlayers Then
            begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +JogadorInfos.Name + ' ('+ JogadorInfos.IP +') uniu-se à partida.');
                InsertOnTeam(JogadorInfos);
                EnviaTodos := True;

                // Altera o Input para também entrar no Request_NewPlayer
                sktInput.Command        := _request_NewPlayer;
                sktInput.Team           := JogadorInfos.Team;

                RefreshListTeam;
                sktSent.Command        := _response_OK;
                sktSent.GameState      := GameStatus;
                sktSent.Value          := '';
                sktSent.ErrorMsg       := '';
                sktSent.CodTransaction := sktInput.CodTransaction;
                sktSent.Team           := JogadorInfos.Team;
                sktSent.Player.Index   := JogadorInfos.Index;
                sktSent.Player.Name    := JogadorInfos.Name;
                sktSent.Player.IP      := JogadorInfos.IP;
                sktSent.Player.Team    := JogadorInfos.Team;
                sktSent.Player.Client  := JogadorInfos.Client;
                sktSent.Team           := JogadorInfos.Team;
            end else
            If GameStatus in [gsNoGame, gsInProgress] then
            begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Tentativa de entrar em uma partida inexistente.');
                sktSent.Command        := _response_ERR;
                sktSent.GameState      := GameStatus;
                sktSent.Value          := '';
                sktSent.ErrorMsg       := 'Tentativa de entrar em uma partida inexistente.';
                sktSent.CodTransaction := sktInput.CodTransaction;
                sktSent.Team           := TeamNone;
            end else
                raise Exception.Create('GameStatus de Tipo inválido');
          except
            On E : Exception Do
            Begin
                Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Error: '+ e.message);
                sktSent.Command  := _response_Err;
                sktSent.ErrorMsg := Copy(e.message,1,255);
                sktSent.CodTransaction := sktInput.CodTransaction;
            end;
          end;

          sktSent.NotifyClient   := True;
          If sktSent.CodTransaction <> '' Then
             Thread.Connection.WriteBuffer(sktSent, SizeOf(sktSent));
       end;

       If sktInput.Command = _request_NewPlayer Then
       Begin
          If JogadorInfos.Team = TeamNone Then
             InsertOnTeam(JogadorInfos);

          sktSent.Command        := _request_NewPlayer;
          sktSent.CodTransaction := '';
          sktSent.Value          := '';
          sktSent.ErrorMsg       := '';
          sktSent.GameState      := GameStatus;
          sktSent.Team           := JogadorInfos.Team;
          sktSent.Player.Index   := JogadorInfos^.Index;
          sktSent.Player.Name    := JogadorInfos^.Name;
          sktSent.Player.IP      := JogadorInfos^.IP;
          sktSent.Player.Team    := JogadorInfos^.Team;
          sktSent.Player.Client  := JogadorInfos^.Client;
          sktSent.NotifyClient   := False;

          BroadCast(sktSent, TeamBoth);

          If EnviaTodos Then
             EnviaPlayersConectados;
       end;
    end;
  Except
    on E : Exception Do
    Begin
       Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Error: '+ e.Message);
    end;
  end;
end;

procedure TfrmServer.BroadCast(sktSent:TSocketData ; Team:TTeam);
var
  x:Integer;
begin
   Try
     CriticalSection1.Acquire;
     Try
       For x:= 0 To Pred(ListaJogadores.Count) Do
       Begin
           If (PJogador(ListaJogadores.Objects[x]).Team = Team) or
              ((Team = TeamBoth) And (PJogador(ListaJogadores.Objects[x]).Team In [TeamA, TeamB])) or
              ( Team = TeamALL)  Then
              TTCPConnection(PJogador(ListaJogadores.Objects[x]).Client).WriteBuffer(sktSent, SizeOf(sktSent));
       end;
     finally
       CriticalSection1.Release;
     end;
   except
     On E : Exception Do
     Begin
         Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Erro em Broadcast.'+ e.Message);
         raise;
     end;
   end;
end;

procedure TfrmServer.btnIniciarPartidaClick(Sender: TObject);
Var
  sktSent:TSocketData;
  Jogador:PJogador;

  function GetFirstPlayer(ATeam:TTeam):PJogador;
  var
    x:Integer;
  begin
     For x:= 0 To Pred(ListaJogadores.Count) Do
     Begin
         If (PJogador(ListaJogadores.Objects[x]).Team = ATeam) Then
           Result := PJogador(ListaJogadores.Objects[x]);
     end;
  end;
begin
  sktSent.Command := _request_GameStart;
  sktSent.Team    := TeamA;
  sktSent.Player  := GetFirstPlayer(TeamA)^;
  sktSent.CodTransaction := '';
  sktSent.NotifyClient := False;
  BroadCast(sktSent, TeamBoth);
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss',Now()) + ' - ' +FormatDateTime('hh:nn:ss',Now()) + ' - ' +'Jogo iniciado!')
end;

end.

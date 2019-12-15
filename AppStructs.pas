unit AppStructs;

interface

uses SysUtils, Classes;

Const
  _PosDisparo     = 1; // Ao alterar, necessário atualizar a constante _MemArrayFields
  _PosIndexShip   = 2;
  _PosIndexPart   = 3;
  _PosOrientation = 4;

  _MemArrayFields = 4;
  _CountGrid = 25; //<- Matriz quadrada

Type
  TMemMatriz=Array of Array of String[_MemArrayFields];
  TOrientation = (otVertical, otHorizontal);
  TTeamTurn    = (turnTeamA, turnTeamB);
  TGameState   = (gsNone, gsNoGame, gsWaitingPlayers, gsInProgress);

  ENoResponse = class(Exception);

  TTeam = (TeamNone, TeamA, TeamB, TeamBoth, TeamALL);
  TTurn = (trMyTurn, trEnemyTurn);

  PJogador = ^TJogador;
  TJogador = record
    Index:Integer;
    Name:String[255];
    IP:String[15];  
    Team:TTeam;
    Client:Pointer;
  end;

  PSocketData = ^TSocketData;
  TSocketData = record
    Command:String[15];
    CodTransaction:String[10];
    Value:String[255];
    ErrorMsg:String[255];
    Player:TJogador;
    Team:TTeam;
    GameState:TGameState;
    NotifyClient:Boolean;
  end;

  EInvalidShipPosition    = class(Exception);
  EInvalidMemGridPosition = class(Exception);

  PPos = ^TPos;
  TPos = record
    x:Integer;
    y:Integer;
  end;
  
implementation

end.
 
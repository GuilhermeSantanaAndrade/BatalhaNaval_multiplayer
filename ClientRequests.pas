unit ClientRequests;

interface

uses geral, uSockets, SysUtils, AppStructs, StdCtrls, Forms;

type
  TClientRequest = class
  public
    class function CheckGame(out GameState:TGameState):Boolean;
    class function StartNewGame(var sktReceive:TSocketData):Boolean;
    class function JoinGame(var sktReceive:TSocketData):Boolean;
  end;

var
  Client:TTCPClientConnection;
  
implementation

class function TClientRequest.CheckGame(out GameState:TGameState):Boolean;
var
  sktData, sktReceive:TSocketData;
begin
    Result := False;
    sktData.Command := _request_CheckGame;
    sktData.CodTransaction := FormatDateTime('hhnnssmmzz',Now());
    Client.WriteBuffer(sktData, SizeOf(sktData));
    If Client.Thread.WaitTransaction(sktData.CodTransaction, sktReceive) Then
    Begin
       GameState := sktReceive.GameState;
       Result := True; 
    end else
       GameState := gsNone;
end;

class function TClientRequest.JoinGame(var sktReceive:TSocketData):Boolean;
var
  sktData:TSocketData;
begin
    Result := False;
    sktData.Command  := _request_JoinGame;
    sktData.CodTransaction := FormatDateTime('hhnnssmmzz',Now());
    Client.WriteBuffer(sktData, SizeOf(sktData));
    Try
      If Client.Thread.WaitTransaction(sktData.CodTransaction, sktReceive) Then
      Begin
         If sktReceive.Command = _response_OK then
           Result := True
         else
         if sktReceive.Command = _response_Already then
           sktReceive.ErrorMsg := 'Erro. Você já está neste jogo.'
         else
         if sktReceive.Command = _response_Err then
           sktReceive.ErrorMsg := sktReceive.ErrorMsg;
      end;
    Except
      On E : ENoResponse Do
      Begin
         Result := False;
         sktReceive.ErrorMsg := 'ENoResponse: '+ e.Message;
      end;
      On E : Exception Do
      Begin
         Result := False;
         sktReceive.ErrorMsg := 'Exception: '+ e.Message;
      end;
    End;
end;

class function TClientRequest.StartNewGame(var sktReceive:TSocketData):Boolean;
var
  sktData:TSocketData;
begin
    Result := False;
    sktData.Command := _request_StartNewGame;
    sktData.CodTransaction := FormatDateTime('hhnnssmmzz',Now());
    Client.WriteBuffer(sktData, SizeOf(sktData));
    Try
      If Client.Thread.WaitTransaction(sktData.CodTransaction, sktReceive) Then
      Begin
         If sktReceive.Command = _response_OK then
           Result := True
         else
         if sktReceive.Command = _response_Already then
           sktReceive.ErrorMsg := 'Alguém iniciou um jogo, tente novamente.';
      end;
    Except
      On E : ENoResponse Do
      Begin
         Result := False;
         sktReceive.ErrorMsg := 'ENoResponse: '+ e.Message;
      end;
      On E : Exception Do
      Begin
         Result := False;
         sktReceive.ErrorMsg := 'Exception: '+ e.Message;
      end;
    End;
end;

end.

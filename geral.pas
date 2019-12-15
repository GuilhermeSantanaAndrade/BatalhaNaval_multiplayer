unit geral;

interface

uses SysUtils, Classes, Scanner, uSockets, AppStructs;

Const
  sTrue = '1';
  sFalse= '0';

  //****** Quatro posições numéricas *********
  //**  Pos 1: 0 = Não disparado            **
  //**         1 = Disparado                **
  //**  Pos 2: Index Barco (Zero para vazio)**
  //**  Pos 3: Index Part                   **
  //**  Pos 4: H = Horizontal               **
  //**         V - Vertical                 **
  //******************************************

  _request_AlterName     = 'ALTERNAME';
  _request_CheckGame     = 'CHECKGAME';
  _request_StartNewGame  = 'STARTNEWGAME';
  _request_JoinGame      = 'JOINGAME';
  _request_NewPlayer     = 'NEWPLAYER';
  _request_GameStart     = 'GAMESTART';

  _response_OK  = 'OK';
  _response_ERR = 'ERR';
  _response_ALREADY = 'ALREADY';

Type
  TShip = class(TObject)
    private
      fOrientation:TOrientation;
      fIniPos:TPos;
      fPoints:Integer;
      fIndex :Integer; // Cada barco preenche o seu Index no Create
      QtdParts:Integer;
    public
      property Orientation:TOrientation read fOrientation write fOrientation;
      property IniPos :TPos      read fIniPos  write fIniPos;
      property Points :Integer   read fPoints  write fPoints;
      property Index  :Integer   read fIndex   write fIndex;
      constructor GetInstance; virtual; abstract;
  end;

  TCouracado = class(TShip) //Index: 1
    private
      Parts:Array[0..4] of Boolean; // <- False = Damaged Part

    public
      constructor Create(var MatrizPai:TMemMatriz ;IniPos:TPos ; Orientation:TOrientation); overload;
      constructor GetInstance; override;
  end;

  TSubmarino = class(TShip) //Index: 2
    private
      Parts:Array[0..2] of Boolean;
    public
      constructor Create(var MatrizPai:TMemMatriz ; IniPos:TPos ; Orientation:TOrientation);
      constructor GetInstance; override;
  end;

  TPortaAvioes = class(TShip) //Index: 3
    private
      Parts:Array[0..2] of Boolean;
    public
      constructor Create(var MatrizPai:TMemMatriz ;IniPos:TPos ; Orientation:TOrientation);
      constructor GetInstance;
  end;

  TTorpedeiro = class(TShip) //Index: 4
    private
      Parts:Array[0..1] of Boolean;
    public
      constructor Create(var MatrizPai:TMemMatriz ;IniPos:TPos ; Orientation:TOrientation);
      constructor GetInstance; override;
  end;

  TShootResult = record
     ShipIdx:String[1];
     IdxPart:String[1];
     Orientation:String[1];
  end;

  procedure RandomAlocate(ListaNavios:TStringList ; MatrizPai:TMemMatriz ; out lstPOS:TStringList);
  function  IsValidPosition(Ship:TShip ; var MatrizPai:TMemMatriz ; IniPos:TPos ; Orientation:TOrientation):Boolean;
  function  PopulateMemMatriz(Ship:TShip ; var MatrizPai:TMemMatriz ; IniPos:TPos ; Orientation:TOrientation):Boolean;
  function  ConvertCharToIndex(const S:String):Integer;
  function  GetPrefixChr(S:String):String;
  function  GetSufixNumber(S:String):String;
  function  ShootOnMemMatriz(MatrizPai:TMemMatriz ; Pos:TPos ; var ShootResult:TShootResult):Boolean;
  function IsWrongIP(IP: string): Boolean;
  Function IsSomenteNumeros(str: string): Boolean;
  function LeadingZeroIPNumber (sIP : string) : string;
  function JogadorGetInstace(nome, ip:String):PJogador;
  function RemoveEOL(Str:String):String;
  function SplitArrayIntoString(AArray:TMemMatriz ; SocketData:TSocketData):String;
  function StringToArray(SInput:String):TMemMatriz;
  function IIF(AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant;

var
  TurnoAtualClient:TTurn;
  TurnoAtualServer:TTeamTurn;
  PontuacaoTeamA, PontuacaoTeamB:Integer;

implementation

uses Math;

{ TCouracado }

constructor TCouracado.Create(var MatrizPai:TMemMatriz ; IniPos: TPos; Orientation: TOrientation);
begin
    fIndex  := 1;
    fPoints := 50;
    QtdParts := High(Parts)+1;
    If not PopulateMemMatriz(Self, MatrizPai, IniPos, Orientation) then
       EInvalidShipPosition.Create('Posição inválida para alocar um Návio Couraçado');
end;

constructor TCouracado.GetInstance;
begin
    fIndex := 1;
    QtdParts := High(Parts)+1;
end;

{ TSubmarino }

constructor TSubmarino.Create(var MatrizPai:TMemMatriz ; IniPos: TPos; Orientation: TOrientation);
begin
    fIndex := 2;
    fPoints := 35;
    QtdParts := High(Parts)+1;
    If not PopulateMemMatriz(Self, MatrizPai, IniPos, Orientation) then
       EInvalidShipPosition.Create('Posição inválida para alocar um Submarino');
end;

constructor TSubmarino.GetInstance;
begin
    fIndex := 2;
    QtdParts := High(Parts)+1;
end;

{ TPortaAvioes }

constructor TPortaAvioes.Create(var MatrizPai:TMemMatriz ; IniPos: TPos; Orientation: TOrientation);
begin
    fIndex := 3;
    fPoints := 20;
    QtdParts := High(Parts)+1;
    If not PopulateMemMatriz(Self, MatrizPai, IniPos, Orientation) then
       EInvalidShipPosition.Create('Posição inválida para alocar um Návio Porta Aviões');
end;

constructor TPortaAvioes.GetInstance;
begin
    fIndex := 3;
    QtdParts := High(Parts)+1;
end;

{ TTorpedeiro }

constructor TTorpedeiro.Create(var MatrizPai:TMemMatriz ; IniPos: TPos;Orientation: TOrientation);
begin
    fIndex := 4;
    fPoints := 15;    
    QtdParts := High(Parts)+1;
    If not PopulateMemMatriz(Self, MatrizPai, IniPos, Orientation) then
       EInvalidShipPosition.Create('Posição inválida para alocar um Návio Torpedeiro');
end;

constructor TTorpedeiro.GetInstance;
begin
    fIndex := 4;
    QtdParts := High(Parts)+1;
end;

{ TShip }

function IsValidPosition(Ship:TShip ; var MatrizPai: TMemMatriz; IniPos: TPos; Orientation: TOrientation): Boolean;
Var
  Pos:Integer;
  i, i2, QtRows, QtCols:Integer;
begin
    Result := True;
    QtCols := High(MatrizPai);
    QtRows := High(MatrizPai[0]);

    If Orientation = otVertical Then
    begin
       i  := IniPos.y;
       i2 := IniPos.y;
    end else
    begin
       i  := IniPos.x;
       i2 := IniPos.x;
    end;

    For Pos := i To i2+Pred(Ship.QtdParts) Do
    Begin
        // Verifica se o barco sairá do tabuleiro
        If ((Orientation = otVertical)   And ((IniPos.y+(Pos-i2)) > QtRows)) Or
           ((Orientation = otHorizontal) And ((IniPos.x+(Pos-i2)) > QtCols)) Then
        Begin
           Result := False;
           Break;
        end;

        // Verifica se existe outro barco no caminho que impeça de posicionar
        If ((Orientation = otVertical)   And (Copy(MatrizPai[IniPos.x]  [IniPos.y+(Pos-i2)],_PosIndexShip, 1) <> sFalse)) Or
           ((Orientation = otHorizontal) And (Copy(MatrizPai[IniPos.x+(Pos-i2)][IniPos.y]  ,_PosIndexShip, 1) <> sFalse)) Then
        begin
           Result := False;
           Break;
        end;
    end;
end;

function PopulateMemMatriz(Ship:TShip ; var MatrizPai: TMemMatriz; IniPos: TPos; Orientation: TOrientation): Boolean;
var
  i, i2, Pos:Integer;
begin
    Result := False;
    If (Ship <> nil) And IsValidPosition(Ship, MatrizPai, IniPos, Orientation) Then
    Begin
        If Orientation = otVertical Then
        begin
           i  := IniPos.y;
           i2 := IniPos.y;
        end else
        begin
           i  := IniPos.x;
           i2 := IniPos.x;
        end;

        For Pos := i To i2+Pred(Ship.QtdParts) Do
        Begin
            If (Orientation = otVertical) Then
            begin
                MatrizPai[IniPos.x][IniPos.y+(Pos-i2)] := Copy(MatrizPai[IniPos.x][IniPos.y+(Pos-i2)],1,1) + IntToStr(Ship.Index) + IntToStr((Pos-i2) + 1) + 'V';
            end else
            If (Orientation = otHorizontal) then
            begin
                MatrizPai[IniPos.x+(Pos-i2)][IniPos.y] := Copy(MatrizPai[IniPos.x+(Pos-i2)][IniPos.y],1,1) + IntToStr(Ship.Index) + IntToStr((Pos-i2) + 1) + 'H';
            end;
        end;
        Result := True
    end;
end;

procedure RandomAlocate(ListaNavios:TStringList ; MatrizPai:TMemMatriz; out lstPOS:TStringList);
Const
  _Tentativas = 200;
var
  n, Cnt, rd:Integer;
  d:double;
  IniPos:TPos;
  Orientation:TOrientation;
  Ship:TShip;
begin
    If Assigned(lstPos) Then
       lstPos.Free;
    lstPos := TStringList.Create();

    For n:=0 To Pred(ListaNavios.Count) Do
    Begin
        Ship := TShip(ListaNavios.Objects[n]);
        Cnt := 1;
        While True Do // Tenta 200 vezes, se não conseguir retornar Timeout
        Begin
            d := Random;
            Randomize;
            If d <= 0.5 Then
            begin
               Orientation := otHorizontal;
               rd := Random(High(MatrizPai)+1-(Ship.QtdParts-1));
               If rd = 0 Then
                  Inc(rd);
               Sleep(3);
               Randomize;
               IniPos.x  := rd;
               rd := Random(High(MatrizPai[0])+1);
               If rd = 0 Then
                  Inc(rd);
               Randomize;
               IniPos.y  := rd;
            end else
            begin
               Orientation := otVertical;
               rd := Random(High(MatrizPai)+1);
               If rd = 0 Then
                  Inc(rd);
               Sleep(3);
               Randomize;
               IniPos.x  := rd;
               rd := Random(High(MatrizPai[0])+1-(Ship.QtdParts-1));
               If rd = 0 Then
                  Inc(rd);
               Randomize;
               IniPos.y  := rd;
            end;

            If PopulateMemMatriz(Ship, MatrizPai, IniPos, Orientation) then
            begin
               lstPOS.Add('X:'+ IntToStr(IniPos.x) + ',' + 'Y:'+ IntToStr(IniPos.y) + ',' + IntToStr(Ship.Index) +','+ IIF(Orientation=otVertical,'V','H'));
               Break;
            end;

            If Cnt > 200 then
               raise Exception.Create('Time out at RandomLocate')
            else
               Inc(Cnt);
        end;
    end;
end;

function ConvertCharToIndex(const S:String):Integer;
var
  n, n2, Idx:Integer;
  Capt:String;
begin
   n      := 65;
   Result := -1;
   Idx    := 0;
   While True Do
   Begin
      n2 := 0;
      Inc(Idx);
      
      Case n Of
        91..116:
        begin
           Capt   := 'A';
           n2 := n - 90;
        end;
        117..142:
        begin
           Capt   := 'B';
           n2 := n - 116;
        end;
        143..168:
        begin
           Capt   := 'C';
           n2 := n - 142;
        end;
        169..194:
        begin
           Capt   := 'D';
           n2 := n - 168;
        end;
        195:
        begin
           Result := -1;
           Break;
        end;
      else
        Capt := Chr(n);
      end;

      If n2 <> 0 Then
           Capt := Capt + Chr(64+n2);

      If UpperCase(Capt) = UpperCase(S) Then
      begin
         Result := Idx;
         Break;
      end;
      Inc(n);
   End;
end;

function  GetPrefixChr(S:String):String;
var
  X:Integer;
begin
    Result := '';
    For x:= 1 To Length(S) Do
    Begin
        If S[x] in ['a'..'z', 'A'..'Z'] then
           result := result + S[x]
        else
           Break;
    end;
end;

function  GetSufixNumber(S:String):String;
var
  X:Integer;
begin
    Result := '';
    For x:= 1 To Length(S) Do
    Begin
        If S[x] in ['0'..'9'] then
           result := result + S[x];
    end;
end;

function ShootOnMemMatriz(MatrizPai:TMemMatriz ; Pos:TPos ; var ShootResult:TShootResult):Boolean;
begin
   If (((Pos.x) < 1) or ((Pos.x) > High(MatrizPai))) or
      (((Pos.y) < 1) or ((Pos.y) > High(MatrizPai[0]))) Then
      Result := False
   else
   begin
       If Copy(MatrizPai[Pos.x][Pos.y], _PosDisparo, 1) = sFalse Then
       begin
          MatrizPai[Pos.x][Pos.y] := sTrue + Copy(MatrizPai[Pos.x][Pos.y], _PosDisparo+1, MaxInt);
          Result := True;
       end;

       If Copy(MatrizPai[Pos.x][Pos.y], _PosIndexShip, 1) <> sFalse Then
       begin
          ShootResult.ShipIdx := Copy(MatrizPai[Pos.x][Pos.y], _PosIndexShip, 1);
          ShootResult.IdxPart := Copy(MatrizPai[Pos.x][Pos.y], _PosIndexPart, 1);
          ShootResult.IdxPart := Copy(MatrizPai[Pos.x][Pos.y], _PosOrientation, 1);
       end else
       begin
          ShootResult.ShipIdx := '';
          ShootResult.IdxPart := '';
          ShootResult.IdxPart := '';
       end;
   end;
end;

function IsWrongIP(IP: string): Boolean;
var
  i   : byte;
  st  : array[1..4] of Integer;
  sIP : String;
begin
    st[1] := 0;
    st[2] := 0;
    st[3] := 0;
    st[4] := 0;

    Result := False;
    sIP    := IP;
    i      := 0;

    // Conta qtos Pontos
    While Pos( '.' , sIP ) > 0 do
    Begin
        Inc(i);
        sIP := Copy(sIP, Pos( '.' , sIP )+1, Length(sIP) );
    End;
    If i <> 3 then
    begin
       IsWrongIP := True;
       Exit;
    end;

    sIP := StringReplace(IP,'.','',[rfReplaceAll]);

    if not IsSomenteNumeros(sIP) Then
    Begin
       IsWrongIP := True;
       Exit;
    end;

    sIP := LeadingZeroIPNumber(IP);

    if Length(sIP) <> 15 then
    Begin
       IsWrongIP := True;
       Exit
    end;

    st[1] := StrToInt(Copy(sIP,1, 3));
    st[2] := StrToInt(Copy(sIP,5, 3));
    st[3] := StrToInt(Copy(sIP,9, 3));
    st[4] := StrToInt(Copy(sIP,13,3));

    if (( st[1] < 1 ) And ( st[1] > 255 )) or
       (( st[2] > 255 )) or
       (( st[3] > 255 )) or
       (( st[4] < 1 ) And ( st[4] > 255 )) Then
    Begin
       IsWrongIP := True;
       Exit
    end;
end;

Function IsSomenteNumeros(str: string): Boolean;
var
  i : integer; // contador para for
begin
  result := (str <> '');

  For i := 1 to length(str) do
    if not (str[i] in ['0'..'9']) then
       result := False;
end;

function LeadingZeroIPNumber (sIP : string) : string;
var
    i : integer;
    cnt : integer;
begin
    Result := '';
    cnt := 1;
    for i := length(sIP) downto 1 do
    begin
      if sIP[i] = '.' then
      begin
        Result := copy('000', cnt, 3) + Result;
        cnt := 0;
      end;
      Result := sIP[i] + Result;
      cnt := cnt + 1;
    end;
    Result := copy('000', cnt, 3) + Result;
end;

function JogadorGetInstace(nome, ip:String):PJogador;
var
  Jogador:PJogador;
begin
    Jogador      := New(PJogador);
    Jogador.Name := nome;
    Jogador.IP   := ip;
    Result       := Jogador;
end;

function RemoveEOL(Str:String):String;
begin
    Result := StringReplace(StringReplace(Str,#13,'',[rfReplaceAll]),#10,'',[rfReplaceAll]);
end;

function SplitArrayIntoString(AArray:TMemMatriz ; SocketData:TSocketData):String;
var
  x, y:Integer;
  str:String;
begin
   For x:=0 To high(AArray) Do
   begin
       str := str + '(';
       For y:=0 To high(AArray[x]) Do
       Begin
           str := str + AArray[x][y];
           if y < high(AArray[x]) then
              str := str + ',';
       end;
       str := str + ')';
      
   end;
   if str<>'' then
     Result := str;
end;

function StringToArray(SInput:String):TMemMatriz;
var
  x, y, i, iniPOS, iLinha:Integer;
  Str, sRow:String;
  found:Boolean;
  scanner:TScanner;
begin
   Str := sInput;
   For iLinha:= 0 To _CountGrid+1 Do
   Begin
       i := 1;
       sRow := '';
       While (i <= Length(Str)) And (Str[i] <> '(') Do
         Inc(i);
       If Str[i]='(' Then
       Begin
           iniPOS := i;
           i := 1;
           While (i <= Length(Str)) And (Str[i] <> ')') Do
             Inc(i);
           If Str[i]=')' Then
           Begin
               sRow := Copy(Str, iniPOS+1, i-1);
               sRow := StringReplace(sRow, Chr(39), '', [rfReplaceAll]);
               Str  := Copy(Str, i+1, MaxInt); 
               scanner := TScanner.Create;
               Try
                 scanner.AnalyzeStr(sRow);
                 For i:=0 To Pred(scanner.Count) Do
                 Begin
                     If scanner.Token[i].Token = ttIdentifier then
                     begin
                         Result[iLinha][i] := scanner.TextI(i);
                     end;
                 end;
               finally
                 scanner.free;
               end;
           end;
       end;
   End;
end;

function IIF(AValue: Boolean; const ATrue: Variant; const AFalse: Variant): Variant;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

initialization
  PontuacaoTeamA := 0;
  PontuacaoTeamB := 0;

end.

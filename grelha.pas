unit grelha;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, geral, AppStructs;

type
  PLabel = ^TLabel;
  PImage = ^TImage;

  TfrmGrelha = class(TForm)
    ScrollBox1: TScrollBox;
    ship_3_3_H: TImage;
    ship_3_2_H: TImage;
    ship_3_1_H: TImage;
    ship_4_1_H: TImage;
    ship_4_2_H: TImage;
    ship_2_2_H: TImage;
    ship_2_1_H: TImage;
    ship_1_1_H: TImage;
    ship_1_2_H: TImage;
    ship_1_3_H: TImage;
    ship_2_3_H: TImage;
    ship_1_4_H: TImage;
    ship_1_5_H: TImage;
    ship_3_3_V: TImage;
    ship_3_2_V: TImage;
    ship_3_1_V: TImage;
    ship_4_1_V: TImage;
    ship_4_2_V: TImage;
    ship_2_2_V: TImage;
    ship_2_1_V: TImage;
    ship_1_1_V: TImage;
    ship_1_2_V: TImage;
    ship_1_3_V: TImage;
    ship_2_3_V: TImage;
    ship_1_4_V: TImage;
    ship_1_5_V: TImage;
    img_branco: TImage;
    img_explosao: TImage;
    img_agua: TImage;
    a1: TImage;
    fixedh_A: TLabel;
    fixedv_1: TLabel;
    img_cinza: TImage;
    pnlAguarde: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure MatrizClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    array_horiz_Lbls: Array of PLabel;
    array_vert_Lbls : Array of PLabel;
    matriz_grelha   : Array of Array of PImage;
    fIsEnemy:Boolean;
    fMemMatriz:TMemMatriz;
    fListTeam:TStringList;
    fFreeMode:Boolean;
    fGameStatus:TGameState;
    procedure CreateShootImg(Pos:TPos);
    procedure ReleaseShootImg(Pos:TPos);
    procedure ExibeMensagem(Capt:String);
    procedure StartMemMatriz;
    procedure ClearMemMatriz;
    procedure MensagemResize;
  public
    constructor Create(AOwner: TComponent ; IsEnemy:Boolean); overload;
    function  alocate(Ship:TShip ; IniPos:TPos ; Orientacao:TOrientation):boolean;
    function  Shoot(Pos:TPos ; var ShootResult:TShootResult):Boolean;
    procedure SetColor(AColor:TColor);
    procedure RefreshScreen; overload;
    procedure RefreshScreen(Pos:TPos); overload;
    procedure RefreshScreen(ListaPosicoes:TStringList); overload;
    function  GetMemMatriz:TMemMatriz;
    procedure AddJogador(Player:TJogador);
    property  IsEnemy:Boolean      read fIsEnemy write fIsEnemy;
    property  ListTeam:TStringList read fListTeam write fListTeam;
    property  FreeMode:Boolean     read fFreeMode write fFreeMode;
    property GameStatus:TGameState read fGameStatus write fGameStatus;
  end;

implementation

{$R *.dfm}

procedure TfrmGrelha.FormCreate(Sender: TObject);
var
  x,y:Integer;

  procedure CreateHorizontal(x1:Integer);
  var n,n2:Integer;
  begin
     array_horiz_Lbls[x1] := PLabel(TLabel.Create(Self));
     n2 := 0;

     With TLabel(array_horiz_Lbls[x1]) Do
     Begin
        AutoSize   := False;
        Width      := fixedh_A.Width;
        Height     := fixedh_A.Height;
        Top        := fixedh_A.Top;
        Left       := TLabel(array_horiz_Lbls[x1-1]).Left + TLabel(array_horiz_Lbls[x1-1]).Width + 2;
        Color      := fixedh_A.Color;
        Font.Color := fixedh_A.Font.Color;
        Font.Style := fixedh_A.Font.Style;
        n := 65+x1-1;

        Case n Of
          91..116:
          begin
             Caption   := 'A';
             n2 := n - 90;
          end;
          117..142:
          begin
             Caption   := 'B';
             n2 := n - 116;
          end;
          143..168:
          begin
             Caption   := 'C';
             n2 := n - 142;
          end;
          169..194:
          begin
             Caption   := 'D';
             n2 := n - 168;
          end;
        else
          Caption   := Chr(n);
        end;

        If n2 <> 0 Then
           Caption := Caption + Chr(64+n2);

        Name       := lowerCase('fixedh_'+ Caption);
        Alignment  := taCenter;
        Layout     := tlCenter;
        Visible    := True;
        Parent     := ScrollBox1;
     End;
  end;

  procedure CreateVertical(y1:Integer);
  begin
     array_vert_Lbls[y1] := PLabel(TLabel.Create(Self));

     With TLabel(array_vert_Lbls[y1]) Do
     Begin
        AutoSize   := False;
        Width      := fixedv_1.Width;
        Height     := fixedv_1.Height;
        Top        := TLabel(array_vert_Lbls[y1-1]).Top + TLabel(array_vert_Lbls[y1-1]).Height + 2;
        Left       := fixedv_1.Left;
        Color      := fixedv_1.Color;
        Font.Color := fixedv_1.Font.Color;
        Font.Style := fixedv_1.Font.Style;
        Caption    := IntToStr(y1);
        Name       := lowerCase('fixedv_'+ Caption);
        Alignment  := taCenter;
        Layout     := tlCenter;
        Visible    := True;
        Parent     := ScrollBox1;
     End;
  end;

  procedure CreateCell(x1, y1:Integer);
  begin
     matriz_grelha[x1][y1] := PImage(TImage.Create(Self));

     With TImage(matriz_grelha[x1][y1]) Do
     Begin
        AutoSize       := False;
        Width          := a1.Width;
        Height         := a1.Height;
        Top            := TLabel(array_vert_Lbls[y1]).Top;
        Left           := TLabel(array_horiz_Lbls[x1]).Left;
        Stretch        := True;
        Picture.Bitmap := a1.Picture.Bitmap;
        Name           := lowerCase(TLabel(array_horiz_Lbls[x1]).Caption + TLabel(array_vert_Lbls[y1]).Caption);
        Visible        := True;
        Parent         := ScrollBox1;
        OnClick        := a1.OnClick;
     End;
  end;
begin
   ScrollBox1.Align := alClient;
   If fIsEnemy Then
      a1.Picture.Bitmap := img_cinza.Picture.Bitmap
   else
      a1.Picture.Bitmap := img_branco.Picture.Bitmap;

   array_horiz_Lbls := nil;
   array_vert_Lbls  := nil;
   matriz_grelha    := nil;

   SetLength(array_horiz_Lbls,_CountGrid+1);
   SetLength(array_vert_Lbls,_CountGrid+1);
   SetLength(matriz_grelha,_CountGrid+1,_CountGrid+1);

   array_horiz_Lbls[0] := nil;
   array_horiz_Lbls[1] := PLabel(fixedh_A);
   array_vert_Lbls[0]  := nil;
   array_vert_Lbls[1]  := PLabel(fixedv_1);
   matriz_Grelha[1][1] := PImage(a1);

   For x:=2 To _CountGrid Do
       CreateHorizontal(x);
   For y:=2 To _CountGrid Do
       CreateVertical(y);
   For x:=1 To _CountGrid Do
   begin
       For y:=1 To _CountGrid Do
       Begin
          If (x=1) And (y=1) Then
             Continue;
          CreateCell(x,y);
       End;
   end;
end;

procedure TfrmGrelha.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmGrelha.FormDestroy(Sender: TObject);
var
  x,y:integer;
  Comp:TComponent;
  Pos:TPos;
begin
    For x:= 2 to _CountGrid Do
    Begin
        If array_horiz_Lbls[x] <> nil Then
        begin
           If Assigned(array_horiz_Lbls[x]) Then
              TLabel(array_horiz_Lbls[x]).Free;
        end;
       array_horiz_Lbls[x] := nil
    end;

    For y:= 2 to _CountGrid Do
    Begin
        If array_vert_Lbls[y] <> nil Then
        begin
           If Assigned(array_vert_Lbls[y]) Then
              TLabel(array_vert_Lbls[y]).Free;
        end;
        array_vert_Lbls[y] := nil
    end;

    For x:= 1 to _CountGrid Do
    Begin
        For y:= 1 to _CountGrid Do
        Begin
            If (x=1) and (y=1) then
            begin
               Comp := Self.FindComponent(TImage(matriz_grelha[x][y]).Name + 'disp');
               If Comp <> nil Then
               begin
                  If Comp <> nil Then
                     TImage(Comp).Free;
               end;
               continue;
            end;                                                 

            If matriz_grelha[x][y] <> nil Then
            begin
               Comp := Self.FindComponent(TImage(matriz_grelha[x][y]).Name + 'disp');
               If Comp <> nil Then
               begin
                 If Comp <> nil Then
                    TImage(Comp).Free;
               end;

               If Assigned(matriz_grelha[x][y]) Then
                  TLabel(matriz_grelha[x][y]).Free;
               matriz_grelha[x][y] := nil
            end;
        End;
    end;

    array_horiz_Lbls := nil;
    array_vert_Lbls  := nil;
end;

function TfrmGrelha.alocate(Ship: TShip; IniPos: TPos; Orientacao: TOrientation): boolean;
begin
    //FreeAndNil(Ship);

    if Ship is TCouracado then
      Ship := TCouracado.Create(Self.fMemMatriz, IniPos, Orientacao)
    else
    if Ship is TSubmarino then
      Ship := TSubmarino.Create(Self.fMemMatriz, IniPos, Orientacao)
    else
    if Ship is TPortaAvioes then
      Ship := TPortaAvioes.Create(Self.fMemMatriz, IniPos, Orientacao)
    else
    if Ship is TTorpedeiro then
      Ship := TTorpedeiro.Create(Self.fMemMatriz, IniPos, Orientacao)
    else
      raise Exception.Create('ClassType inválido.');
end;

procedure TfrmGrelha.SetColor(AColor: TColor);
begin
    Self.ScrollBox1.Color  := AColor;
end;

function TfrmGrelha.Shoot(Pos:TPos ; var ShootResult:TShootResult):Boolean;
begin
    Result := ShootOnMemMatriz(fMemMatriz, Pos, ShootResult);
end;

constructor TfrmGrelha.Create(AOwner: TComponent; IsEnemy: Boolean);
begin
    fIsEnemy := IsEnemy;
    Self.Create(AOwner);
    Self.DoubleBuffered := True;
    StartMemMatriz;
end;

procedure TfrmGrelha.CreateShootImg(Pos: TPos);
begin
    with TImage.Create(Self) do
    begin
        AutoSize       := False;
        Width          := img_explosao.Width;
        Height         := img_explosao.Height;
        Top            := TLabel(array_vert_Lbls[Pos.y]).Top +1;
        Left           := TLabel(array_horiz_Lbls[Pos.x]).Left +1;
        Stretch        := True;
        Picture.Bitmap := img_explosao.Picture.Bitmap;
        Name           := lowerCase(TLabel(array_horiz_Lbls[Pos.x]).Caption + TLabel(array_vert_Lbls[Pos.y]).Caption) +'disp';
        Visible        := True;
        Transparent    := True;
        Parent         := ScrollBox1;
        BringToFront;
    end;
end;

procedure TfrmGrelha.ReleaseShootImg(Pos: TPos);
Var
  Comp:TComponent;
begin
    Comp := Self.FindComponent(TImage(matriz_grelha[Pos.x][Pos.y]).Name + 'disp');
    If Comp <> nil Then
       TImage(Comp).Free;
end;

procedure TfrmGrelha.StartMemMatriz;
begin
    fMemMatriz := nil;
    SetLength(fMemMatriz,_CountGrid+1,_CountGrid+1);
    ClearMemMatriz;
end;

procedure TfrmGrelha.ClearMemMatriz;
var
  x, y:Integer;
begin
   For x:=0 To _CountGrid Do
   begin
       For y:=0 To _CountGrid Do
       Begin
          If (x=0) or (y=0) then
             fMemMatriz[x][y] := ''
          else
             fMemMatriz[x][y] := '000H';
       End;
   end;
end;

function TfrmGrelha.GetMemMatriz: TMemMatriz;
begin
    Result := fMemMatriz;
end;

procedure TfrmGrelha.MatrizClick(Sender: TObject);
var
  Pos:TPos;
  ShootResult:TShootResult;
  b:boolean;
begin
    If (fFreeMode) or
       ((GameStatus = gsInProgress) and (TurnoAtualClient in [trMyTurn]) And (Self.fIsEnemy)) Then
    Begin
      Pos.x := ConvertCharToIndex( GetPrefixChr(TImage(Sender).Name));
      Pos.y := StrToIntDef(GetSufixNumber(TImage(Sender).Name),-1);

      If (Pos.x < 1) or (Pos.y < 1) then
         exit;

      Self.Enabled       := False;
      Try
        b := Self.Shoot(Pos, ShootResult);
        if b then
        begin
           ExibeMensagem('Aguarde');
           Application.ProcessMessages;
           Sleep(500);
           RefreshScreen(Pos);
        end;
      Finally
        pnlAguarde.Visible := False;
        Application.ProcessMessages;
        Sleep(1000);
        Self.Enabled       := True;
      end;
    End else
    if GameStatus = gsWaitingPlayers then
    begin             
      Self.Enabled := False;
      Try
        ExibeMensagem('O guerra não começou!');
        Application.ProcessMessages;
        Sleep(1500);
      Finally
        pnlAguarde.Visible := False;
        Application.ProcessMessages;
        Sleep(1000);
        Self.Enabled       := True;
      end;
    end else
    begin
      If not (TurnoAtualClient in [trMyTurn]) and (Self.fIsEnemy) then
      begin
          Self.Enabled := False;
          Try
            ExibeMensagem('Não é o seu turno!');
            Application.ProcessMessages;
            Sleep(1500);
          Finally
            pnlAguarde.Visible := False;
            Application.ProcessMessages;
            Sleep(1000);
            Self.Enabled       := True;
          end;
      end;

      if (TurnoAtualClient in [trMyTurn]) and not (Self.fIsEnemy) then
      begin
          Self.Enabled := False;
          Try
            ExibeMensagem('Soldado diz: Comandante, não seria melhor atacar o inimigo?');
            Application.ProcessMessages;
            Sleep(4000);
          Finally
            pnlAguarde.Visible := False;
            Application.ProcessMessages;
            Sleep(1000);
            Self.Enabled  := True;
          end;
      end;    
    end;
end;

procedure TfrmGrelha.RefreshScreen;
var
  x:Integer;
  y:Integer;
  Pos:TPos;
begin
    For x:= 1 to _CountGrid Do
    Begin
        For y:= 1 to _CountGrid Do
        Begin
            Pos.x := x;
            Pos.y := y;
            RefreshScreen(Pos);
        End;
    end;
end;

procedure TfrmGrelha.RefreshScreen(Pos: TPos);
var
  Comp, Comp2:TComponent;
  Img:TImage;

  procedure ValidPosition;
  Begin
      If matriz_grelha[Pos.x][Pos.y] = nil Then
          raise EInvalidMemGridPosition.Create('Erro inexperado.'+#13+'X:'+IntToStr(Pos.x) +' Y:'+IntToStr(Pos.y) +' está nil.');

      If Length(fMemMatriz[Pos.x][Pos.y]) <> _MemArrayFields Then
      Begin
         raise EInvalidMemGridPosition.Create('Posição inválida na Matriz de memória(Length Assert) '+#13+'X:'+IntToStr(Pos.x) +' Y:'+IntToStr(Pos.y));
      end;

      Try
        If not((StrToIntDef(Copy(fMemMatriz[Pos.x][Pos.y],1,1),-1) in [0..9]) or
               (StrToIntDef(Copy(fMemMatriz[Pos.x][Pos.y],2,1),-1) in [0..9]) or
               (StrToIntDef(Copy(fMemMatriz[Pos.x][Pos.y],3,1),-1) in [0..9]) or
               ((Copy(fMemMatriz[Pos.x][Pos.y],4,1) = 'H') or (Copy(fMemMatriz[Pos.x][Pos.y],4,1) = 'V'))) Then
        Begin
           raise EInvalidMemGridPosition.Create('');
        end;
      except
        On E : Exception Do
        Begin
            raise EInvalidMemGridPosition.Create('Posição inválida na Matriz de memória.'+#13+'X:'+IntToStr(Pos.x) +' Y:'+IntToStr(Pos.y) +#13+ e.message);
        End;
      End;
  end;
begin
    Comp := Self.FindComponent(TImage(matriz_grelha[Pos.x][Pos.y]).Name + 'disp');

    If (Copy(fMemMatriz[Pos.x][Pos.y], _PosDisparo, 1) = sFalse) then
    begin
        If Comp <> nil Then
        begin
           If Comp <> nil Then
              TImage(Comp).Free;
        end;
    end else
    begin
        If (Copy(fMemMatriz[Pos.x][Pos.y], _PosIndexShip, 1) <> sFalse) then
        begin
            If Comp = nil Then
            begin
               CreateShootImg(Pos);
            end;
        End Else
        begin
            TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := img_agua.Picture.Bitmap;
            Exit;
        end;
    end;

    If (Copy(fMemMatriz[Pos.x][Pos.y], _PosIndexShip, 1) = sFalse) then
    begin
       If TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap <> img_agua.Picture.Bitmap Then
       Begin
           If fIsEnemy And (Comp = nil)Then
           begin
             If (TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap <> img_cinza.Picture.Bitmap) Then
                 TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := img_cinza.Picture.Bitmap;
           end else
           begin
             If (TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap <> img_branco.Picture.Bitmap) Then
                 TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := img_branco.Picture.Bitmap;
           end;
       end;
    end else
    begin
       If (not fIsEnemy) or
          (fIsEnemy And (Copy(fMemMatriz[Pos.x][Pos.y], _PosDisparo, 1) = sTrue)) Then
       begin
          Comp2 := Self.FindComponent( 'ship_' +
                                      Copy(fMemMatriz[Pos.x][Pos.y], _PosIndexShip,  1) + '_' +
                                      Copy(fMemMatriz[Pos.x][Pos.y], _PosIndexPart,  1) + '_' +
                                      Copy(fMemMatriz[Pos.x][Pos.y], _PosOrientation,1));
          If Comp2 <> nil Then
             Img := TImage(Comp2)
          else
          begin
             TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := nil;
             Exit;
          end;

          If (TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap <> img.Picture.Bitmap) Then
              TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := img.Picture.Bitmap;
       end else
       begin
          If (TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap <> img_cinza.Picture.Bitmap) Then
              TImage(matriz_grelha[Pos.x][Pos.y]).Picture.Bitmap := img_cinza.Picture.Bitmap;
       end;
    end;
end;

procedure TfrmGrelha.RefreshScreen(ListaPosicoes: TStringList);
begin
    
end;

procedure TfrmGrelha.FormResize(Sender: TObject);
begin
   MensagemResize;
end;



procedure TfrmGrelha.ExibeMensagem(Capt:String);
begin
    pnlAguarde.Caption := Capt;
    pnlAguarde.Width   := Canvas.TextWidth(pnlAguarde.Caption) + 40;
    MensagemResize;
    pnlAguarde.Visible := True;
end;

procedure TfrmGrelha.MensagemResize;
begin
   pnlAguarde.Left := (Self.Width div 2) - (pnlAguarde.Width div 2);
   pnlAguarde.top  := (Self.Height div 2) - (pnlAguarde.Height div 2);
end;

procedure TfrmGrelha.AddJogador(Player: TJogador);
begin

end;

end.

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, jpeg, config, regras, batalha;

type
  TfrmMain = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Image2: TImage;
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.SpeedButton4Click(Sender: TObject);
begin
    Application.Terminate;
end;

procedure TfrmMain.SpeedButton3Click(Sender: TObject);
begin
    TfrmConfig.Execute;
end;

procedure TfrmMain.SpeedButton2Click(Sender: TObject);
begin
    frmRegras.ShowModal;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
    Application.CreateForm(TfrmBatalha, frmBatalha);
    frmBatalha.Showmodal;
end;

end.

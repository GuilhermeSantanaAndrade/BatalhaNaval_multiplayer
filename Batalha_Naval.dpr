program Batalha_Naval;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  config in 'config.pas' {frmConfig},
  regras in 'regras.pas' {frmRegras},
  grelha in 'grelha.pas' {frmGrelha},
  batalha in 'batalha.pas' {frmBatalha},
  geral in 'geral.pas',
  ClientRequests in 'ClientRequests.pas',
  uList in 'uList.pas',
  uSockets in 'uSockets.pas',
  uThread in 'uThread.pas',
  Scanner in 'Scanner.pas',
  AppStructs in 'AppStructs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmRegras, frmRegras);
  Application.Run;
end.

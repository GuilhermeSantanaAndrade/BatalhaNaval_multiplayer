program Server;

uses
  Forms,
  MainServer in 'MainServer.pas' {frmServer},
  geral in '..\geral.pas',
  uList in '..\uList.pas',
  uSockets in '..\uSockets.pas',
  uThread in '..\uThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.Run;
end.
